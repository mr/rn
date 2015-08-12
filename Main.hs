import Codec.Picture
import Codec.Picture.Types
import Control.Applicative
import Control.Monad (join)
import Data.Maybe
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import System.Console.ArgParser
import System.Directory
import System.Process
import System.Environment
import System.Exit
import System.FilePath
import System.FilePath.Glob
import qualified Control.Monad as M
import qualified Data.Text as T
import qualified Data.Text.Format as F
import qualified Data.Vector as V

import Paths_rn

import Debug.Trace

data RenderGroup = RenderGroup {
    name       :: String,
    backend    :: Backend,
    ninePatch  :: Maybe String,
    images     :: Vector FilePath,
    renderJobs :: Vector RenderJob
} deriving (Show)

instance FromJSON RenderGroup where
    parseJSON (Object v) = RenderGroup <$>
        fmap T.unpack (v .: "name") <*>
        v .: "backend" <*>
        v .:? "9patch" <*>
        fmap (fmap T.unpack) (v .: "images") <*>
        v .: "render"

data Backend = Illustrator | Inkscape | ImageMagick
    deriving (Show)

type BackendFunc = FilePath -> FilePath -> Float -> (String, [String])

instance FromJSON Backend where
    parseJSON (String "illustrator") = return Illustrator
    parseJSON (String "inkscape")    = return Inkscape
    parseJSON (String "imagemagick") = return ImageMagick
    parseJSON _                      = fail "Backend must be one of: illustrator, inkscape"

data RenderJob = RenderJob {
    jobPath :: FilePath,
    dpi     :: Float,
    scale   :: Either Float Float,
    prepend :: Maybe String,
    append  :: Maybe String,
    rename  :: Maybe String
} deriving (Show)

instance FromJSON RenderJob where
    parseJSON (Object v) = RenderJob <$>
        fmap T.unpack (v .:  "path") <*>
        v .:? "dpi" .!= 72 <*>
        scalingOrSize v <*>
        fmap (fmap T.unpack) (v .:? "prepend") <*>
        fmap (fmap T.unpack) (v .:? "append") <*>
        fmap (fmap T.unpack) (v .:? "rename")
        where
            scalingOrSize w = w .:? "scaling" >>= \case
                Just scaling -> return . Left $ scaling
                Nothing -> w .:? "size" >>= \case
                    Just size -> return . Right $ size
                    Nothing -> fail "Need 1 of either dpi or scaling"

illustrator :: BackendFunc
illustrator input output scaling =
    let cmd = "osascript"
        args = [ input
               , output
               , show scaling
               ]
   in (cmd, args)

imagemagick :: BackendFunc
imagemagick input output scaling =
    let cmd = "convert"
        args = [ input
               , "-resize"
               , show scaling ++ "%"
               , output
               ]
    in (cmd, args)

inkscape :: BackendFunc
inkscape input output scaling =
    let cmd = "inkscape"
        args = [ "-d"
               , show scaling
               , "-e"
               , output
               , input
               ]
    in (cmd, args)

backFun :: Backend -> BackendFunc
backFun Illustrator = illustrator
backFun Inkscape = inkscape
backFun ImageMagick = imagemagick

runBackend :: Backend -> FilePath -> FilePath -> Float -> IO ()
runBackend b i o s = do
    illustratorExe <- getDataFileName "illustrator-render"
    let (cmd, pargs) = backFun b i o s
        args = case b of
                   Illustrator -> illustratorExe : pargs
                   _ -> pargs
    F.print "Rendering with {} from {} to {}\n" [show b, i, o]
    (ecode, _, _) <- readProcessWithExitCode cmd args ""
    case ecode of
        ExitSuccess -> return ()
        ExitFailure c -> F.print "Render of {} failed with code {}\n" [o, show c]

render :: RenderGroup -> IO ()
render (RenderGroup n b np inputs jobs) = do
    globs <- join <$> V.mapM (fmap V.fromList . glob) inputs
    V.forM_ globs $ \input ->
        V.forM_ jobs $ \job -> do
            createDirectoryIfMissing True $ jobPath job

            absoluteInput <- makeAbsolute input
            absoluteJobPath <- makeAbsolute $ jobPath job
            let baseName = takeBaseName input
                prepended = maybe baseName (++ baseName) (prepend job)
                newFilename = fromMaybe (maybe prepended (prepended ++) (append job) <.> "png") (rename job)
                absoluteOutput = absoluteJobPath </> newFilename
                scaling = case scale job of
                            Left scale -> scale
                            Right size -> size / dpi job * 100

            runBackend b absoluteInput absoluteOutput scaling
            case np of
                Just nPath -> do
                    absoluteNPath <- makeAbsolute nPath
                    M.void $ renderNinePatch b absoluteNPath absoluteOutput scaling
                Nothing -> return ()

renderNinePatch :: Backend -> FilePath -> FilePath -> Float -> IO ()
renderNinePatch b i o s = do
    let ipath  = replaceExtension i ".png"
    runBackend b i ipath s
    eorig <- readPng o
    enew <- readPng ipath
    case eorig >>= \o -> enew >>= \n -> return (o, n) of
        Left str -> print str
        Right (origPng, ninePng) -> do
            createBorderImg ninePng ipath
            ninePatchify o origPng ninePng
            removeFile ipath

tracePixelAt :: Pixel a => Image a -> Int -> Int -> a
tracePixelAt i x y =
    let h = imageHeight i
        w = imageWidth i
    in if x <= 0 || y <= 0 || x >= w - 1 || y >= h - 1
        then trace ("YOOOO " ++ show x ++ " " ++ show y) (pixelAt i x y)
        else pixelAt i x y

ninePatchify :: FilePath -> DynamicImage -> DynamicImage -> IO ()
ninePatchify o (ImageRGBA8 orig) (ImageRGBA8 nine) = writePng o $ generateImage pixelRender w h
    where
        h = imageHeight nine
        w = imageWidth nine
        pixelRender x y = if x == 0 || y == 0 || x == w - 1 || y == h - 1
                                then if isOpaque (tracePixelAt nine x y) && not ((x == 0 && y == 0) || (x == w - 1 && y == h - 1))
                                        then PixelRGBA8 0 0 0 255
                                        else PixelRGBA8 255 255 255 0
                                else tracePixelAt orig x y

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

createBorderImg :: DynamicImage -> FilePath -> IO ()
createBorderImg (ImageRGBA8 i) output = writePng output $ createImage i
createBorderImg (ImageYA8 i) output = writePng output . createImage $ promoteImage i
createBorderImg _ _ = fail "What are you doing champ"

isOpaque :: PixelRGBA8 -> Bool
isOpaque (PixelRGBA8 _ _ _ a) = a > 0

onEdges :: Int -> Int -> Int -> Int -> Bool
onEdges x y h w = xor (y == 0) (x == 0) || xor (y == h - 1) (x == w - 1)

createImage :: Image PixelRGBA8 -> Image PixelRGBA8
createImage i = generateImage pixelRender (imageWidth i + 2) (imageHeight i + 2)
    where
        pixelRender x y =
            let h = imageHeight i
                w = imageWidth i
                x'  | x < 1      = 1
                    | x >= w     = w - 1
                    | otherwise  = x
                y'  | y < 1      = 1
                    | y >= h     = h - 1
                    | otherwise  = y
            in PixelRGBA8 0 0 0 $ if isOpaque (pixelAt i x' y') && onEdges x y (h + 2) (w + 2) then 255 else 0

data Opts = Opts String String

argParser :: ParserSpec Opts
argParser = Opts `parsedBy` optFlag "images.yaml" "input" `andBy` optFlag  "" "target"

main :: IO ()
main = withParseResult argParser $ \(Opts input target) -> do
    let renderTarget = if target == "" then Nothing else Just target
    renderGroups <- decodeFileEither input
    case renderGroups of
        Left e -> print e
        Right r -> V.mapM_ render $ V.filter (maybe (const True) (==) renderTarget . name) r
