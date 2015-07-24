{-# LANGUAGE Arrows #-}

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
import Text.XML.HXT.Core
import qualified Control.Monad as M
import qualified Data.Text as T
import qualified Data.Text.Format as F
import qualified Data.Vector as V

data RenderGroup = RenderGroup {
    name       :: String,
    backend    :: Backend,
    ninePatch  :: Bool,
    images     :: Vector FilePath,
    renderJobs :: Vector RenderJob
} deriving (Show)

instance FromJSON RenderGroup where
    parseJSON (Object v) = RenderGroup <$>
        fmap T.unpack (v .: "name") <*>
        v .: "backend" <*>
        v .:? "9patch" .!= False <*>
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
        args = [ "illustrator-render"
               , input
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
    let (cmd, args) = backFun b i o s
    F.print "Rendering {} to {}\n" [i, o]
    F.print "Using cmd: {} args: {}\n" [cmd, show args]
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
            let (_, fileName) = splitFileName $ replaceExtension input ".png"
                prepended = maybe fileName (++ fileName) (prepend job)
                newFilename = fromMaybe (maybe prepended (prepended ++) (append job)) (rename job)
                absoluteOutput = absoluteJobPath </> newFilename
                scaling = case scale job of
                            Left scale -> scale
                            Right size -> size / dpi job * 100

            M.when np . M.void $ ninePatchPre b absoluteInput absoluteOutput scaling
            runBackend b absoluteInput absoluteOutput scaling

ninePatchPre :: Backend -> FilePath -> FilePath -> Float -> IO ()
ninePatchPre b i o s = M.void $ do
    let tmp = i <.> "tmp9" <.> "svg"
        tmpPng = i <.> "tmp9" <.> "png"
    copyFile i tmp
    runXmlArrow tmp tmp $ setVisibility Nothing False >>> setVisibility (Just "9patch") True
    runXmlArrow i i $ setVisibility (Just "9patch") False
    runBackend b tmp tmpPng s

ninePatchPost :: Backend -> FilePath -> FilePath -> Float -> IO ()
ninePatchPost b i o s = do
    let inPng = replaceExtension i ".png"
        tmpPng = i <.> "tmp9" <.> "png"
    res <- readPng tmpPng
    rem <- readPng inPng
    case res of
        Left str -> print "ayy lmao"
        Right dimg -> print "ayyy"

ink = "http://www.inkscape.org/namespaces/inkscape"
svg = "http://www.w3.org/2000/svg"

runXmlArrow :: String -> String -> IOSArrow XmlTree XmlTree -> IO [Int]
runXmlArrow src dst arrow = runX $
    readDocument [] src >>>
    propagateNamespaces >>>
    processChildren (arrow `when` isElem) >>>
    writeDocument [withIndent yes] dst >>> getErrStatus

setVisibility :: Maybe String -> Bool -> IOSArrow XmlTree XmlTree
setVisibility mname vis = processChildren $ processGroups mname vis `when` hasQName (mkQName "" "g" svg) `orElse` this

processGroups :: Maybe String -> Bool -> IOSArrow XmlTree XmlTree
processGroups name vis = proc value -> do
    matches <- if isJust name
                    then hasQAttrValue (mkQName "" "label" ink) (== fromJust name) -< value
                    else this -< value
    hidden <- if vis
                then addAttr "display" "" >>> addAttr "style" "" -< matches
                else addAttr "display" "none" >>> addAttr "style" "display:none" -< matches
    returnA -< hidden

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

getImage :: DynamicImage -> IO ()
getImage (ImageRGBA8 i) = writePng "ayylmao.png" $ createImage i
getImage (ImageYA8 i) = writePng "ayylmao.png" . createImage $ promoteImage i
getImage _ = print "ayy lmao 2"

isOpaque :: PixelRGBA8 -> Bool
isOpaque (PixelRGBA8 _ _ _ a) = a == 255

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
        Right r -> V.mapM_ render (V.filter (maybe (const True) (==) renderTarget . name) r)
