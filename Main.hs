import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import System.Directory
import System.Process
import System.Environment
import System.Exit
import System.FilePath
import System.FilePath.Glob
import qualified Data.Text as T
import qualified Data.Text.Format as F
import qualified Data.Vector as V

data RenderGroup = RenderGroup {
    backend    :: Backend,
    images     :: Vector FilePath,
    renderJobs :: Vector RenderJob
} deriving (Show)

instance FromJSON RenderGroup where
    parseJSON (Object v) = RenderGroup <$>
        v .: "backend" <*>
        fmap (fmap T.unpack) (v .: "images") <*>
        v .: "render"

data Backend = Illustrator | Inkscape
    deriving (Show)

instance FromJSON Backend where
    parseJSON (String "illustrator") = return Illustrator
    parseJSON (String "inkscape")    = return Inkscape
    parseJSON _                      = fail "Backend must be one of: illustrator, inkscape"

data RenderJob = RenderJob {
    jobPath :: FilePath,
    dpi     :: Float,
    scale   :: Either Float Float,
    prepend :: Maybe String
} deriving (Show)

instance FromJSON RenderJob where
    parseJSON (Object v) = RenderJob <$>
        fmap T.unpack (v .:  "path") <*>
        v .:? "dpi" .!= 72 <*>
        scalingOrSize v <*>
        fmap (fmap T.unpack) (v .:? "prepend")
        where
            scalingOrSize w = w .:? "scaling" >>= \case
                Just scaling -> return . Left $ scaling
                Nothing -> w .:? "size" >>= \case
                    Just size -> return . Right $ size
                    Nothing -> fail "Need 1 of either dpi or scaling"

illustrator :: FilePath -> RenderJob -> IO ()
illustrator input job = do
    let scaling = case scale job of
                    Left scale -> scale
                    Right size -> size / dpi job * 100
    absoluteInput <- makeAbsolute input
    absoluteJobPath <- makeAbsolute $ jobPath job
    let cmd = "osascript"
        (_, fileName) = splitFileName $ replaceExtension input ".png"
        absoluteOutput = absoluteJobPath </> maybe fileName (++ fileName) (prepend job)
        args = [ "illustrator-render"
               , absoluteInput
               , absoluteOutput
               , show scaling
               ]
    F.print "Rendering {} to {}" [absoluteInput, absoluteOutput]
    (ecode, out, err) <- readProcessWithExitCode cmd args ""
    case ecode of
        ExitSuccess -> return ()
        ExitFailure c -> F.print "Render of {} failed with code {}" [absoluteOutput, show c]

inkscape :: FilePath -> RenderJob -> IO ()
inkscape input job = undefined

render :: RenderGroup -> IO ()
render (RenderGroup b inputs jobs) = do
    globs <- join <$> V.mapM (fmap V.fromList . glob) inputs
    V.forM_ globs $ \input ->
        V.forM_ jobs $ \job -> do
            createDirectoryIfMissing True $ jobPath job
            let backFun = case b of
                            Illustrator -> illustrator
                            Inkscape -> inkscape
            backFun input job

main :: IO ()
main = do
    args <- getArgs
    let defaultPath = "images.yaml"
        yamlFile = fromMaybe defaultPath $ listToMaybe args
    renderGroups <- decodeFileEither yamlFile
    case renderGroups of
        Left e -> print e
        Right r -> V.mapM_ render r
