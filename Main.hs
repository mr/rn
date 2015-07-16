import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Yaml
import System.Directory
import System.Process
import System.Environment
import System.Exit
import System.FilePath

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
    jobPath  :: FilePath,
    dpi      :: Maybe Float,
    srcDPI   :: Maybe Float,
    scaling  :: Maybe Float,
    prepend  :: Maybe String
} deriving (Show)

instance FromJSON RenderJob where
    parseJSON (Object v) = RenderJob <$>
        fmap T.unpack (v .:  "path") <*>
        v .:? "dpi" <*>
        v .:? "srcdpi" <*>
        v .:? "prepend" <*>
        fmap (fmap T.unpack) (v .:? "scaling")

illustrator :: FilePath -> RenderJob -> IO ()
illustrator input job =
    let srcdpi = fromMaybe 72 $ srcDPI job
        mScale = scaling job `mplus` (dpi job >>= \dp -> Just $ (dp / srcdpi) * 100)
    in case mScale of
        Nothing -> print "What are you doing"
        Just scale -> do
            let cmd = "osascript"
                (_, fileName) = splitFileName $ replaceExtension input ".png"
                output = jobPath job </> fileName
                args = [ "illustrator-render"
                       , input
                       , output
                       , show scale
                       ]
            print $ "Rendering " ++ input ++ " to " ++ output
            (ecode, out, err) <- readProcessWithExitCode cmd args ""
            case ecode of
                ExitSuccess -> return ()
                ExitFailure c -> print $ "Render of " ++ output ++ "failed with code " ++ show c

inkscape :: FilePath -> RenderJob -> IO ()
inkscape input job = undefined

render :: RenderGroup -> IO ()
render (RenderGroup b inputs jobs) =
    V.forM_ inputs $ \input ->
        V.forM_ jobs $ \job -> do
            createDirectoryIfMissing True $ jobPath job
            let backFun = case b of
                            Illustrator -> illustrator
                            Inkscape -> inkscape
            backFun input job

main :: IO ()
main = do
    args <- getArgs
    let defaultPath = "/home/matt/workspace/rn/images.yaml"
        yamlFile = fromMaybe defaultPath $ listToMaybe args
    renderGroups <- decodeFileEither yamlFile
    case renderGroups of
        Left e -> print e
        Right r -> V.mapM_ render r
