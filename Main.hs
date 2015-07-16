import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Yaml
import System.Process
import System.Exit
import TextShow
import Data.Monoid ((<>))

data RenderGroup = RenderGroup {
    backend    :: Backend,
    images     :: Vector Text,
    renderJobs :: Vector RenderJob
} deriving (Show)

instance FromJSON RenderGroup where
    parseJSON (Object v) = RenderGroup <$>
        v .: "backend" <*>
        v .: "images" <*>
        v .: "render"

data Backend = Illustrator | Inkscape
    deriving (Show)

instance FromJSON Backend where
    parseJSON (String "illustrator") = return Illustrator
    parseJSON (String "inkscape")    = return Inkscape
    parseJSON _                      = fail "Backend must be one of: illustrator, inkscape"

data RenderJob = RenderJob {
    jobPath  :: Text,
    dpi      :: Maybe Float,
    srcDPI   :: Maybe Float,
    scaling  :: Maybe Float,
    prepend  :: Maybe Text
} deriving (Show)

instance FromJSON RenderJob where
    parseJSON (Object v) = RenderJob <$>
        v .:  "path" <*>
        v .:? "dpi" <*>
        v .:? "srcdpi" <*>
        v .:? "prepend" <*>
        v .:? "scaling"

illustrator :: Text -> RenderJob -> IO ()
illustrator input job =
    let srcdpi = fromMaybe 72 $ srcDPI job
        mScale = scaling job `mplus` (dpi job >>= \dp -> Just $ dp / srcdpi)
    in case mScale of
        Nothing -> print "What are you doing"
        Just scale -> do
            let cmd = "osascript"
                output = jobPath job <> input
                args = [ "illustrator-render"
                       , T.unpack input
                       , T.unpack output
                       , show scale
                       ]
            print $ "Rendering " <> input <> " to " <> output
            (ecode, out, err) <- readProcessWithExitCode cmd args ""
            case ecode of
                ExitSuccess -> return ()
                ExitFailure c -> print $ "Render of " ++ T.unpack output ++ "failed with code " ++ show c

inkscape :: FilePath -> RenderJob -> IO ()
inkscape input job = undefined

render :: Vector RenderGroup -> IO ()
render = mapM_ render'
    where
        render' (RenderGroup Illustrator inputs jobs) =
            forM_ inputs $ \input ->
                forM_ jobs $ \job ->
                    illustrator input job
        render' (RenderGroup Inkscape inputs jobs) = undefined

main :: IO ()
main = do
    renderGroups <- decodeFileEither "/home/matt/workspace/rn/images.yaml"
    case renderGroups of
      Left e -> print e
      Right r -> render r
