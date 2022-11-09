{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Codegen where

import Relude
import Data.HashMap.Lazy qualified as HashMap
import Data.Text qualified as Text
import Language.Elm.Definition (Definition)
import Language.Elm.Name (Module)
import Language.Elm.Pretty qualified as Pretty
import Language.Elm.Simplification qualified as Simplification
import Language.Haskell.To.Elm (jsonDefinitions)
import Prettyprinter (Doc)
import Prettyprinter.Render.Text (hPutDoc)
import NavPoint (WaypointStyle, NavPoint)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, findExecutable, removeDirectoryRecursive)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath.Posix (takeDirectory)
import System.Process (readProcessWithExitCode)
import FlightTask (Turnpoint, TaskStart, TaskFinish, FlightTask (FlightTask))
import Entity (Entity(Entity))
import Geo (Latitude, Elevation, Direction, Longitude, Distance)
import Data.Time (DiffTime)
import ProgressPoint (ProgressPointDto)
import TaskProgress (TaskProgressDto)

typeDefinitions :: [Definition]
typeDefinitions =
    concat
        [ jsonDefinitions @WaypointStyle
        , jsonDefinitions @Latitude
        , jsonDefinitions @Longitude
        , jsonDefinitions @Elevation
        , jsonDefinitions @Direction
        , jsonDefinitions @Distance
        , jsonDefinitions @NavPoint
        , jsonDefinitions @Turnpoint
        , jsonDefinitions @TaskStart
        , jsonDefinitions @TaskFinish
        , jsonDefinitions @FlightTask
        , jsonDefinitions @Entity
        , jsonDefinitions @ProgressPointDto
        , jsonDefinitions @TaskProgressDto
        ]

elmSrcPath :: FilePath
elmSrcPath = "../client/src/"


writeContentsToFile :: forall ann. Module -> Doc ann -> IO ()
writeContentsToFile moduleName contents = do
  let path = elmSrcPath <> toString (Text.intercalate "/" moduleName) <> ".elm"
  -- Note: It's important to check and create missing directories individually, because there may several directories
  createDirectoryIfMissing True $ takeDirectory path
  putStrLn $ "Writing file: " <> path
  withFile path WriteMode (`hPutDoc` contents)

recursivelyDeleteDirectories :: [Text] -> IO ()
recursivelyDeleteDirectories subdirectories = do
  let directories = (elmSrcPath <>) . toString <$> subdirectories
  forM_ directories $ \d -> do
    doesExist <- doesDirectoryExist d
    when doesExist $ do
      putStrLn ""
      putStrLn $ "Recursively deleting a directory: " <> d
      removeDirectoryRecursive d

formatGeneratedCode :: [Text] -> IO ExitCode
formatGeneratedCode directories = do
  putStrLn ""
  mElmFormat <- findExecutable "elm-format"
  case mElmFormat of
    Just elmFormatPath -> do
      putStrLn $ "Found elm-format: " <> elmFormatPath
      putStrLn ""
      codes <- forM directories $ \dirName -> do
        let dir = elmSrcPath <> toString dirName
        putStrLn $ "Formatting .elm files in a directory (recursively): " <> dir
        (code, stdout', stderr') <- readProcessWithExitCode elmFormatPath [dir, "--yes"] ""
        unless (null stdout') $ putStrLn stdout'
        unless (null stderr') $ putStrLn stderr'
        pure code
      pure . fromMaybe ExitSuccess $ find (/= ExitSuccess) codes
    Nothing -> do
      putTextLn "elm-format was not found in PATH, leaving output unformatted."
      pure ExitSuccess

runCodegen :: IO ()
runCodegen = do
  -- Note: Since there isn't any restriction on writing the generated code to different directories,
  -- we can't say for sure which the previous directories were,
  -- therefore we explicitly specify a list of directories we want to be deleted recursively before writing new files.
  recursivelyDeleteDirectories ["Api"]
  putTextLn ""
  putTextLn "Generating Elm types encoders and decoders..."
  -- Combine endpoint definitions with type definitions and divide them into modules
  let modules = Pretty.modules $ Simplification.simplifyDefinition <$> typeDefinitions
  -- For each module write contents to file
  forM_ (HashMap.toList modules) $ uncurry writeContentsToFile
  formatGeneratedCode ["Api"] >>= exitWith