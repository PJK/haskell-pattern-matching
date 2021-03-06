module TestUtils where

import           Control.Monad         (forM_)
import           System.Directory      (doesDirectoryExist, listDirectory)
import           System.FilePath.Posix (takeExtension, (</>))
import           Test.Hspec


-- List the source files in a given directory
sourceFiles :: FilePath -> IO [FilePath]
sourceFiles dir = do
    direxists <- doesDirectoryExist dir
    if direxists
    then map (\f -> dir </> f)
        <$> filter (\f -> takeExtension f == ".hs")
        <$> listDirectory dir
    else return []


-- | Sets up a test case for every sourcefile in the given dir path
forSourcesIn :: FilePath -> (FilePath -> IO ()) -> Spec
forSourcesIn dir = forSourcesInDirs [dir]

forSourcesInDirs :: [FilePath] -> (FilePath -> IO ()) -> Spec
forSourcesInDirs dirs func = forM_ dirs $ \dir -> do
    sfs <- runIO $ sourceFiles dir
    forM_ sfs $ \fp -> it fp $ func fp

