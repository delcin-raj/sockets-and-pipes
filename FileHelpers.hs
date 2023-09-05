module FileHelpers where

import Relude
import qualified System.Directory as Dir
import Control.Monad.Trans.Resource (ResourceT, ReleaseKey, allocate)
import qualified System.IO as IO
import System.FilePath ((</>))

getDataDir :: IO FilePath
getDataDir = do
    dir <- Dir.getXdgDirectory Dir.XdgData "sockets-and-pipes"
    Dir.createDirectoryIfMissing True dir
    return dir


fileResource :: FilePath -> IO.IOMode -> ResourceT IO (ReleaseKey, IO.Handle)
fileResource path mode = do
    dir <- liftIO getDataDir
    allocate (IO.openFile (dir </> path) mode) IO.hClose