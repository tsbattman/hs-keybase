
module Network.Keybase.Proc (
    findKeybase
  , execKeybase
  ) where

import System.IO (hClose)

import System.Directory (findExecutable)
import System.Process (CreateProcess(..), StdStream(..), proc, withCreateProcess)
import qualified Data.ByteString.Lazy as LB

findKeybase :: IO (Maybe FilePath)
findKeybase = findExecutable "keybase"

execKeybase :: FilePath -> [String] -> LB.ByteString -> (LB.ByteString -> a) -> IO a
execKeybase keybase args lb f = withCreateProcess cp $ \(Just hin) (Just hout) _ _ -> do
  LB.hPutStr hin lb
  hClose hin
  r <- LB.hGetContents hout
  return $! LB.length r `seq` f r
  where
    cp = (proc keybase args) {
        std_in = CreatePipe
      , std_out = CreatePipe
      }
