module Chunks where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.IO as IO
import System.FilePath ( (</>) ) 
import Prelude ()
import Relude

import Control.Monad.Trans.Resource (runResourceT)
import FileHelpers (getDataDir, fileResource)
import qualified Data.Char as C

-- T.hGetLine is unbounded

_repeatUntilIO :: IO chunk -> (chunk -> Bool) -> (chunk -> IO x) -> IO ()
_repeatUntilIO getChunk isEnd todo = continue
    where 
        continue = do
            chunk <- getChunk
            if isEnd chunk then
                return ()
            else do
                _ <- todo chunk
                continue

-- Exercises
digitsOnly :: Text -> Text
digitsOnly = T.filter C.isDigit

capitalizeLast :: Text -> Text
capitalizeLast t = T.append (T.dropEnd 1 t) (T.pack [C.toUpper (T.last t)])

unparen :: Text -> Maybe Text
unparen t
    | T.null t || T.null (T.drop 1 t) = Nothing
    | otherwise = let xs = T.dropEnd 1 t in
        if T.head t == '(' && T.last t == ')'
        then Just $  T.drop 1 xs
        else Nothing 

characterCount :: FilePath -> IO Int
characterCount fp = 
    runResourceT do
        (_, h) <- fileResource fp ReadMode
        liftIO $ count (T.hGetChunk h) 0
        where
            count :: IO Text -> Int -> IO Int
            count ioChunk acc = do
                chunk <- ioChunk  
                if T.null chunk then return acc
                else count ioChunk (T.length chunk + acc)

repeatUntilIO :: IO chunk -> (chunk -> Bool) -> (chunk -> IO x) -> IO ()
repeatUntilIO getChunk isEnd act = continue
    where 
        continue = do
            chunk <- getChunk 
            unless (isEnd chunk) (
                do
                    _ <- act chunk
                    continue
                )


printFileContentUpperCase :: IO ()
printFileContentUpperCase = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
    liftIO (printCapitalizedText h)

printCapitalizedText :: IO.Handle -> IO ()
printCapitalizedText h = repeatUntilIO (T.hGetChunk h) T.null (T.putStrLn . T.toUpper)
