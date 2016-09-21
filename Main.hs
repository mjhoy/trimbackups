module Main where

import Options.Applicative
import Control.Monad
import System.IO
import System.Exit
import System.Directory
import Data.Time
import System.Posix.Files
import Data.List

import Trimbackups.Trimmer

data Opts = Opts { optsPath :: String
                 , optsConfirm :: Bool
                 , optsDryRun  :: Bool }
            deriving (Show)

opts :: Parser Opts
opts = Opts
  <$> (strArgument
       (metavar "PATH"
     <> help "path to the backup directory"))
  <*> (switch
       (short 'y'
     <> help "Delete files without confirmation"))
  <*> (switch
       (long "dry-run"
     <> help "Print a summary of what would be deleted"))

prog :: ParserInfo Opts
prog = info (helper <*> opts)
         (fullDesc
       <> progDesc "trim backups in PATH"
       <> header "trimbackups -- prune backup files")

exitErr :: String -> IO ()
exitErr msg = do
  hPutStrLn stderr msg
  exitFailure

-- human readable file size
humanSize :: Real a => a -> [Char]
humanSize size = humanSize' (realToFrac size) ["Bytes", "Kb", "Mb", "Gb", "Tb"]
  where
    humanSize' x bases
        | x >= 1024 = humanSize' (x / 1024) (tail bases)
        | otherwise = (show x) ++ " " ++ (head bases)

main :: IO ()
main = do
  (Opts path confirm dry) <- execParser prog

  exists <- doesDirectoryExist path

  unless exists $ exitErr $ "path " ++ path ++ " is not a directory"

  now <- getCurrentTime

  trim <- filesToTrim now path

  case dry of

    True -> do
      putStrLn "dry run, not doing anything...\n"
      hFlush stdout

      putStrLn "files to trim:"
      forM_ trim $ \(totrim, _) -> do
        putStrLn totrim

      let sumSize = foldl' (\size (_, status) -> size + fileSize status) 0 trim
      putStrLn $ "\ntotal space to remove: " ++ (humanSize sumSize)

    False -> do

      let n = length trim
          w = if n == 1 then "file" else "files"

      when (n < 1) $ do
        exitSuccess

      unless confirm $ do
        putStrLn $ (show n) ++ " " ++ w ++ " will be removed. Continue? [y/n]"
        inp <- getChar
        when (inp /= 'y') $ do
          exitErr "User aborted"

      forM_ trim $ \(totrim, _) -> do
        putStrLn $ "rm " ++ totrim
