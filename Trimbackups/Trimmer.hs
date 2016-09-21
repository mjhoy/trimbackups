module Trimbackups.Trimmer (filesToTrim) where

import System.Directory
import System.FilePath.Posix ((</>))
import Data.Time
import Data.Time.Clock.POSIX
import System.Posix.Files
import Control.Monad
import Data.List
import Data.Tuple.Extra

modificationTimeUTC :: FileStatus -> UTCTime
modificationTimeUTC = posixSecondsToUTCTime . modificationTimeHiRes

-- Return absolute paths to the contents of `dir`.
getAbsoluteDirectoryContents :: FilePath -> IO [FilePath]
getAbsoluteDirectoryContents dir = do
  files <- getDirectoryContents dir
  return $ map (\f -> dir </> f) files

-- Return absolute paths to files in `dir`.
getAbsoluteDirectoryFiles :: FilePath -> IO [FilePath]
getAbsoluteDirectoryFiles dir =
  getAbsoluteDirectoryContents dir >>= filterM doesFileExist

type FileMTime = (UTCTime, FilePath, FileStatus)

foldFn :: (Maybe UTCTime, UTCTime, [FileMTime]) -> FileMTime -> (Maybe UTCTime, UTCTime, [FileMTime])
foldFn (prevTime , now, acc) cur = (prevTime', now, acc')
  where
    curTime = fst3 cur
    keep = case prevTime of

      -- This is the most recent backup, of course we keep it.
      Nothing -> True

      -- Otherwise, compare with the previously kept backup.
      Just prevTimeJ

          -- after 2 months, weekly backups.
          | scope > (2 * month) -> diff > pad week

          -- after 2 weeks, daily backups.
          | scope > (2 * week) -> diff > pad day

          -- after a day, 6-hour backups.
          | scope > day -> diff > pad (6 * hour)

          -- otherwise, keep everything.
          | otherwise -> True

        where diff = diffUTCTime prevTimeJ curTime
              scope = diffUTCTime now prevTimeJ
              hour = 60 * 60
              day = hour * 24
              week = day * 7
              month = day * 30
              pad t = t - (60 * 10) -- 10 minute leeway

    (prevTime', acc') = if keep then (Just curTime, acc) else (prevTime, cur : acc)

pairFileWithMTime :: FilePath -> IO FileMTime
pairFileWithMTime path = do
  status <- getFileStatus path
  let mtime = modificationTimeUTC status
  return (mtime, path, status)

sortFstDesc :: [FileMTime] -> [FileMTime]
sortFstDesc = sortBy (\a b -> (fst3 b) `compare` (fst3 a))

filesToTrim :: UTCTime ->       -- current time
               FilePath ->      -- directory to look in
               IO [(FilePath, FileStatus)]    -- files we should delete
filesToTrim now backupDir = do
  filesAndTimes <- getAbsoluteDirectoryFiles backupDir >>=
                   mapM pairFileWithMTime >>=
                   return . sortFstDesc
  let removeFiles = foldl' foldFn (Nothing, now, []) filesAndTimes
  let files = map (\x -> (snd3 x, thd3 x)) (thd3 removeFiles)
  return files
