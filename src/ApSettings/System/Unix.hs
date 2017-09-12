module ApSettings.System.Unix (
  searchRc,
  suiteSearchRc
  ) where

import System.Posix.User

homeDir :: IO String
homeDir = homeDirectory <$> (getLoginName >>= getUserEntryForName)

searchRc :: String -> IO [FilePath]
searchRc programName = do
  let nm = programName
  home <- homeDir
  return [
    nm,
    home ++ "/." ++ nm,
    "/etc/" ++ nm
    ]

suiteSearchRc :: String -> String -> IO [FilePath]
suiteSearchRc suiteName programName = do
  let st = suiteName
      nm = programName
  home <- homeDir
  return [
    nm,
    home ++ "./" ++ st ++ "/" ++ nm,
    "/etc/" ++ st ++ "/" ++ nm
    ]
