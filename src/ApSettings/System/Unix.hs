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

-- | Usual search path for unix configuration files
unixSearchRc :: FilePath -> FilePath -> [FilePath]
unixSearchRc dir nm = if null dir
                      then [
                        nm,
                        "~/." ++ nm,
                        "/etc/" ++ nm
                        ]
                      else [
                        nm,
                        "~/." ++ dir ++ "/" ++ nm,
                        "/etc/" ++ dir ++ "/" ++ nm
                        ]

