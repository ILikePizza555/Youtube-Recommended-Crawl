module Args where

import Data.List.Split
import System.Console.GetOpt

data Flag = Verbose | Tags [String] | Output String | Help deriving (Show)

-- Converts the input string to a Value of Tags
convArgTags :: String -> Flag
convArgTags s = Tags $ splitOn "," s

flags :: [OptDescr Flag]
flags = [ Option ['v'] ["verbose"] (NoArg Verbose)                "Verbose output."
        , Option ['t'] ["tags"]    (ReqArg convArgTags "TAGS")    "A comma-separated list of tags to look for."
        , Option ['o'] ["output"]  (ReqArg Output "FILE")         "Outputs results to FILE."
        , Option []    ["help"]    (NoArg Help)                   "Prints help output and exits." ]

usageHeader :: String
usageHeader = "Usage: yrc [--help] [-v] [-o FILE] <-t tag1,tag2,tag3,...> BEGIN_ID"

usageStr :: String
usageStr = usageInfo usageHeader flags

-- Parses the arguments given a list of command-line strings
parseArgs :: [String] ->  IO ([Flag], String)
parseArgs xs = case getOpt RequireOrder flags xs of
                -- Variable flags, one parameter, no errors
                (fl, s:[], []) -> return (fl, s)
                -- too many parameters, no errors
                (_, xs, []) -> ioError . userError $ "Error: Too many parameters given. \n" ++ usageStr
                -- Errors given
                (_, _, errl) -> ioError . userError $ concat errl ++ usageStr