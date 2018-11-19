module Args where

import Data.List.Split
import System.Console.GetOpt

data Flag = Verbose | Tags [String] | Output String | Help deriving (Show)

-- Converts the input string to a Value of Tags
convArgTags :: String -> Flag
convArgTags s = Tags $ splitOn "," s

flags = [
    Option ["v"] ["verbose"] NoArg Verbose                  "Verbose output."
    Option ["t"] ["tags"]    (ReqArg convArgTags "TAGS")    "A comma-separated list of tags to look for."
    Option ["o"] ["output"]  (OptArg Output "FILE")         "Outputs results to FILE."
    Option []    ["help"]    NoArg Help                     "Prints help output and exits."
]

-- Parses the arguments given the command-line
parseArgs :: [String] ->  IO ([Flag], [String])
parseArgs = case getOpt RequireOrder flags of
                (fl, sl, _) -> return (fl, sl)
                (_, _, err) -> ioError userError $ concat err ++ usageInfo "Usage: yrc [--help] [-v] [-o FILE] <-t tag1,tag2,tag3,...> BEGIN_ID" flags