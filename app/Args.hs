module Args where

import Data.List.Split
import System.Console.GetOpt

data Flag = Verbose | Tags [String] | Output String | Help deriving (Show)

-- Converts the input string to a Value of Tags
convArgTags :: String -> Flag
convArgTags s = Tags $ splitOn "," s

convArgOutput :: Maybe String -> Flag
convArgOutput (Just s) = Output s
convArgOutput Nothing = Output ""

flags :: [OptDescr Flag]
flags = [ Option ['v'] ["verbose"] (NoArg Verbose)                "Verbose output."
        , Option ['t'] ["tags"]    (ReqArg convArgTags "TAGS")    "A comma-separated list of tags to look for."
        , Option ['o'] ["output"]  (OptArg convArgOutput "FILE")  "Outputs results to FILE."
        , Option []    ["help"]    (NoArg Help)                   "Prints help output and exits." ]

-- Parses the arguments given the command-line
parseArgs :: [String] ->  IO ([Flag], [String])
parseArgs xs = case getOpt RequireOrder flags xs of
                (fl, sl, []) -> return (fl, sl)
                (_, _, errl) -> ioError . userError $ concat errl ++ usageInfo header flags
                where header = "Usage: yrc [--help] [-v] [-o FILE] <-t tag1,tag2,tag3,...> BEGIN_ID"