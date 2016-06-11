module OptParse.Types where


-- | Kept throughout the program
data Settings = Settings
    { setsCommand :: Command
    , setsDebug   :: Bool
    } deriving (Show, Eq)

-- | Command-line arguments
data Arguments = Arguments
    { argsDebug :: Bool
    } deriving (Show, Eq)

-- | Configuration, from config file
data Configuration = Configuration
    deriving (Show, Eq)


data Command
    = Analyze FilePath
    | AnalyzeEvaluatedness FilePath
    | DumpResults FilePath
    deriving (Show, Eq)
