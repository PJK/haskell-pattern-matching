module OptParse.Types where


-- | Kept throughout the program
data Settings = Settings
    { setsCommand    :: Command
    , setsTargetFile :: FilePath -- ^ File to analyze
    } deriving (Show, Eq)

-- | Command-line arguments
data Arguments = Arguments
    { argsTargetFile :: FilePath -- ^ File to analyze
    } deriving (Show, Eq)

-- | Configuration, from config file
data Configuration = Configuration
    deriving (Show, Eq)


data Command
    = Analyze
    | DumpResults
    deriving (Show, Eq)
