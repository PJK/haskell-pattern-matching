module OptParse where

import           Options.Applicative
import           System.Environment  (getArgs)

import           OptParse.Types

getSettings :: IO Settings
getSettings = do
    argv <- getArgs
    (command, args) <- parseArgs argv
    conf <- getConfig args
    case buildSettings command args conf of
        Left probl -> error $ "Could not build settings from arguments and config: " ++ probl
        Right sets -> return sets

buildSettings :: Command -> Arguments -> Configuration -> Either String Settings
buildSettings command args _ = Right Settings
    { setsCommand = command
    , setsDebug = argsDebug args
    }

defaultSettings :: Settings
defaultSettings = Settings { setsCommand = Analyze undefined, setsDebug = False }

getConfig :: Arguments -> IO Configuration
getConfig _ = return Configuration

parseArgs :: [String] -> IO (Command, Arguments)
parseArgs args = handleParseResult $ execParserPure prefs myParser args
  where
    prefs = defaultPrefs
      { prefMultiSuffix = "PATTERNS"
      , prefDisambiguate = True
      , prefShowHelpOnError = True
      , prefBacktrack = True
      , prefColumns = 80
      }


myParser :: ParserInfo (Command, Arguments)
myParser = info (helper <*> parser) help
  where
    parser = (,) <$> commandP <*> argumentsP
    help = fullDesc <> progDesc descr
    descr = unlines
        [ "Analyse pattern matching"
        , "by Pavel Kalvoda and Tom Sydney Kerckhove"
        ]

commandP :: Parser Command
commandP = hsubparser $ mconcat
    [ command "analyze"       parseAnalyze
    , command "evaluatedness" parseEvaluatedness
    , command "dump-results"  parseDumpResults
    ]

parseAnalyze :: ParserInfo Command
parseAnalyze = info parser modifier
  where
    parser = Analyze <$> strArgument (metavar "FILE" <> help "the file to analyze")
    modifier = fullDesc <> progDesc "Analyze and present recommendations"

parseEvaluatedness :: ParserInfo Command
parseEvaluatedness = info parser modifier
  where
    parser = AnalyzeEvaluatedness <$> strArgument (metavar "FILE" <> help "the file for which to generate evaluatedness")
    modifier = fullDesc <> progDesc "Present evaluatedness"

parseDumpResults :: ParserInfo Command
parseDumpResults = info parser modifier
  where
    parser = DumpResults <$> strArgument (metavar "FILE" <> help "the file for which to generate test results")
    modifier = fullDesc <> progDesc "Dump analysis results for testing"

argumentsP :: Parser Arguments
argumentsP
    = Arguments
    <$> switch
        ( short 'd'
        <> long "debug"
        <> help "turn on debug information")
