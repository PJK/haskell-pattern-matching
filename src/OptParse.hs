module OptParse where

import           Options.Applicative
import           System.Environment  (getArgs)

import           OptParse.Types

getSettings :: IO Settings
getSettings = do
    argv <- getArgs
    let (command, argss) = case argv of
            ("dump-results":as) -> (DumpResults, as)
            ("evaluatedness":as) -> (AnalyzeEvaluatedness, as)
            _ -> (Analyze, argv)
    args <- parseArgs argss
    conf <- getConfig args
    case buildSettings command args conf of
        Left probl -> error $ "Could not build settings from arguments and config: " ++ probl
        Right sets -> return sets

buildSettings :: Command -> Arguments -> Configuration -> Either String Settings
buildSettings command args _ = Right Settings
    { setsCommand = command
    , setsTargetFile = argsTargetFile args
    , setsDebug = argsDebug args
    }

defaultSettings :: Settings
defaultSettings = Settings
    { setsCommand = Analyze
    , setsTargetFile = undefined
    , setsDebug = False
    }

getConfig :: Arguments -> IO Configuration
getConfig _ = return Configuration

parseArgs :: [String] -> IO Arguments
parseArgs args = handleParseResult $ execParserPure defaultPrefs arguments args

arguments :: ParserInfo Arguments
arguments = info (helper <*> argumentsP)
    ( fullDesc
     <> progDesc "Analyse pattern matching"
     <> header "Here be a header" )

argumentsP :: Parser Arguments
argumentsP
    = Arguments
    <$> strArgument
        ( help "analysis target" )
    <*> switch
        ( short 'd'
        <> long "debug"
        <> help "turn on debug information")
