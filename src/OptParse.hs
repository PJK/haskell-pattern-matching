module OptParse (getSettings) where

import           Options.Applicative

import           OptParse.Types

getSettings :: IO Settings
getSettings = do
    args <- parseArgs
    conf <- getConfig args
    case buildSettings args conf of
        Left probl -> error $ "Could not build settings from arguments and config: " ++ probl
        Right sets -> return sets

buildSettings :: Arguments -> Configuration -> Either String Settings
buildSettings args _ = Right Settings
    { setsTargetFile = argsTargetFile args
    }

getConfig :: Arguments -> IO Configuration
getConfig _ = return Configuration

parseArgs :: IO Arguments
parseArgs = execParser arguments

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
