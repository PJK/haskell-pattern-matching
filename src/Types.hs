module Types where

import           DataDefs
import qualified Language.Haskell.Exts as H

data AnalysisAssigment
    = AnalysisAssigment
        FilePath
        H.Module
    deriving (Show, Eq)

data AnalysisResult
    = AnalysisError AnalysisError
    | AnalysisSuccess
    deriving (Show, Eq)

data AnalysisError
    = GatherError -- ^ Something went wrong while scraping the information
    deriving (Show, Eq)

data FunctionTarget
    = FunctionTarget H.SrcLoc Function
    deriving (Show, Eq)

data FunctionResult
    = FunctionResult
        FunctionTarget
    deriving (Show, Eq)

