{-# LANGUAGE DeriveGeneric #-}

module DataDefs where

import           Data.Aeson            (FromJSON, ToJSON)
import           Data.List             (intercalate)
import qualified Data.Map              as Map
import           GHC.Generics          (Generic)
import           Language.Haskell.Exts hiding (DataOrNew (..), Name (..),
                                        Pretty, Type (..), prettyPrint)
import qualified Language.Haskell.Exts as H

-- Input:
--   - Context of datatypes (but not other functions)
--   - AST of function (but no need for RHS)
type Name = String

data DataType
    = DataType Name [TypeVariable] [Constructor]
  deriving (Show, Eq)

data TypeVariable
    = TypeVariable Name
  deriving (Show, Eq)

data Constructor
    = Constructor Name [Type]
  deriving (Show, Eq)

data Type
    = FunctionType Type Type -- ^ Function: a -> b
    | TupleType [Type] -- ^ Tuple: (a, b, ..., z)
        -- TODO Determine whether we really want to view this as a special case instead of as a constructor
        --   Answer(?): Yes, for better error messages.
    | ListType Type -- ^ List: [a]
    | TypeApplication Type Type -- ^ Application of type constructor: Tree a
    | VariableType Name -- ^ Type variable: a
    | TypeConstructor Name -- ^ Named constructor: Tree
  deriving (Show, Eq)


data Function
    = Function
      Name
      Type -- We'll only deal with explicitly typed functions, for now.
      [Clause]
  deriving (Show, Eq)

data Clause -- No need to include the right-hand side. We're only doing our analysis on the left part anyway.
    = Clause
      [Pattern] -- ^ The patterns to match for this clause, one for each argument.
  deriving (Show, Eq)

-- | TODO these should model constraints arising from guards.
type Constraint = String

data Guard
    = ConstraintGuard Constraint
    | PatternGuard Pattern Constraint
    | LetGuard Pattern Constraint

data Pattern
    = VariablePattern Name -- ^ Variable: a
    | LiteralPattern Sign Literal -- ^ Literal: -5
    | ConstructorPattern Name [Pattern] -- ^ Data constructor: Node Leaf Leaf
    | TuplePattern [Pattern] -- ^ Tuple: (a, b, ..., z)
    | ListPattern [Pattern] -- ^ List: [a, b, ..., z]
    | WildcardPattern
    | PlaceHolderPattern -- ^ Represents Pattern parameter that should be substituted
    | GuardPattern Pattern Constraint
  deriving (Show, Eq, Generic, Ord)

-- We need to annotate all Patterns with the type they represent. Use this for guards
guardType :: Name
guardType = "__anonymous_guard_type"

-- TODO should we introduce new type for these? seems laborious
-- -- | Patterns that can be processed without desugaring
-- data RestrictedPattern
--     = VariablePattern Name -- ^ Variable: a
--     | LiteralPattern Sign Literal -- ^ Literal: -5
--     | ConstructorPattern Name [Pattern] -- ^ Data constructor: Node Leaf Leaf
--     | TuplePattern [Pattern] -- ^ Tuple: (a, b, ..., z)
--     | ListPattern [Pattern] -- ^ List: [a, b, ..., z]
--     | WildcardPattern
--     | PlaceHolderPattern -- ^ Represents Pattern parameter that should be substituted
--   deriving (Show, Eq, Generic, Ord)


type SimpleTypeMap = Map.Map String [Pattern]
type TypeMap = Map.Map String [(Pattern, String)]

-- |(Pattern, name of type)
type TypedPattern = (Pattern, String)
type PatternVector = [TypedPattern]

type ValueAbstractionVector = [Pattern]
type ValueAbstractionSet = [ValueAbstractionVector]

-- | A value abstraction that is valid if the constraint bag is satisfiable
data ConditionedValueAbstractionVector = CVAV
    { valueAbstraction :: ValueAbstractionVector
    , delta            :: [Constraint]
    } deriving (Show, Eq, Generic)

type ConditionedValueAbstractionSet = [ConditionedValueAbstractionVector]

instance ToJSON   ConditionedValueAbstractionVector
instance FromJSON ConditionedValueAbstractionVector

instance ToJSON   Pattern
instance FromJSON Pattern

instance ToJSON   Sign
instance FromJSON Sign

instance ToJSON   Literal
instance FromJSON Literal

instance Pretty Pattern where
    pretty (VariablePattern n) = n
    pretty (LiteralPattern Signless l) = H.prettyPrint l
    pretty (LiteralPattern Negative l) = '-' : H.prettyPrint l
    pretty (ConstructorPattern n []) = n
    pretty (ConstructorPattern n pats) = pars $ unwords $ n : map pretty pats
    pretty (TuplePattern pats) = tup $ map pretty pats
    pretty (ListPattern pats) = list $ map pretty pats
    pretty WildcardPattern = "_"
    pretty PlaceHolderPattern = "<placeholder>"
    pretty (GuardPattern _ const) = "Gbar(" ++ const ++ ")"


pars :: String -> String
pars s = "(" ++ s ++ ")"

braq :: String -> String
braq s = "[" ++ s ++ "]"

list :: [String] -> String
list = braq . commaSeparated

tup :: [String] -> String
tup = pars . commaSeparated

commaSeparated :: [String] -> String
commaSeparated = intercalate ", "

-- There can be functions with both nonexhaustive patterns and redundant patterns
data CoverageResult
    = CoverageResult
        [Pattern] -- ^ Missing patterns
        [Pattern] -- ^ Redundant patterns
  deriving (Show, Eq, Generic)

instance ToJSON   CoverageResult
instance FromJSON CoverageResult

instance Pretty CoverageResult where
    pretty (CoverageResult ms rs)
         = unlines
             [ "Missing:   " ++ list (map pretty ms)
             , "Redundant: " ++ list (map pretty rs)
             ]

data EvaluatednessResult
    = EvaluatednessResult
      [ArgumentEvaluatedness]
  deriving (Show, Eq, Generic)

instance ToJSON   EvaluatednessResult
instance FromJSON EvaluatednessResult

instance Pretty EvaluatednessResult where
    pretty (EvaluatednessResult aes)
        = list $ map pretty aes

data ArgumentEvaluatedness
    = ArgumentEvaluatedness
      [([Pattern] -- ^ List of patterns of input arguments
       , [EvaluatednessPattern]) -- ^ Pattern of evaluatedness for each argument.
      ]
  deriving (Show, Eq, Generic)

instance ToJSON   ArgumentEvaluatedness
instance FromJSON ArgumentEvaluatedness

instance Pretty ArgumentEvaluatedness where
    pretty (ArgumentEvaluatedness ls)
        = concatMap (\(ps, eps) -> pars $ commaSeparated [list $ map pretty ps, list $ map pretty eps]) ls

data EvaluatednessPattern
    = NotEvaluated
    | EvaluatedConstructor Name [EvaluatednessPattern]
    | EvaluatedListCons EvaluatednessPattern EvaluatednessPattern
    | EvaluatedTuple [EvaluatednessPattern]
    | EvaluatedLiteral Sign Literal
  deriving (Show, Eq, Generic)

instance ToJSON   EvaluatednessPattern
instance FromJSON EvaluatednessPattern

instance Pretty EvaluatednessPattern where
    pretty NotEvaluated = "_"
    pretty (EvaluatedConstructor n pats) = pars $ unwords $ n : map pretty pats
    pretty (EvaluatedListCons l ls) = pretty l ++ ":" ++ pretty ls
    pretty (EvaluatedTuple pats) = tup $ map pretty pats
    pretty (EvaluatedLiteral Signless l) = H.prettyPrint l
    pretty (EvaluatedLiteral Negative l) = '-' : H.prettyPrint l

class Pretty a where
    pretty :: a -> String

    prettyPrint :: a -> IO ()
    prettyPrint = putStrLn . pretty
