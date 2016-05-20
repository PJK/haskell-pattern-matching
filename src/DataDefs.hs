{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module DataDefs where

import           Data.Aeson            (FromJSON, ToJSON)
import           Data.List             (intercalate)
import qualified Data.Map              as Map
import           GHC.Generics          (Generic)
import           Language.Haskell.Exts hiding (DataOrNew (..), Name (..),
                                        Pretty, Type (..), prettyPrint)
import qualified Language.Haskell.Exts as H

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
  deriving (Show, Eq, Generic)

instance ToJSON   Type
instance FromJSON Type


data Function
    = Function
      Name
      Type -- We'll only deal with explicitly typed functions, for now.
      [Clause]
  deriving (Show, Eq, Generic)

instance ToJSON   Function
instance FromJSON Function

data Clause -- No need to include the right-hand side. We're only doing our analysis on the left part anyway.
    = Clause
      [Pattern] -- ^ The patterns to match for this clause, one for each argument.
  deriving (Show, Eq, Generic)

instance ToJSON   Clause
instance FromJSON Clause


data Constraint
    = BoolExp BoolE
    | Uncheckable String
  deriving (Show, Eq, Generic, Ord)

instance ToJSON   Constraint
instance FromJSON Constraint

data BoolE
    = LitBool Bool
    | BoolVar String
    | BoolNot BoolE
    | BoolOp BoolBinOp BoolE
    | IntBoolOp IntBoolBinOp IntE IntE
    | FracBoolOp FracBoolBinOp FracE FracE
  deriving (Show, Eq, Generic, Ord)

instance ToJSON   BoolE
instance FromJSON BoolE

data BoolBinOp
    = BoolAnd
    | BoolOr
    | BoolEQ
    | BoolNEQ
  deriving (Show, Eq, Generic, Ord)

instance ToJSON   BoolBinOp
instance FromJSON BoolBinOp

data FracE
    = FracLit Rational
    | FracVar String
    | FracUnOp FracUnOp FracE
    | FracOp FracBinOp FracE FracE
  deriving (Show, Eq, Generic, Ord)

instance ToJSON   FracE
instance FromJSON FracE

data FracUnOp
    = FracNeg
  deriving (Show, Eq, Generic, Ord)

instance ToJSON   FracUnOp
instance FromJSON FracUnOp

data FracBinOp
    = FracPlus
    | FracMin
    | FracTimes
    | FracDiv
  deriving (Show, Eq, Generic, Ord)

instance ToJSON   FracBinOp
instance FromJSON FracBinOp

data FracBoolBinOp
    = FracLT
    | FracLE
    | FracGT
    | FracGE
    | FracEQ
    | FracNEQ
  deriving (Show, Eq, Generic, Ord)

instance ToJSON   FracBoolBinOp
instance FromJSON FracBoolBinOp

data IntBoolBinOp
    = IntLT
    | IntLE
    | IntGT
    | IntGE
    | IntEQ
    | IntNEQ
  deriving (Show, Eq, Generic, Ord)

instance ToJSON   IntBoolBinOp
instance FromJSON IntBoolBinOp

data IntE
    = IntLit Integer
    | IntVar String
    | IntUnOp IntUnOp IntE
    | IntOp IntBinOp IntE IntE
  deriving (Show, Eq, Generic, Ord)

instance ToJSON   IntE
instance FromJSON IntE

data IntUnOp
    = IntNeg
  deriving (Show, Eq, Generic, Ord)

instance ToJSON   IntUnOp
instance FromJSON IntUnOp

data IntBinOp
    = IntPlus
    | IntTimes
    | IntMinus
    | IntDiv
    | IntMod
  deriving (Show, Eq, Generic, Ord)

instance ToJSON   IntBinOp
instance FromJSON IntBinOp

data Guard
    = ConstraintGuard Constraint
    | PatternGuard Pattern Constraint
    | LetGuard Pattern Constraint
  deriving (Show, Eq, Generic, Ord)

data Pattern
    = VariablePattern Name -- ^ Variable: a
    | LiteralPattern Sign Literal -- ^ Literal: -5
    | ConstructorPattern Name [Pattern] -- ^ Data constructor: Node Leaf Leaf
    | TuplePattern [Pattern] -- ^ Tuple: (a, b, ..., z)
    | ListPattern [Pattern] -- ^ List: [a, b, ..., z]
    | InfixConstructorPattern Pattern Name Pattern -- Name will be a symbol, this is used for lists, for example.
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

-- | Maps type names to lists of their constructors
type SimpleTypeMap = Map.Map String [Pattern]

-- |(Pattern, name of type)
type TypedPattern = (Pattern, String)
type PatternVector = [TypedPattern]

type ValueAbstractionVector = [Pattern]
type ValueAbstractionSet = [ValueAbstractionVector]

-- | Type equality constraint, e.g. Tree a ~ Tree (Maybe Int)
type TypeConstraint = (Type, Type)

-- | Models Gamma - type binding for variables
type Binding = Map.Map String Type

data ConstraintSet = ConstraintSet
    { termConstraints :: [Constraint]
    , typeConstraints :: [TypeConstraint]
    } deriving (Show, Eq, Generic)

-- | A value abstraction that is valid if the constraint bag is satisfiable
data ConditionedValueAbstractionVector = CVAV
    { valueAbstraction :: ValueAbstractionVector
    , gamma            :: Binding
    , delta            :: [Constraint]
    } deriving (Show, Eq, Generic)

type ConditionedValueAbstractionSet = [ConditionedValueAbstractionVector]

instance ToJSON   Type
instance FromJSON Type

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
    pretty (GuardPattern pat const) = "Gbar(" ++ pretty pat ++ ", " ++ show const ++ ")"


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
