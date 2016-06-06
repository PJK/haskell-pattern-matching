{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module DataDefs where

import           Data.Aeson            (FromJSON, ToJSON)
import           Data.List             (intercalate)
import qualified Data.Map              as Map
import qualified Data.Set              as Set
import           GHC.Generics          (Generic)
import           Language.Haskell.Exts hiding (DataOrNew (..), Name (..),
                                        Pretty, Type (..), prettyPrint)
import qualified Language.Haskell.Exts as H

type Name = String


-- This is confusing. DataType ~ data Name = .... declaration
data DataType
    = DataType Name [TypeVariable] [Constructor]
  deriving (Show, Eq, Ord)

data TypeVariable
    = TypeVariable Name
  deriving (Show, Eq, Ord)

data Constructor
    = Constructor Name [Type]
  deriving (Show, Eq, Ord)

data Type
    = FunctionType Type Type -- ^ Function: a -> b
    | TupleType [Type] -- ^ Tuple: (a, b, ..., z)
        -- TODO Determine whether we really want to view this as a special case instead of as a constructor
        --   Answer(?): Yes, for better error messages.
    | ListType Type -- ^ List: [a]
    | TypeApplication Type Type -- ^ Application of type constructor: Tree a
    | VariableType Name -- ^ Type variable: a
    | TypeConstructor Name -- ^ Named constructor: Tree
  deriving (Show, Eq, Generic, Ord)

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

instance Pretty Clause where
    pretty (Clause ps) = unwords $ map pretty ps

data Constraint
    = IsBottom Name
    | VarsEqual Name Name
    | VarEqualsBool Name BoolE
    | VarEqualsCons Name Name [Pattern]
    | VarEqualsPat Name Pattern -- ^ There are constructs that don't have constructor Name
    | Uncheckable String
  deriving (Show, Eq, Generic, Ord)

instance ToJSON   Constraint
instance FromJSON Constraint

instance Pretty Constraint where
    pretty (IsBottom v)         = v ++ "= ⊥"
    pretty (VarsEqual n1 n2)    = n1 ++ " = " ++ n2
    pretty (Uncheckable s)      = s
    pretty (VarEqualsBool v be) = v ++ " = " ++ pretty be
    pretty (VarEqualsCons v n as) = v ++ " = (" ++ n ++ unwords (map show as) ++ ")"
    pretty (VarEqualsPat v p) = v ++ " = " ++ show p

data BoolE
    = LitBool Bool
    | Otherwise
    | BoolVar Name
    | BoolNot BoolE
    | BoolOp BoolBinOp BoolE BoolE
    | IntBoolOp IntBoolBinOp IntE IntE
  deriving (Show, Eq, Generic, Ord)

instance ToJSON   BoolE
instance FromJSON BoolE

instance Pretty BoolE where
    pretty (LitBool b) = show b
    pretty Otherwise = "otherwise"
    pretty (BoolVar v) = v
    pretty (BoolNot n) = "not " ++ pretty n
    pretty (BoolOp bo be1 be2) = unwords [pretty be1, pretty bo, pretty be2]
    pretty (IntBoolOp ibo ie1 ie2) = unwords [pretty ie1, pretty ibo, pretty ie2]


data BoolBinOp
    = BoolAnd
    | BoolOr
    | BoolEQ
    | BoolNEQ -- XOR
  deriving (Show, Eq, Generic, Ord)

instance ToJSON   BoolBinOp
instance FromJSON BoolBinOp

instance Pretty BoolBinOp where
    pretty BoolAnd = "&&"
    pretty BoolOr = "||"
    pretty BoolEQ = "=="
    pretty BoolNEQ = "/="

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

instance Pretty IntBoolBinOp where
    pretty IntLT = "<"
    pretty IntLE = "<="
    pretty IntGT = ">"
    pretty IntGE = ">="
    pretty IntEQ = "=="
    pretty IntNEQ = "/="

data IntE
    = IntLit Integer
    | IntVar Name
    | IntUnOp IntUnOp IntE
    | IntOp IntBinOp IntE IntE
  deriving (Show, Eq, Generic, Ord)

instance ToJSON   IntE
instance FromJSON IntE

instance Pretty IntE where
    pretty (IntLit i) = show i
    pretty (IntVar v) = v
    pretty (IntUnOp IntNeg ie) = "- " ++ pretty ie
    pretty (IntOp ibo ie1 ie2) = unwords [pretty ie1, pretty ibo, pretty ie2]

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

instance Pretty IntBinOp where
    pretty IntPlus = "+"
    pretty IntTimes = "*"
    pretty IntMinus = "-"
    pretty IntDiv = "`div`"
    pretty IntMod = "`mod`"


data Guard
    = ConstraintGuard BoolE
    | PatternGuard Pattern Constraint
    | LetGuard Pattern Constraint
  deriving (Show, Eq, Generic, Ord)

data Pattern
    = VariablePattern Name -- ^ Variable: a
    | LiteralPattern Sign Literal -- ^ Literal: -5
    | ConstructorPattern Name [Pattern] -- ^ Data constructor: Node Leaf Leaf
    | TuplePattern [Pattern] -- ^ Tuple: (a, b, ..., z)
    | EmptyListPattern -- ^ List: [a, b, ..., z]
    | InfixConstructorPattern Pattern Name Pattern -- Name will be a symbol, this is used for lists, for example.
    | WildcardPattern
    | IntVariablePattern -- ^ Will be replaced by a fresh integer variable during processing
    -- | PlaceHolderPattern -- ^ Represents Pattern parameter that should be substituted
    | GuardPattern Pattern Expression
  deriving (Show, Eq, Generic, Ord)

data Expression
    = UnknownExp String
    | BExp BoolE
  deriving (Show, Eq, Generic, Ord)

instance ToJSON   Expression
instance FromJSON Expression

instance Pretty Expression where
    pretty (UnknownExp s) = s
    pretty (BExp be) = pretty be

-- | Replaces the previous attemps to provide maps between types and constructors
-- | Since all the translations are isomorphisms (hopefully), all we need is the type
-- | information.
type TypeUniverse = Set.Set DataType

type PatternVector = [Pattern]

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
-- | Invariant: All VarPatterns in valueAbstraction are bound by gamma
-- Challenge: encode this as a GADT
data ConditionedValueAbstractionVector = CVAV
    { valueAbstraction :: ValueAbstractionVector
    , gamma            :: Binding
    , delta            :: ConstraintSet
    } deriving (Show, Eq, Generic)

type ConditionedValueAbstractionSet = [ConditionedValueAbstractionVector]


instance ToJSON   ConstraintSet
instance FromJSON ConstraintSet

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
    pretty l@(InfixConstructorPattern i ":" is)
        = if endsWithEmptyListPattern l
          then let items = map pretty (collectListItems l)
               in "[" ++ intercalate ", " items ++ "]"
          else "(" ++ pretty i ++ ":" ++ pretty is ++ ")"
      where
        endsWithEmptyListPattern :: Pattern -> Bool
        endsWithEmptyListPattern (InfixConstructorPattern _ ":" is) = endsWithEmptyListPattern is
        endsWithEmptyListPattern EmptyListPattern = True
        endsWithEmptyListPattern _ = False
    pretty (InfixConstructorPattern p1 name p2) = pretty p1 ++ name ++ pretty p2
    pretty (TuplePattern pats) = tup $ map pretty pats
    pretty EmptyListPattern = "[]"
    pretty WildcardPattern = "_"
    pretty (GuardPattern (ConstructorPattern "True" []) const) = "| " ++ pretty const
    pretty (GuardPattern pat const) = "| " ++ pretty pat ++ " <- " ++ pretty const
    pretty IntVariablePattern = "Int variable"


collectListItems :: Pattern -> [Pattern]
collectListItems (InfixConstructorPattern p1 ":" p2) = p1:collectListItems p2
collectListItems EmptyListPattern = []
collectListItems WildcardPattern = []
collectListItems a = error $ "not a list item: " ++ show a

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
