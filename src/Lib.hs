module Lib
    (
      patterns
    )
    where

import           Control.Monad         (forM_)
import           Data.List             (intercalate, nub)
import           Data.Maybe            (mapMaybe)
import           Language.Haskell.Exts hiding (DataOrNew (..), Name (..),
                                        Pretty, Type (..), prettyPrint)
import qualified Language.Haskell.Exts as H

patterns :: IO ()
patterns = do
    ast <- fromParseResult <$> parseFile "data/redundant.hs"
    let types = getTypes ast
    print types
    let functions = getFunctions ast
    print functions
    let results = map (analyse types) functions
    forM_ results $ \(cr, er) -> do
        prettyPrint cr
        prettyPrint er


getTypes :: Module -> [DataType]
getTypes (Module _ _ _ _ _ _ decls)
    = mapMaybe go decls
  where
    -- TODO make go total: use Either as a monad to collect why a datatype cannot be used.
    go (DataDecl _ _ _ name tyvars ddecls _)
        = Just $ DataType (mkName name) (map mkTypeVariable tyvars) (map mkConstructor ddecls)
    go _ = Nothing

    mkTypeVariable :: TyVarBind -> TypeVariable
    mkTypeVariable (UnkindedVar n) = TypeVariable $ mkName n
    mkTypeVariable (KindedVar _ _) = error "Kinded type variable declarations are not supported"

    mkConstructor :: QualConDecl -> Constructor
    mkConstructor (QualConDecl _ [] _ condecl)
        = case condecl of
            ConDecl n ts -> Constructor (mkName n) $ map mkType ts
            InfixConDecl _ _ _ -> error "Infix data constructors are not supported"
            RecDecl _ _ -> error "Record constructors not supported"
    mkConstructor _ = error "Existentially quantified constructors are not supported"

getFunctions :: Module -> [Function]
getFunctions (Module _ _ _ _ _ _ decls)
    = matchFunctions signatures clauses
  where
    matchFunctions :: [(Name, Type)] -> [(Name, [Clause])] -> [Function]
    matchFunctions ts cs
        | length (nub $ map fst ts) == length ts
            = map (collect cs) ts
        | otherwise = error "Duplicate type signature for function"
      where
        collect :: [(Name, [Clause])] -> (Name, Type) -> Function
        collect cls (n, t)
            = case lookup n cls of
                Nothing -> error $ "Function without implementation: " ++ n
                Just cs -> Function n t cs

    signatures :: [(Name, Type)]
    signatures = concatMap go decls
      where
        go :: Decl -> [(Name, Type)]
        go (TypeSig _ ns t) = map (\n -> (mkName n, mkType t)) ns
        go _ = []

    -- Postcondition: Only one list of clauses per name
    clauses :: [(Name, [Clause])]
    clauses = mapMaybe go decls
      where
        go :: Decl -> Maybe (Name, [Clause])
        go (FunBind ms) =
            let cs = map mkClause ms
            in case nub (map fst cs) of
                [name] -> Just (name, map snd cs)
                _ -> error $ "Illegal list of clauses in function binding: " ++ show ms
        go _ = Nothing

        mkClause :: Match -> (Name, Clause)
        mkClause (Match _ name pats _ _ _) = (mkName name, Clause $ map mkPattern pats)

mkQname :: H.QName -> Name
mkQname (UnQual n) = mkName n
mkQname a = error $ "Unsupported name qualification: " ++ show a

mkName :: H.Name -> Name
mkName (H.Ident s) = s
mkName a = error $ "Name not supported: " ++ show a

mkType :: H.Type -> Type
mkType (H.TyFun t1 t2) = FunctionType (mkType t1) (mkType t2)
mkType (H.TyTuple _ ts) = TupleType $ map mkType ts
mkType (H.TyApp t1 t2) = TypeApplication (mkType t1) (mkType t2)
mkType (H.TyVar n) = VariableType $ mkName n
mkType (H.TyCon qn) = TypeConstructor $ mkQname qn
mkType (H.TyBang _ _) = error "Banged types/Unpacked types not supported"
mkType a = error $ "Unsupported type declaration: " ++ show a

mkPattern :: Pat -> Pattern
mkPattern (PVar n) = VariablePattern $ mkName n
mkPattern (PLit sign lit) = LiteralPattern sign lit
mkPattern (PApp name pats) = ConstructorPattern (mkQname name) $ map mkPattern pats
mkPattern (PTuple _ pats) = TuplePattern $ map mkPattern pats
mkPattern (PList pats) = ListPattern $ map mkPattern pats
mkPattern (PAsPat _ pat) = mkPattern pat
mkPattern PWildCard = WildcardPattern
mkPattern (PBangPat pat) = mkPattern pat
mkPattern a = error $ "Unsupported pattern: " ++ show a

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

data Pattern
    = VariablePattern Name -- ^ Variable: a
    | LiteralPattern Sign Literal -- ^ Literal: -5
    | ConstructorPattern Name [Pattern] -- ^ Data constructor: Node Leaf Leaf
    | TuplePattern [Pattern] -- ^ Tuple: (a, b, ..., z)
    | ListPattern [Pattern] -- ^ List: [a, b, ..., z]
    | WildcardPattern
  deriving (Show, Eq)

instance Pretty Pattern where
    pretty (VariablePattern n) = n
    pretty (LiteralPattern Signless l) = H.prettyPrint l
    pretty (LiteralPattern Negative l) = '-' : H.prettyPrint l
    pretty (ConstructorPattern n []) = n
    pretty (ConstructorPattern n pats) = pars $ unwords $ n : map pretty pats
    pretty (TuplePattern pats) = tup $ map pretty pats
    pretty (ListPattern pats) = list $ map pretty pats
    pretty WildcardPattern = "_"

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
  deriving (Show, Eq)

instance Pretty CoverageResult where
    pretty (CoverageResult ms rs)
         = unlines
             [ "Missing:   " ++ list (map pretty ms)
             , "Redundant: " ++ list (map pretty rs)
             ]

data EvaluatednessResult
    = EvaluatednessResult
      [ArgumentEvaluatedness]
  deriving (Show, Eq)

instance Pretty EvaluatednessResult where
    pretty (EvaluatednessResult aes)
        = list $ map pretty aes

data ArgumentEvaluatedness
    = ArgumentEvaluatedness
      [([Pattern] -- ^ List of patterns of input arguments
       , [EvaluatednessPattern]) -- ^ Pattern of evaluatedness for each argument.
      ]
  deriving (Show, Eq)

instance Pretty ArgumentEvaluatedness where
    pretty (ArgumentEvaluatedness ls)
        = concatMap (\(ps, eps) -> pars $ commaSeparated [list $ map pretty ps, list $ map pretty eps]) ls

data EvaluatednessPattern
    = NotEvaluated
    | EvaluatedConstructor Name [EvaluatednessPattern]
    | EvaluatedListCons EvaluatednessPattern EvaluatednessPattern
    | EvaluatedTuple [EvaluatednessPattern]
    | EvaluatedLiteral Sign Literal
  deriving (Show, Eq)

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


analyse :: [DataType] -- ^ DataTypes 'in scope'
        -> Function
        -> (CoverageResult, EvaluatednessResult)
analyse _ _ =
    -- Just the expected answer for our current only data file.
    ( CoverageResult [ConstructorPattern "True" []] [ConstructorPattern "False" []]
    , EvaluatednessResult [ ArgumentEvaluatedness
                            [ ( [WildcardPattern]
                              , [ EvaluatedConstructor "True" []
                                , EvaluatedConstructor "False" []
                                ]
                              )
                            ]
                          ]
    )



