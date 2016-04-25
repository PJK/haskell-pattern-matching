module Lib
    (
      patterns
    )
    where

import           Data.Maybe            (mapMaybe)
import           Language.Haskell.Exts hiding (DataOrNew (..), Name (..),
                                        Type (..))
import qualified Language.Haskell.Exts as H

patterns :: IO ()
patterns = do
    ast <- fromParseResult <$> parseFile "data/redundant.hs"
    -- print ast
    -- putStrLn $ prettyPrint ast
    print $ getTypes ast


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

    mkName (H.Ident s) = s
    mkName a = error $ "Name not supported: " ++ show a

    mkType (H.TyFun t1 t2) = FunctionType (mkType t1) (mkType t2)
    mkType (H.TyTuple _ ts) = TupleType $ map mkType ts
    mkType (H.TyApp t1 t2) = TypeApplication (mkType t1) (mkType t2)
    mkType (H.TyVar n) = VariableType $ mkName n
    mkType (H.TyCon qn)
        = case qn of
            UnQual n -> TypeConstructor $ mkName n
            _ -> error $ "Unsupported type constructor qualification: " ++ show qn
    mkType (H.TyBang _ _) = error "Banged types/Unpacked types not supported"
    mkType a = error $ "Unsupported type declaration: " ++ show a

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

