{-# LANGUAGE QuasiQuotes #-}
module Gatherer where

import           Data.List             (nub)
import qualified Data.Map              as Map
import           Data.Maybe            (catMaybes)
import           DataDefs
import           Language.Haskell.Exts hiding (DataOrNew (..), Name (..),
                                        Pretty, Type (..), prettyPrint)
import qualified Language.Haskell.Exts as H
import           TH
import           Types



type MayFail = Either GatherError

-- | Transforms all patterns into the standard form (See figure 7)
desugarPattern :: TypedPattern -> PatternVector
desugarPattern (LiteralPattern sign literal, _)
    = (VariablePattern "__x", "__guard_var"):desugarGuard (ConstraintGuard equality)
    where
        showSign Signless = []
        showSign Negative = "-"
        equality = "__x = " ++ showSign sign ++ show literal
desugarPattern (WildcardPattern, wildcardType)
    = [(VariablePattern "_", wildcardType)] -- Replace with Variable of same type
desugarPattern x = [x]

desugarGuard :: Guard -> PatternVector
desugarGuard (ConstraintGuard constraint)
    = [(GuardPattern (ConstructorPattern "True" []) constraint, guardType)]
desugarGuard _ = error "FIXME: implement lets and patternGuards"

getTypedPatternVectors :: Function -> MayFail [PatternVector]
getTypedPatternVectors (Function _ functionType patterns) = Right $
    map (`zip` typesList) patternsList
    where
        typeName :: Type -> String
        typeName (TypeConstructor name) = name
        typeName _                      = error "FIXME: we can only handle simple nominal types"

        extractType :: Type -> [Type]
        extractType t = case t of
                        FunctionType t1 t2 -> extractType t1 ++ extractType t2
                        TypeConstructor t  -> [TypeConstructor t]
                        _ -> error "no way to extract a type from this"

        patternsList = map (\xs -> case xs of Clause patterns -> patterns) patterns

        typesList = map typeName (extractType functionType)

desugarPatternVector :: PatternVector -> PatternVector
desugarPatternVector = concatMap desugarPattern



getTypesMap :: Module -> MayFail (Map.Map String [Constructor])
getTypesMap mod = do
    types <- getTypes mod
    return $ Map.fromList
           $ map (\t -> case t of DataType name _ constructors -> (name, constructors))
           $ types ++ builtinTypes


getPlainTypeConstructorsMap :: Module -> MayFail SimpleTypeMap
getPlainTypeConstructorsMap mod = do
    typesMap <- getTypesMap mod
    return $ Map.map (map constructorToPattern) typesMap
    where
        substitutionPatterns parameters = replicate (length parameters) PlaceHolderPattern
        constructorToPattern (Constructor name parameters) = ConstructorPattern name (substitutionPatterns  parameters)

err :: String -> MayFail a
err = Left

-- FIXME currently it's still returning Left in case of _any_ error.
-- We have to decide whether that's what we want or whether we want it to parse as many types as possible
-- same question for @getFunctions@
getTypes :: Module -> MayFail [DataType]
getTypes (Module _ _ _ _ _ _ decls)
    = catMaybes <$> mapM go decls -- Add builtins
  where
    -- TODO make go total: use Either as a monad to collect why a datatype cannot be used.
    go :: Decl -> MayFail (Maybe DataType)
    go (DataDecl _ _ _ name tyvars ddecls _) = do
        n <- mkName name
        tvs <- mapM mkTypeVariable tyvars
        cs <- mapM mkConstructor ddecls
        return $ Just $ DataType n tvs cs
    go _ = return Nothing

    mkTypeVariable :: TyVarBind -> MayFail TypeVariable
    mkTypeVariable (UnkindedVar n) = TypeVariable <$> mkName n
    mkTypeVariable (KindedVar _ _) = err "Kinded type variable declarations are not supported"

    mkConstructor :: QualConDecl -> MayFail Constructor
    mkConstructor (QualConDecl _ [] _ condecl)
        = case condecl of
            ConDecl n ts -> Constructor <$> mkName n <*> mapM mkType ts
            InfixConDecl _ _ _ -> err "Infix data constructors are not supported"
            RecDecl _ _ -> err "Record constructors not supported"
    mkConstructor _ = err "Existentially quantified constructors are not supported"


getFunctions :: Module -> MayFail [Function]
getFunctions (Module _ _ _ _ _ _ decls) = do
    ss <- signatures
    cs <- clauses
    matchFunctions ss cs
  where
    matchFunctions :: [(Name, Type)] -> [(Name, [Clause])] -> MayFail [Function]
    matchFunctions ts cs
        | length (nub $ map fst ts) == length ts
            = mapM (collect cs) ts
        | otherwise = err "Duplicate type signature for function"
      where
        collect :: [(Name, [Clause])] -> (Name, Type) -> MayFail Function
        collect cls (n, t)
            = case lookup n cls of
                Nothing -> err $ "Function without implementation: " ++ n
                Just cs -> return $ Function n t cs

    signatures :: MayFail [(Name, Type)]
    signatures = concat <$> mapM go decls
      where
        go :: Decl -> MayFail [(Name, Type)]
        go (TypeSig _ ns t) = mapM (\n -> (,) <$> mkName n <*> mkType t) ns
        go _ = return []

    -- Postcondition: Only one list of clauses per name
    clauses :: MayFail [(Name, [Clause])]
    clauses = catMaybes <$> mapM go decls
      where
        go :: Decl -> MayFail (Maybe (Name, [Clause]))
        go (FunBind ms) = do
            cs <- mapM mkClause ms
            case nub (map fst cs) of
                [name] -> return $ Just (name, map snd cs)
                _ -> err $ "Illegal list of clauses in function binding: " ++ show ms
        go _ = return Nothing

        mkClause :: Match -> MayFail (Name, Clause)
        mkClause (Match _ name pats _ _ _) = (,) <$> mkName name <*> (Clause <$> mapM mkPattern pats)


mkQname :: H.QName -> MayFail Name
mkQname (UnQual n) = mkName n
mkQname a = err $ "Unsupported name qualification: " ++ show a

mkName :: H.Name -> MayFail Name
mkName (H.Ident s) = return s
mkName a = err $ "Name not supported: " ++ show a

mkType :: H.Type -> MayFail Type
mkType (H.TyFun t1 t2) = FunctionType <$> mkType t1 <*> mkType t2
mkType (H.TyTuple _ ts) = TupleType <$> mapM mkType ts
mkType (H.TyApp t1 t2) = TypeApplication <$> mkType t1 <*> mkType t2
mkType (H.TyVar n) = VariableType <$> mkName n
mkType (H.TyCon qn) = TypeConstructor <$> mkQname qn
mkType (H.TyBang _ _) = err "Banged types/Unpacked types not supported"
mkType a = err $ "Unsupported type declaration: " ++ show a

mkPattern :: Pat -> MayFail Pattern
mkPattern (PVar n) = VariablePattern <$> mkName n
mkPattern (PLit sign lit) = return $ LiteralPattern sign lit
mkPattern (PApp name pats) = ConstructorPattern <$> mkQname name <*> mapM mkPattern pats
mkPattern (PTuple _ pats) = TuplePattern <$> mapM mkPattern pats
mkPattern (PList pats) = ListPattern <$> mapM mkPattern pats
mkPattern (PAsPat _ pat) = mkPattern pat
mkPattern PWildCard = return WildcardPattern
mkPattern (PBangPat pat) = mkPattern pat
mkPattern (PParen pat) = mkPattern pat
mkPattern a = err $ "Unsupported pattern: " ++ show a

-- Compile the base datatypes into the binary.
-- Yes, this uses unsafe functions, but it'll start failing tests
-- TODO: make this a compile error by modifying TH.hs
builtinTypes :: [DataType]
builtinTypes
    = let Right baseTypes = getTypes
            $ fromParseResult
            $ parseFileContents [litFile|build_data/BaseDataTypes.hs|]
      in baseTypes

