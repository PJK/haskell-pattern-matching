{-# LANGUAGE QuasiQuotes #-}
module Gatherer where

import           Control.Applicative   (liftA2)
import           Control.Monad         (forM)
import           Data.List             (nub)
import qualified Data.Map              as Map
import           Data.Maybe            (catMaybes)
import           DataDefs
import           Debug.Trace
import           Language.Haskell.Exts hiding (DataOrNew (..), Name (..),
                                        Pretty, Type (..), prettyPrint)
import qualified Language.Haskell.Exts as H
import           TH
import           Types



type MayFail = Either GatherError

signFact :: Num a => Sign -> a
signFact Signless = 1
signFact Negative = -1

-- | Transforms all patterns into the standard form (See figure 7)
desugarPattern :: TypedPattern -> PatternVector
desugarPattern (LiteralPattern sign (Frac f), _)
    = (VariablePattern var, "__guard_var"):desugarGuard (ConstraintGuard $ BoolExp $ FracBoolOp FracEQ (FracVar var) (FracLit f))
    where var = "__x"

desugarPattern (LiteralPattern sign (Int i), _)
    = (VariablePattern var, "__guard_var"):desugarGuard (ConstraintGuard $ BoolExp $ IntBoolOp IntEQ (IntVar var) (IntLit i))
    where var = "__x"

desugarPattern (WildcardPattern, wildcardType)
    = [(VariablePattern "_", wildcardType)] -- TODO Replace with Variable of same type
desugarPattern x = [x]

-- | Recover the original number of parameters before desugaring and guard expansion.
arity :: Clause -> Int
arity (Clause (GuardPattern _ _:cs)) = arity (Clause cs)
arity (Clause (_:cs)) = 1 + arity (Clause cs)
arity (Clause []) = 0

desugarGuard :: Guard -> PatternVector
desugarGuard (ConstraintGuard constraint)
    = [(GuardPattern (ConstructorPattern "True" []) constraint, guardType)]
desugarGuard _ = error "FIXME: implement lets and patternGuards"

getTypedPatternVectors :: Function -> MayFail [PatternVector]
getTypedPatternVectors (Function _ functionType patterns) = Right $
    map (`zip` typesList) (patternsList patterns)
    where
        typeName :: Type -> String
        typeName (TypeConstructor name) = name
        typeName _                      = error "FIXME: we can only handle simple nominal types"

        typesList = map typeName (extractType functionType)


desugarPatternVector :: PatternVector -> PatternVector
desugarPatternVector = concatMap desugarPattern

patternsList :: [Clause] -> [[Pattern]]
patternsList = map (\xs -> case xs of Clause patterns -> patterns)

extractType :: Type -> [Type]
extractType t = case t of
                FunctionType t1 t2 -> extractType t1 ++ extractType t2
                TypeConstructor t  -> [TypeConstructor t]
                _ -> error "no way to extract a type from this"


-- | Provides initial binding for each clause -- all variables that occur among the patterns
-- | have to be assigned a type.
initialGammas :: Function -> MayFail [Binding]
initialGammas (Function _ functionType patterns)
    = Right $ map (buildGamma functionTypes) (patternsList patterns)
    where
        functionTypes = extractType functionType

        buildGamma :: [Type] -> [Pattern] -> Binding
        buildGamma x y | trace ("Buildgamma: " ++ show (x, y)) False = error "asdfa"
        buildGamma ts (GuardPattern _ _:ps) = buildGamma ts ps -- Do not bind guards
        buildGamma (t:ts) (VariablePattern name:ps)
            = Map.insert name t (buildGamma ts ps)
        buildGamma (_:ts) (_:ps) = buildGamma ts ps
        buildGamma [_] []        = Map.fromList [] -- One extra element for return type

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
--- > This is not a problem because GHC won't work anyway so we cannot do anything about the rest
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
                [name] -> return $ Just (name, concatMap snd cs)
                _ -> err $ "Illegal list of clauses in function binding: " ++ show ms
        go _ = return Nothing

mkClause :: Match -> MayFail (Name, [Clause])
mkClause (Match _ name pats _ (UnGuardedRhs _) _) = (,) <$> mkName name <*> ((\ps -> [Clause ps]) <$> mapM mkPattern pats)
mkClause (Match _ name pats _ (GuardedRhss rhss) _)
    = (,) <$> mkName name <*> (forM rhss $ \(GuardedRhs _ [Qualifier exp] _) ->
        (Clause <$> (liftA2 snoc (forM pats mkPattern) (GuardPattern <$> (pure $ ConstructorPattern "True" []) <*> mkConstraint exp))))

mkConstraint :: Exp -> MayFail Constraint
mkConstraint e = return $ Uncheckable $ show e -- TODO parse nicer guards

snoc :: [a] -> a -> [a]
snoc as a = as ++ [a]


mkQname :: H.QName -> MayFail Name
mkQname (UnQual n) = mkName n
mkQname (Special _) = err "Tried to use a special name qualification, not sure what to do with that yet."
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
mkType (H.TyList t) = ListType <$> mkType t
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
mkPattern (PInfixApp h cons t) = InfixConstructorPattern <$> mkPattern h <*> mkQname cons <*> mkPattern t
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

