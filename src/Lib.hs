{-# LANGUAGE DeriveGeneric #-}
module Lib where


import           Control.Monad            (forM_)
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as LB
import           Data.List                (intercalate, nub)
import           Data.Maybe               (mapMaybe)
import qualified Data.Map                 as Map
import           GHC.Generics             (Generic)
import           Language.Haskell.Exts    hiding (DataOrNew (..), Name (..),
                                           Pretty, Type (..), prettyPrint)
import qualified Language.Haskell.Exts    as H
import qualified System.Environment       as Env
import DataDefs
import ClauseProcessing


patterns :: IO ()
patterns = do
    args <- Env.getArgs
    print args
    mapM_ process args


process :: String -> IO ()
process inputFile = do
    results <- doItAll inputFile
    ast <- fromParseResult <$> parseFile inputFile
    -- Mock passing the previous U
    print $ getTypes ast
    print $ getTypeConstructorsMap ast
    mapM_ (\func@(Function name _ _) -> do
            putStrLn $ "Processing " ++ name
            prettyIteratedVecProc 0 (getPatternVectors func) [[VariablePattern "x1", VariablePattern "x2"]] (getTypeConstructorsMap ast))
            (getFunctions ast)

--     forM_ results $ \(cr, er) -> do
--         prettyPrint cr
--         prettyPrint er
--     LB.putStr $ encodePretty results

getPatternVectors :: Function -> [[Pattern]]
getPatternVectors (Function _ _ patterns) = map (\xs -> case xs of Clause patterns -> patterns) patterns

doItAll :: FilePath -> IO [(CoverageResult, EvaluatednessResult)]
doItAll fp = do
    -- FIXME do some actual error handling here.
    ast <- fromParseResult <$> parseFile fp
    -- FIXME make sure these are total
    let types = getTypes ast
        functions = getFunctions ast
        results = map (analyse types) functions
    return results

getTypesMap :: Module -> Map.Map String [Constructor]
getTypesMap mod = Map.fromList $ map (\t -> case t of DataType name _ constructors -> (name, constructors)) (getTypes mod)

getTypeConstructorsMap :: Module -> TypeMap
getTypeConstructorsMap mod = Map.map (map constructorToPattern) (getTypesMap mod)
    where
        -- TODO handle params
        constructorToPattern (Constructor name args) = ConstructorPattern name []


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


analyse :: [DataType] -- ^ DataTypes 'in scope'
        -> Function
        -> (CoverageResult, EvaluatednessResult)
-- analyse _ (Function _ _ clauses) =
analyse _ _ =
    -- Just the expected answer for our current only data file.
    -- See assignment.pdf for another example of this format.
    ( CoverageResult
        [ConstructorPattern "True" []] -- Missing patterns
        [ConstructorPattern "False" []] -- Redundant patterns (exact patterns that we find)
    , EvaluatednessResult
        [ ArgumentEvaluatedness -- Length = number of arguments to the function
          [ ( [WildcardPattern] -- Length = number of arguments to the function
            , [ EvaluatedConstructor "True" []
              , EvaluatedConstructor "False" []
              ]
            )
          ]
        ]
    )



