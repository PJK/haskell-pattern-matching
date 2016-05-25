module Oracle.SBVQueries where

import           Control.Monad.State       (MonadState (..), StateT, evalStateT,
                                            gets, modify)
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Data.Maybe                (catMaybes)
import           Data.SBV
import           DataDefs
import qualified Text.Show.Pretty          as Pr
import           Types


-- boolESat :: BoolE -> IO Bool
-- boolESat b = do
--     SatResult result <- boolESatResult b
--     case result of
--         -- Overapproximation as per section 6.1 -- unless we can prove it unsatisfiable, we must
--         -- assume it may match
--         Unsatisfiable _ -> return False
--         _ -> return True

boolESatResult :: BoolE -> IO SatResult
boolESatResult b = sat $ flip evalStateT initState $ buildSBool b
  where
    initState = SBuilderState { boolVars = [], integerVars = [] }


buildSBool :: BoolE -> SymbolicBuilder SBool
buildSBool (LitBool True)  = return true
buildSBool (LitBool False) = return false
buildSBool Otherwise = return true
buildSBool (BoolVar v) = boolVar v
buildSBool (BoolNot be) = bnot <$> buildSBool be
buildSBool (BoolOp bo bv1 bv2) = do
    b1 <- buildSBool bv1
    b2 <- buildSBool bv2
    let o = case bo of
            BoolAnd -> (&&&)
            BoolOr -> (|||)
            BoolEQ -> (<=>)
            BoolNEQ -> (<+>)
    return $ b1 `o` b2

buildSBool (IntBoolOp op ie1 ie2) = do
    i1 <- buildSInt ie1
    i2 <- buildSInt ie2
    let o = case op of
            IntLT  -> (.<)
            IntLE  -> (.<=)
            IntGT  -> (.>)
            IntGE  -> (.>=)
            IntEQ  -> (.==)
            IntNEQ -> (./=)
    return $ i1 `o` i2

buildSBool (FracBoolOp _ _ _) = error "fractions are not supported yet."

buildSInt :: IntE -> SymbolicBuilder SInteger
buildSInt (IntLit i) = return $ literal i
buildSInt (IntVar v) = integerVar v
buildSInt (IntUnOp IntNeg i) = do
    si <- buildSInt i
    return $ - si
buildSInt (IntOp io iv1 iv2) = do
    i1 <- buildSInt iv1
    i2 <- buildSInt iv2
    let o = case io of
            IntPlus -> (+)
            IntTimes -> (*)
            IntMinus -> (-)
            IntDiv -> sDiv
            IntMod -> sMod
    return $ i1 `o` i2

varsInBE :: BoolE -> [Name]
varsInBE (LitBool _)            = []
varsInBE (BoolNot be)           = varsInBE be
varsInBE (BoolOp _ be1 be2)     = varsInBE be1 ++ varsInBE be2
varsInBE (IntBoolOp _ ie1 ie2)  = varsInIE ie1 ++ varsInIE ie2
varsInBE (FracBoolOp _ fe1 fe2) = varsInFE fe1 ++ varsInFE fe2
varsInBE (BoolVar var)          = [var]

varsInIE :: IntE -> [Name]
varsInIE (IntLit _)             = []
varsInIE (IntUnOp _ ie)         = varsInIE ie
varsInIE (IntOp _ ie1 ie2)      = varsInIE ie1 ++ varsInIE ie2
varsInIE (IntVar var)           = [var]

varsInFE :: FracE -> [Name]
varsInFE (FracLit _)            = []
varsInFE (FracUnOp _ fe)        = varsInFE fe
varsInFE (FracOp _ fe1 fe2)     = varsInFE fe1 ++ varsInFE fe2
varsInFE (FracVar var)          = [var]


type SymbolicBuilder = StateT SBuilderState Symbolic

data SBuilderState = SBuilderState
    { boolVars    :: [(String, SBool)]
    , integerVars :: [(String, SInteger)]
    }

boolVar :: String -> SymbolicBuilder SBool
boolVar = lookupVar boolVars sBool (\name v -> modify (\s -> s { boolVars = (name, v) : boolVars s } ) )

integerVar :: String -> SymbolicBuilder SInteger
integerVar = lookupVar integerVars sInteger (\name v -> modify (\s -> s { integerVars = (name, v) : integerVars s } ) )

lookupVar :: (SBuilderState -> [(String, a)]) -- To lookup
         -> (String -> Symbolic a) -- To generate a new one if there isn't one yet
         -> (String -> a -> SymbolicBuilder ()) -- To add the var after generation, because record accessors cannot be passed around, apparently
         -> String -> SymbolicBuilder a
lookupVar func genFunc update name = do
    bvs <- gets func
    case lookup name bvs of
        Nothing -> do
            bv <- lift $ genFunc name
            update name bv
            return bv
        Just sb -> return sb

