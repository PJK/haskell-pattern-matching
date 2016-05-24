module Oracle.SBVQueries where

import           Data.Maybe       (catMaybes)
import           Data.SBV
import           DataDefs
import qualified Text.Show.Pretty as Pr
import           Types


boolESat :: BoolE -> IO Bool
boolESat b = do
    SatResult result <- boolESatResult b
    case result of
        -- Overapproximation as per section 6.1 -- unless we can prove it unsatisfiable, we must
        -- assume it may match
        Unsatisfiable _ -> return False
        _ -> return True

boolESatResult :: BoolE -> IO SatResult
boolESatResult b = sat $ buildSBool b

buildSBool :: BoolE -> Symbolic SBool
buildSBool (LitBool True)  = return true
buildSBool (LitBool False) = return false
buildSBool Otherwise = return true
buildSBool (BoolVar v) = sBool v
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

buildSInt :: IntE -> Symbolic SInteger
buildSInt (IntLit i) = return $ literal i
buildSInt (IntVar v) = sInteger v
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
