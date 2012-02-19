module GLPK where

import Foreign
import Foreign.C
import Control.Monad
import GLPK.Internal

newtype Problem = Problem (ForeignPtr GLP_PROB)

data Dir = Min | Max
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data VarType = FR | LO | UP | DB | FX
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

type SMCP = ForeignPtr GLP_SMCP

encodeVarType :: VarType -> CInt
encodeVarType FR = gLP_FR
encodeVarType LO = gLP_LO
encodeVarType UP = gLP_UP
encodeVarType DB = gLP_DB
encodeVarType FX = gLP_FX

createProblem :: IO Problem
createProblem =
    liftM Problem . newForeignPtr glp_delete_prob'  =<< glp_create_prob

setProbName :: Problem -> String -> IO ()
setProbName (Problem prob) str =
  withForeignPtr prob $ \p -> 
    withCString str $ \pstr -> do
      glp_set_prob_name p pstr

setObjDir :: Problem -> Dir -> IO ()
setObjDir (Problem prob) dir =
  withForeignPtr prob $ \p -> do
    let dir2 = case dir of
                 Min -> gLP_MIN
                 Max -> gLP_MAX
    glp_set_obj_dir p dir2

-- | add new rows to problem object
addRows :: Problem -> Int -> IO Int
addRows (Problem prob) n =
  withForeignPtr prob $ \p ->
    liftM fromIntegral $ glp_add_rows p (fromIntegral n)

-- | add new columns to problem object
addCols :: Problem -> Int -> IO Int
addCols (Problem prob) n =
  withForeignPtr prob $ \p ->
    liftM fromIntegral $ glp_add_cols p (fromIntegral n)

setRowName :: Problem -> Int -> String -> IO ()
setRowName (Problem prob) i name =
  withForeignPtr prob $ \p -> 
    withCString name $ \pname -> do
      glp_set_row_name p (fromIntegral i) pname

setColName :: Problem -> Int -> String -> IO ()
setColName (Problem prob) i name =
  withForeignPtr prob $ \p -> 
    withCString name $ \pname -> do
      glp_set_col_name p (fromIntegral i) pname

setRowBnds :: Problem -> Int -> VarType -> Double -> Double -> IO ()
setRowBnds(Problem prob) i typ lb ub =
  withForeignPtr prob $ \p -> 
    glp_set_row_bnds p (fromIntegral i) (encodeVarType typ) (realToFrac lb) (realToFrac ub)

setColBnds :: Problem -> Int -> VarType -> Double -> Double -> IO ()
setColBnds (Problem prob) j typ lb ub =
  withForeignPtr prob $ \p -> 
    glp_set_col_bnds p (fromIntegral j) (encodeVarType typ) (realToFrac lb) (realToFrac ub)

setObjCoef :: Problem -> Int -> Double -> IO ()
setObjCoef (Problem prob) j coef =
  withForeignPtr prob $ \p -> 
    glp_set_obj_coef p (fromIntegral j) (realToFrac coef)

-- | load (replace) the whole constraint matrix
loadMatrix :: Problem -> [((Int,Int),Double)] -> IO ()
loadMatrix (Problem prob) m =
  withForeignPtr prob $ \p ->  do
    let n = length m
    let is = 0:[fromIntegral i | ((i,_),_) <- m]
    let js = 0:[fromIntegral j | ((_,j),_) <- m]
    let vs = 0:[realToFrac v | (_,v) <- m]
    withArray is $ \is2 ->
      withArray js $ \js2 ->
        withArray vs $ \vs2 ->
          glp_load_matrix p (fromIntegral n) is2 js2 vs2

-- | solve LP problem with the simplex method
simplex :: Problem -> SMCP -> IO Int
simplex (Problem prob) smcp =
  withForeignPtr prob $ \p ->
    withForeignPtr smcp $ \q -> 
      liftM fromIntegral $ glp_simplex p q

-- | solve LP problem in exact arithmetic
exact :: Problem -> SMCP -> IO Int
exact = undefined

newSMCP :: IO SMCP
newSMCP = do
  fp <- mallocForeignPtrBytes sizeof_GLP_SMCP
  withForeignPtr fp glp_init_smcp    
  return fp

getObjVal :: Problem -> IO Double
getObjVal (Problem prob) =
  withForeignPtr prob $ \p ->  do
    val <- glp_get_obj_val p
    return (realToFrac val)

getColPrim :: Problem -> Int -> IO Double
getColPrim (Problem prob) j =
  withForeignPtr prob $ \p ->  do
    val <- glp_get_col_prim p (fromIntegral j)
    return (realToFrac val)
