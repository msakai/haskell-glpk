{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}
module GLPK.Internal where

import Foreign
import Foreign.C

#include <glpk.h>

-- | library version numbers:

gLP_MAJOR_VERSION = #const GLP_MAJOR_VERSION
gLP_MINOR_VERSION = #const GLP_MINOR_VERSION

-- | LP/MIP problem object
data GLP_PROB

-- optimization direction flag:
-- | minimization
gLP_MIN :: CInt
gLP_MIN = #const GLP_MIN
 -- | maximization
gLP_MAX :: CInt
gLP_MAX = #const GLP_MAX

-- kind of structural variable:
-- | continuous variable
gLP_CV = #const GLP_CV
-- | integer variable
gLP_IV = #const GLP_IV
-- | binary variable
gLP_BV = #const GLP_BV

-- type of auxiliary/structural variable:
-- | free variable
gLP_FR :: CInt
gLP_FR = #const GLP_FR
-- | variable with lower bound
gLP_LO :: CInt
gLP_LO = #const GLP_LO
-- | variable with upper bound
gLP_UP :: CInt
gLP_UP = #const GLP_UP 
-- | double-bounded variable
gLP_DB :: CInt
gLP_DB = #const GLP_DB 
-- | fixed variable
gLP_FX :: CInt
gLP_FX = #const GLP_FX 

-- status of auxiliary/structural variable:
-- | basic variable
gLP_BS = #const GLP_BS
-- | non-basic variable on lower bound
gLP_NL = #const GLP_NL
-- | non-basic variable on upper bound
gLP_NU = #const GLP_NU
-- | non-basic free variable
gLP_NF = #const GLP_NF
-- | non-basic fixed variable
gLP_NS = #const GLP_NS

-- scaling options:
-- | perform geometric mean scaling
gLP_SF_GM   = #const GLP_SF_GM
-- | perform equilibration scaling
gLP_SF_EQ   = #const GLP_SF_EQ
-- | round scale factors to power of two
gLP_SF_2N   = #const GLP_SF_2N
-- | skip if problem is well scaled
gLP_SF_SKIP = #const GLP_SF_SKIP
-- | choose scaling options automatically
gLP_SF_AUTO = #const GLP_SF_AUTO

-- solution indicator:
-- | basic solution
gLP_SOL = #const GLP_SOL
-- | interior-point solution
gLP_IPT = #const GLP_IPT
-- | mixed integer solution
gLP_MIP = #const GLP_MIP

-- solution status:
-- | solution is undefined
gLP_UNDEF  = #const GLP_UNDEF
-- | solution is feasible
gLP_FEAS   = #const GLP_FEAS
-- | solution is infeasible
gLP_INFEAS = #const GLP_INFEAS
-- | no feasible solution exists
gLP_NOFEAS = #const GLP_NOFEAS
-- | solution is optimal
gLP_OPT    = #const GLP_OPT
-- | solution is unbounded
gLP_UNBND  = #const GLP_UNBND

-- | basis factorization control parameters
data GLP_BFCP

-- | simplex method control parameters
data GLP_SMCP

-- | INTERIOR-point solver control parameters
data GLP_IPTCP

-- | branch-and-bound tree
data GLP_TREE

-- | integer optimizer control parameters
data GLP_IOCP

-- | additional row attributes
data GLP_ATTR

-- /* enable/disable flag: */
-- #define GLP_ON             1  /* enable something */
-- #define GLP_OFF            0  /* disable something */
-- 
-- /* reason codes: */
-- #define GLP_IROWGEN     0x01  /* request for row generation */
-- #define GLP_IBINGO      0x02  /* better integer solution found */
-- #define GLP_IHEUR       0x03  /* request for heuristic solution */
-- #define GLP_ICUTGEN     0x04  /* request for cut generation */
-- #define GLP_IBRANCH     0x05  /* request for branching */
-- #define GLP_ISELECT     0x06  /* request for subproblem selection */
-- #define GLP_IPREPRO     0x07  /* request for preprocessing */
-- 
-- /* branch selection indicator: */
-- #define GLP_NO_BRNCH       0  /* select no branch */
-- #define GLP_DN_BRNCH       1  /* select down-branch */
-- #define GLP_UP_BRNCH       2  /* select up-branch */
-- 
-- /* return codes: */
-- #define GLP_EBADB       0x01  /* invalid basis */
-- #define GLP_ESING       0x02  /* singular matrix */
-- #define GLP_ECOND       0x03  /* ill-conditioned matrix */
-- #define GLP_EBOUND      0x04  /* invalid bounds */
-- #define GLP_EFAIL       0x05  /* solver failed */
-- #define GLP_EOBJLL      0x06  /* objective lower limit reached */
-- #define GLP_EOBJUL      0x07  /* objective upper limit reached */
-- #define GLP_EITLIM      0x08  /* iteration limit exceeded */
-- #define GLP_ETMLIM      0x09  /* time limit exceeded */
-- #define GLP_ENOPFS      0x0A  /* no primal feasible solution */
-- #define GLP_ENODFS      0x0B  /* no dual feasible solution */
-- #define GLP_EROOT       0x0C  /* root LP optimum not provided */
-- #define GLP_ESTOP       0x0D  /* search terminated by application */
-- #define GLP_EMIPGAP     0x0E  /* relative mip gap tolerance reached */
-- #define GLP_ENOFEAS     0x0F  /* no primal/dual feasible solution */
-- #define GLP_ENOCVG      0x10  /* no convergence */
-- #define GLP_EINSTAB     0x11  /* numerical instability */
-- #define GLP_EDATA       0x12  /* invalid data */
-- #define GLP_ERANGE      0x13  /* result out of range */
-- 
-- /* condition indicator: */
-- #define GLP_KKT_PE         1  /* primal equalities */
-- #define GLP_KKT_PB         2  /* primal bounds */
-- #define GLP_KKT_DE         3  /* dual equalities */
-- #define GLP_KKT_DB         4  /* dual bounds */
-- #define GLP_KKT_CS         5  /* complementary slackness */
-- 
-- /* MPS file format: */
-- #define GLP_MPS_DECK       1  /* fixed (ancient) */
-- #define GLP_MPS_FILE       2  /* free (modern) */

-- | MPS format control parameters
data GLP_MPSCP

-- | CPLEX LP format control parameters
data GLP_CPXCP

-- | MathProg translator workspace
data GLP_TRAN

-- | create problem object
foreign import ccall glp_create_prob
    :: IO (Ptr GLP_PROB)

-- | assign (change) problem name
foreign import ccall glp_set_prob_name
    :: Ptr GLP_PROB -> CString -> IO ()

-- | assign (change) objective function name
foreign import ccall glp_set_obj_name
    :: Ptr GLP_PROB -> CString -> IO ()

-- | set (change) optimization direction flag
foreign import ccall glp_set_obj_dir
    :: Ptr GLP_PROB -> CInt -> IO ()

-- | add new rows to problem object
foreign import ccall glp_add_rows
    :: Ptr GLP_PROB -> CInt -> IO CInt

-- | add new columns to problem object
foreign import ccall glp_add_cols
    :: Ptr GLP_PROB -> CInt -> IO CInt

-- | assign (change) row name
foreign import ccall glp_set_row_name
    :: Ptr GLP_PROB -> CInt -> CString -> IO ()

-- | assign (change) column name
foreign import ccall glp_set_col_name
    :: Ptr GLP_PROB -> CInt -> CString -> IO ()

-- | set (change) row bounds
foreign import ccall glp_set_row_bnds
    :: Ptr GLP_PROB -> CInt -> CInt -> CDouble -> CDouble -> IO ()

-- | set (change) column bounds
foreign import ccall glp_set_col_bnds
    :: Ptr GLP_PROB -> CInt -> CInt -> CDouble -> CDouble -> IO ()

-- | set (change) obj. coefficient or constant term
foreign import ccall glp_set_obj_coef
    :: Ptr GLP_PROB -> CInt -> CDouble -> IO ()

{-
void glp_set_mat_row(glp_prob *P, int i, int len, const int ind[],
      const double val[]);
/* set (replace) row of the constraint matrix */

void glp_set_mat_col(glp_prob *P, int j, int len, const int ind[],
      const double val[]);
/* set (replace) column of the constraint matrix */
-}

-- | load (replace) the whole constraint matrix
foreign import ccall glp_load_matrix
    :: Ptr GLP_PROB -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()

{-
int glp_check_dup(int m, int n, int ne, const int ia[], const int ja[]);
/* check for duplicate elements in sparse matrix */

void glp_sort_matrix(glp_prob *P);
/* sort elements of the constraint matrix */

void glp_del_rows(glp_prob *P, int nrs, const int num[]);
/* delete specified rows from problem object */

void glp_del_cols(glp_prob *P, int ncs, const int num[]);
/* delete specified columns from problem object */

void glp_copy_prob(glp_prob *dest, glp_prob *prob, int names);
/* copy problem object content */

void glp_erase_prob(glp_prob *P);
/* erase problem object content */
-}

-- | delete problem object
foreign import ccall glp_delete_prob
    :: Ptr GLP_PROB -> IO ()

foreign import ccall "&glp_delete_prob" glp_delete_prob'
    :: FunPtr (Ptr GLP_PROB -> IO ())

