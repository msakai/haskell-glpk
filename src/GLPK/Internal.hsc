{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}
module GLPK.Internal where

import Foreign
import Foreign.C

#include <glpk.h>

-- library version numbers:
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

sizeof_GLP_SMCP :: Int
sizeof_GLP_SMCP = #size glp_smcp

-- | INTERIOR-point solver control parameters
data GLP_IPTCP

-- | branch-and-bound tree
data GLP_TREE

-- | integer optimizer control parameters
data GLP_IOCP

-- | additional row attributes
data GLP_ATTR

-- enable/disable flag:
-- | enable something
gLP_ON = #const GLP_ON
-- | disable something
gLP_OFF = #const GLP_OFF

-- reason codes:
-- | request for row generation
gLP_IROWGEN = #const GLP_IROWGEN
-- | better integer solution found
gLP_IBINGO  = #const GLP_IBINGO
-- | request for heuristic solution
gLP_IHEUR   = #const GLP_IHEUR
-- | request for cut generation
gLP_ICUTGEN = #const GLP_ICUTGEN
-- | request for branching
gLP_IBRANCH = #const GLP_IBRANCH
-- | request for subproblem selection
gLP_ISELECT = #const GLP_ISELECT
-- | request for preprocessing
gLP_IPREPRO = #const GLP_IPREPRO

-- branch selection indicator:
-- | select no branch
gLP_NO_BRNCH = #const GLP_NO_BRNCH
-- | select down-branch
gLP_DN_BRNCH = #const GLP_DN_BRNCH
-- | select up-branch
gLP_UP_BRNCH = #const GLP_UP_BRNCH

-- return codes:
-- | invalid basis
gLP_EBADB   = #const GLP_EBADB
-- | singular matrix
gLP_ESING   = #const GLP_ESING
-- | ill-conditioned matrix
gLP_ECOND   = #const GLP_ECOND
-- | invalid bounds
gLP_EBOUND  = #const GLP_EBOUND
-- | solver failed
gLP_EFAIL   = #const GLP_EFAIL
-- | objective lower limit reached
gLP_EOBJLL  = #const GLP_EOBJLL
-- | objective upper limit reached
gLP_EOBJUL  = #const GLP_EOBJUL
-- | iteration limit exceeded
gLP_EITLIM  = #const GLP_EITLIM
-- | time limit exceeded
gLP_ETMLIM  = #const GLP_ETMLIM
-- | no primal feasible solution
gLP_ENOPFS  = #const GLP_ENOPFS
-- | no dual feasible solution
gLP_ENODFS  = #const GLP_ENODFS
-- | root LP optimum not provided
gLP_EROOT   = #const GLP_EROOT
-- | search terminated by application
gLP_ESTOP   = #const GLP_ESTOP
-- | relative mip gap tolerance reached
gLP_EMIPGAP = #const GLP_EMIPGAP
-- | no primal/dual feasible solution
gLP_ENOFEAS = #const GLP_ENOFEAS
-- | no convergence
gLP_ENOCVG  = #const GLP_ENOCVG
-- | numerical instability
gLP_EINSTAB = #const GLP_EINSTAB
-- | invalid data
gLP_EDATA   = #const GLP_EDATA
-- | result out of range
gLP_ERANGE  = #const GLP_ERANGE

-- condition indicator:
-- | primal equalities
gLP_KKT_PE = #const GLP_KKT_PE
-- | primal bounds
gLP_KKT_PB = #const GLP_KKT_PB
-- | dual equalities
gLP_KKT_DE = #const GLP_KKT_DE
-- | dual bounds
gLP_KKT_DB = #const GLP_KKT_DB
-- | complementary slackness
gLP_KKT_CS = #const GLP_KKT_CS

-- MPS file format:
-- | fixed (ancient)
gLP_MPS_DECK = #const GLP_MPS_DECK
-- | free (modern)
gLP_MPS_FILE = #const GLP_MPS_FILE

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

-- | set (replace) row of the constraint matrix
foreign import ccall glp_set_mat_row
    :: Ptr GLP_PROB -> CInt -> CInt -> Ptr CInt -> Ptr CDouble -> IO ()

-- | set (replace) column of the constraint matrix
foreign import ccall glp_set_mat_col
    :: Ptr GLP_PROB -> CInt -> CInt -> Ptr CInt -> Ptr CDouble -> IO ()

-- | load (replace) the whole constraint matrix
foreign import ccall glp_load_matrix
    :: Ptr GLP_PROB -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()

-- | check for duplicate elements in sparse matrix
foreign import ccall glp_check_dup
    :: Ptr GLP_PROB -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- | sort elements of the constraint matrix
foreign import ccall glp_sort_matrix
    :: Ptr GLP_PROB -> IO ()

-- | delete specified rows from problem object
foreign import ccall glp_del_rows
    :: Ptr GLP_PROB -> CInt -> Ptr CInt -> IO ()

-- | delete specified columns from problem object
foreign import ccall glp_del_cols
    :: Ptr GLP_PROB -> CInt -> Ptr CInt -> IO ()

-- | copy problem object content
foreign import ccall glp_copy_prob
    :: Ptr GLP_PROB -> Ptr GLP_PROB -> CInt -> IO ()

-- | erase problem object content
foreign import ccall glp_erase_prob
    :: Ptr GLP_PROB -> IO ()

-- | delete problem object
foreign import ccall glp_delete_prob
    :: Ptr GLP_PROB -> IO ()

-- | delete problem object
foreign import ccall "&glp_delete_prob" glp_delete_prob'
    :: FunPtr (Ptr GLP_PROB -> IO ())

-- | retrieve problem name
foreign import ccall glp_get_prob_name
    :: Ptr GLP_PROB -> IO CString

-- | retrieve objective function name
foreign import ccall glp_get_obj_name
    :: Ptr GLP_PROB -> IO CString

-- | retrieve optimization direction flag
foreign import ccall glp_get_obj_dir
    :: Ptr GLP_PROB -> IO CInt

-- | retrieve number of rows
foreign import ccall glp_get_num_rows
    :: Ptr GLP_PROB -> IO CInt

-- | retrieve number of columns
foreign import ccall glp_get_num_cols
    :: Ptr GLP_PROB -> IO CInt

-- | retrieve row name
foreign import ccall glp_get_row_name
    :: Ptr GLP_PROB -> CInt -> IO CString

-- | retrieve column name
foreign import ccall glp_get_col_name
    :: Ptr GLP_PROB -> CInt -> IO CString

-- | retrieve row type
foreign import ccall glp_get_row_type
    :: Ptr GLP_PROB -> CInt -> IO CInt

-- | retrieve row lower bound
foreign import ccall glp_get_row_lb
    :: Ptr GLP_PROB -> CInt -> IO CDouble

-- | retrieve row upper bound
foreign import ccall glp_get_row_ub
    :: Ptr GLP_PROB -> CInt -> IO CDouble

-- | retrieve column type
foreign import ccall glp_get_col_type
    :: Ptr GLP_PROB -> CInt -> IO CInt

-- | retrieve column lower bound
foreign import ccall glp_get_col_lb
    :: Ptr GLP_PROB -> CInt -> IO CDouble

-- | retrieve column upper bound
foreign import ccall glp_get_col_ub
    :: Ptr GLP_PROB -> CInt -> IO CDouble

-- | retrieve obj. coefficient or constant term
foreign import ccall glp_get_obj_coef
    :: Ptr GLP_PROB -> CInt -> IO CDouble

-- | retrieve number of constraint coefficients
foreign import ccall glp_get_num_nz
    :: Ptr GLP_PROB -> IO CInt

-- | retrieve row of the constraint matrix
foreign import ccall glp_get_mat_row
    :: Ptr GLP_PROB -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- | retrieve column of the constraint matrix
foreign import ccall glp_get_mat_col
    :: Ptr GLP_PROB -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- | create the name index
foreign import ccall glp_create_index
    :: Ptr GLP_PROB -> IO ()

-- | find row by its name
foreign import ccall glp_find_row
    :: Ptr GLP_PROB -> CString -> IO ()

-- | find column by its name
foreign import ccall glp_find_col
    :: Ptr GLP_PROB -> CString -> IO ()

-- | delete the name index
foreign import ccall glp_delete_index
    :: Ptr GLP_PROB -> IO ()

-- | set (change) row scale factor
foreign import ccall glp_set_rii
    :: Ptr GLP_PROB -> CInt -> CDouble -> IO ()

-- | set (change) column scale factor
foreign import ccall glp_set_sjj
    :: Ptr GLP_PROB -> CInt -> CDouble -> IO ()

-- | retrieve row scale factor
foreign import ccall glp_get_rii
    :: Ptr GLP_PROB -> CInt -> IO CDouble

-- | retrieve column scale factor
foreign import ccall glp_get_sjj
    :: Ptr GLP_PROB -> CInt -> IO CDouble

-- | scale problem data
foreign import ccall glp_scale_prob
    :: Ptr GLP_PROB -> CInt -> IO ()

-- | unscale problem data
foreign import ccall glp_unscale_prob
    :: Ptr GLP_PROB -> IO ()

-- | set (change) row status
foreign import ccall glp_set_row_stat
    :: Ptr GLP_PROB -> CInt -> CInt -> IO ()

-- | set (change) column status
foreign import ccall glp_set_col_stat
    :: Ptr GLP_PROB -> CInt -> CInt -> IO ()

-- | construct standard initial LP basis
foreign import ccall glp_std_basis
    :: Ptr GLP_PROB -> IO ()

-- | construct advanced initial LP basis
foreign import ccall glp_adv_basis
    :: Ptr GLP_PROB -> IO ()

-- | construct Bixby's initial LP basis
foreign import ccall glp_cpx_basis
    :: Ptr GLP_PROB -> IO ()

-- | solve LP problem with the simplex method
foreign import ccall glp_simplex
    :: Ptr GLP_PROB -> Ptr GLP_SMCP -> IO CInt

-- | solve LP problem in exact arithmetic
foreign import ccall glp_exact
    :: Ptr GLP_PROB -> Ptr GLP_SMCP -> IO CInt

-- | initialize simplex method control parameters
foreign import ccall glp_init_smcp
    :: Ptr GLP_SMCP -> IO ()

-- | retrieve generic status of basic solution
foreign import ccall glp_get_status
    :: Ptr GLP_PROB -> IO CInt

-- | retrieve status of primal basic solution
foreign import ccall glp_get_prim_stat
    :: Ptr GLP_PROB -> IO CInt

-- | retrieve status of dual basic solution
foreign import ccall glp_get_dual_stat
    :: Ptr GLP_PROB -> IO CInt

-- | retrieve objective value (basic solution)
foreign import ccall glp_get_obj_val
    :: Ptr GLP_PROB -> IO CDouble

-- | retrieve row status
foreign import ccall glp_get_row_stat
     :: Ptr GLP_PROB -> CInt -> IO CInt

-- | retrieve row primal value (basic solution)
foreign import ccall glp_get_row_prim
     :: Ptr GLP_PROB -> CInt -> IO CDouble

-- | retrieve row dual value (basic solution)
foreign import ccall glp_get_row_dual
     :: Ptr GLP_PROB -> CInt -> IO CDouble

-- | retrieve column status
foreign import ccall glp_get_col_stat
     :: Ptr GLP_PROB -> CInt -> IO CInt

-- | retrieve column primal value (basic solution)
foreign import ccall glp_get_col_prim
     :: Ptr GLP_PROB -> CInt -> IO CDouble

-- | retrieve column dual value (basic solution)
foreign import ccall glp_get_col_dual
     :: Ptr GLP_PROB -> CInt -> IO CDouble

-- | determine variable causing unboundedness
foreign import ccall glp_get_unbnd_ray
     :: Ptr GLP_PROB -> IO CInt

-- | solve LP problem with the interior-point method
foreign import ccall glp_interior
     :: Ptr GLP_PROB -> Ptr GLP_IPTCP -> IO CInt

-- | initialize interior-point solver control parameters
foreign import ccall glp_init_iptcp
     :: Ptr GLP_IPTCP -> IO ()

-- | retrieve status of interior-point solution
foreign import ccall glp_ipt_status
     :: Ptr GLP_PROB -> IO CInt

-- | retrieve objective value (interior point)
foreign import ccall glp_ipt_obj_val
     :: Ptr GLP_PROB -> IO CDouble

-- | retrieve row primal value (interior point)
foreign import ccall glp_ipt_row_prim
     :: Ptr GLP_PROB -> CInt -> IO CDouble

-- | retrieve row dual value (interior point)
foreign import ccall glp_ipt_row_dual
     :: Ptr GLP_PROB -> CInt -> IO CDouble

-- | retrieve column primal value (interior point)
foreign import ccall glp_ipt_col_prim
     :: Ptr GLP_PROB -> CInt -> IO CDouble

-- | retrieve column dual value (interior point)
foreign import ccall glp_ipt_col_dual
     :: Ptr GLP_PROB -> CInt -> IO CDouble

-- | set (change) column kind
foreign import ccall glp_set_col_kind
     :: Ptr GLP_PROB -> CInt -> CInt -> IO ()

-- | retrieve column kind
foreign import ccall glp_get_col_kind
     :: Ptr GLP_PROB -> CInt -> IO CInt

-- | retrieve number of integer columns
foreign import ccall glp_get_num_int
     :: Ptr GLP_PROB -> IO CInt

-- | retrieve number of binary columns
foreign import ccall glp_get_num_bin
     :: Ptr GLP_PROB -> IO CInt

-- | solve MIP problem with the branch-and-bound method
foreign import ccall glp_intopt
     :: Ptr GLP_PROB -> Ptr GLP_IOCP -> IO CInt

-- | initialize integer optimizer control parameters
foreign import ccall glp_init_iocp
     :: Ptr GLP_IOCP -> IO ()

-- | retrieve status of MIP solution
foreign import ccall glp_mip_status
     :: Ptr GLP_PROB -> IO CInt

-- | retrieve objective value (MIP solution)
foreign import ccall glp_mip_obj_val
     :: Ptr GLP_PROB -> IO CDouble

-- | retrieve row value (MIP solution)
foreign import ccall glp_mip_row_val
     :: Ptr GLP_PROB -> CInt -> IO CDouble

-- | retrieve column value (MIP solution)
foreign import ccall glp_mip_col_val
     :: Ptr GLP_PROB -> CInt -> IO CDouble

-- | write basic solution in printable format
foreign import ccall glp_print_sol
     :: Ptr GLP_PROB -> CString -> IO CInt

-- | read basic solution from text file
foreign import ccall glp_read_sol
     :: Ptr GLP_PROB -> CString -> IO CInt

-- | write basic solution to text file
foreign import ccall glp_write_sol
     :: Ptr GLP_PROB -> CString -> IO CInt
