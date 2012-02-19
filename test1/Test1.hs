module Main where

import Text.Printf
import GLPK

main :: IO ()
main = do
  lp <- createProblem
  setProbName lp "sample"
  setObjDir lp Max

  addRows lp 3
  setRowName lp 1 "p"
  setRowBnds lp 1 UP 0 100
  setRowName lp 2 "q"
  setRowBnds lp 2 UP 0 600
  setRowName lp 3 "r"
  setRowBnds lp 3 UP 0 300

  addCols lp 3
  setColName lp 1 "x1"
  setColBnds lp 1 LO 0 0
  setObjCoef lp 1 10
  setColName lp 2 "x2"
  setColBnds lp 2 LO 0 0
  setObjCoef lp 2 6
  setColName lp 3 "x3"
  setColBnds lp 3 LO 0 0
  setObjCoef lp 3 4

  loadMatrix lp    
    [ ((1,1),1)
    , ((1,2),1) 
    , ((1,3),1) 
    , ((2,1),10) 
    , ((3,1),2) 
    , ((2,2),4) 
    , ((3,2),2) 
    , ((2,3),5) 
    , ((3,3),6)
    ]

  smcp <- newSMCP
  simplex lp smcp

  z <- getObjVal lp
  x1 <- getColPrim lp 1
  x2 <- getColPrim lp 2
  x3 <- getColPrim lp 3

  printf "z = %g; x1 = %g; x2 = %g; x3 = %g\n"
    z x1 x2 x3

  return ()
