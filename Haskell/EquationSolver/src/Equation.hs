module Equation where

import Data.Set
import Data.List

data Atom = Const Double
          | PI
          | E
          | Var Char
          deriving (Eq)

data Expr = EAtom Atom
          | Neg Expr
          | Prod Expr Expr
          | Quot Expr Expr
          | Sum Expr Expr
          | Diff Expr Expr
          | Pow Expr Int -- for now only support integer exponents
          | Paren Expr
          deriving (Eq)

data Dividend = DAtom Atom
              | DNeg Atom
              | DProd Dividend Dividend
              | DPow Atom Int
              | DParen Dividend
          deriving (Eq)

data Term = TSimple Dividend
          | TQuot Dividend Dividend
          deriving (Eq)

-- | Simplified expression (almost irreducible)  
--   List represents a sum of terms

newtype StdExpr = StdExpr [Term]

data Eqn = Eqn Expr Expr

data Solution = SolSet (Set StdExpr)
              | NoSol
              | AllReals


instance Show Atom where
    show (Const d) = show d
    show PI = "(PI)"
    show E = "e"
    show (Var c) = [c]

instance Show Expr where
    show (EAtom a) = show a
    show (Neg e) = '-':show e
    show (Prod left right) = concat [show left, "*", show right]
    show (Quot left right) = concat [show left, "/", show right]
    show (Sum left right) = concat [show left, "+", show right]
    show (Diff left right) = concat [show left, "-", show right]
    show (Pow base power) = concat [show base, "^", show power]
    show (Paren e) = concat ["(", show e, ")"]

instance Show Dividend where
    show (DAtom a) = show a
    show (DNeg a) = '-':show a
    show (DProd left right) = concat [show left, "*", show right]
    show (DPow base power) = concat [show base, "^", show power]
    show (DParen e) = concat ["(", show e, ")"]

instance Show Term where
    show (TSimple t) = show t
    show (TQuot num den) = concat [show num, "/", show den]

instance Show StdExpr where
    show (StdExpr terms) = intersperse '+' (terms >>= show)