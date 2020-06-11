module Equation where

data Atom = Const Double
          | PI
          | E
          | Var Char

data Expr = EAtom Atom
          | Prod Expr Expr
          | Quot Expr Expr
          | Sum Expr Expr
          | Diff Expr Expr
          | Pow Expr Rational -- for now only support rational exponents
          | Abs Expr
          | Paren Expr

data Term = TAtom Atom
          | TProd Term Term -- shouldn't allow double * double but whatever
          | TPow Term Rational
          | TParen Term

type StdExpr = [Term]

data Eqn = Eqn Expr Expr