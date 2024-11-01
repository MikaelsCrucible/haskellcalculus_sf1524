{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
module Calculus (lookUp, eval, showExpr, diff, maclaurin) where

import Vars
import Expr

import Data.Maybe
import GHC.Float (Floating(expm1))

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

-- Comment this out if you want to implement your own instance in terms
-- of `showExpr`
--deriving instance Show Expr
instance Show Expr where
  show = showExpr

-- strangely after I reduced the redudancies I couldn't pass the test and keeps getting something like 
{- #2:  FAIL
      test\Tests.hs:43:
      expected: ((((x*1.0)+(1.0*x))+0.0)+-(0.0))
       but got: (x+x) -}
--But appearantly ((((x*1.0)+(1.0*x))+0.0)+-(0.0)) = x+x
--I'm not gonna change my code back to pass the test
--Why would this happen anyway? I have no idea

instance Num Expr where
  fromInteger :: Integer -> Expr
  fromInteger n = Val (fromInteger n)
  negate (Val 0) = Val 0
  negate e = Neg e
  e + Val 0 = e
  Val 0 + e = e
  e1 + e2 = Add e1 e2

  e * Val 1 = e
  Val 1 * e = e
  e * Val 0 = Val 0
  Val 0 * e = Val 0
  e1 * e2 = Mul e1 e2


instance Fractional Expr where
  fromRational n = Val (fromRational n)
  Val 0 / e = Val 0
  e / Val 1 = e
  e1 / e2 = Div e1 e2

instance Floating Expr where
  sin = Sin
  cos = Cos
  log = Log

---------------------------------------------------------------------------

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp key list = fromJust (lookup key list)

{-|
Evaluates a given expression, evaluating any variables to their value within
the provided environment.
-}
eval :: Expr -> Env -> Double
eval (Val x) _ = x
eval (Id x) env = lookUp x env
eval (Add e1 e2) env = eval e1 env + eval e2 env
eval (Mul e1 e2) env = eval e1 env * eval e2 env
eval (Div e1 e2) env = eval e1 env / eval e2 env
eval (Neg e) env = - eval e env
eval (Sin e) env = sin (eval e env)
eval (Cos e) env = cos (eval e env)
eval (Log e) env = log (eval e env)

{-| OPTIONAL
Pretty prints an expression to a more human-readable form.
-}
showExpr :: Expr -> String
showExpr (Val x) = show x
showExpr (Id x) = x
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++ ")"
showExpr (Mul e1 e2) = "(" ++ showExpr e1 ++ "*" ++ showExpr e2 ++ ")"
showExpr (Div e1 e2) = "(" ++ showExpr e1 ++ "/" ++ showExpr e2 ++ ")"
showExpr (Neg e) = "-(" ++ showExpr e ++ ")"
showExpr (Sin e) = "sin(" ++ showExpr e ++ ")"
showExpr (Cos e) = "cos(" ++ showExpr e ++ ")"
showExpr (Log e) = "log(" ++ showExpr e ++ ")"

{-|
Symbolically differentiates a term with respect to a given identifier.
-}
diff :: Expr -> String -> Expr
diff (Val x) _ = 0.0
diff (Id x) var
  | x == var = 1.0
  | otherwise = 0.0        
diff (Add e1 e2) var = diff e1 var + diff e2 var
diff (Mul e1 e2) var = diff e1 var * e2 + e1 * diff e2 var
diff (Div e1 e2) var = ((diff e1 var * e2) - (e1 * diff e2 var)) / Mul e2 e2
diff (Neg e) var = -diff e var
diff (Sin e) var = cos e * diff e var
diff (Cos e) var = (-sin e) * diff e var
diff (Log e) var = diff e var / e
{-diff (Val x) _ = Val 0.0
diff (Id x) var
  | x == var = Val 1.0
  | otherwise = Val 0.0        
diff (Add e1 e2) var = Add (diff e1 var) (diff e2 var)
diff (Mul e1 e2) var = Add (Mul (diff e1 var) e2) (Mul e1 (diff e2 var))
diff (Div e1 e2) var = Div (Add (Mul (diff e1 var) e2) (Neg (Mul e1 (diff e2 var)))) (Mul e2 e2)
diff (Neg e) var = Neg (diff e var)
diff (Sin e) var = Mul (Cos e) (diff e var)
diff (Cos e) var = Mul (Neg (Sin e)) (diff e var)
diff (Log e) var = Div (diff e var) e-}

{-|
Computes the approximation of an expression `f` by expanding the Maclaurin
series on `f` and taking its summation.
-}
maclaurin :: Expr   -- ^ expression to approximate (with `x` free)
          -> Double -- ^ value to give to `x`
          -> Int    -- ^ number of terms to expand
          -> Double -- ^ the approximate result
maclaurin expr x n = sum(zipWith3 calc (take n deri) fact [0..n-1]) -- not sure why zipWith is metioned instead in the spec,x^i where i=0 to n also needs to be calculated aside from i-th deriviative and i!
                                                                       -- so in my opinion zipWith3 could be a more straightforward method as we calculate each term and sum them up
  where
    deri :: [Expr] -- expr, diff expr, diff diff expr ......
    deri = iterate (`diff` "x") expr 

    fact :: [Int] -- 1, 1*1, 1*1*2, 1*1*2*3 ......
    fact = scanl (*) 1 [1..]

    calc :: Expr -> Int -> Int -> Double
    calc d f n = eval d [("x", 0)] * (x ^ n) / fromIntegral f -- d = n-th deriviative  f = n! n = n-th term

