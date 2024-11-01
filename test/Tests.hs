{-# LANGUAGE StandaloneDeriving #-}
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, Assertion)
import IC.Approx ((~~>))
import IC.Exact ((-->))

import Expr
import Calculus

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "calculus"
  [ testGroup "eval" (numberedTests evalTests)
  , testGroup "diff" (numberedTests diffTests)
  , testGroup "maclaurin" (numberedTests maclaurinTests)
  ]

evalTests :: [Assertion]
evalTests = [ eval (Val 7)  [("x",380)] ~~> 7.0
            , eval (Id "a") [("x",380), ("a",42), ("t",10)] ~~> 42.0
            , eval (Add (Val (-5)) (Id "t'")) [("t",10), ("t'",18)] ~~> 13.0
            , eval (Neg (Add (Val (-5)) (Id "t'"))) [("t",10), ("t'",19)]
                ~~> -14.0
            , eval (Mul (Id "x") (Id "x")) [("t",10), ("t'",18.6), ("x",-55)]
                ~~> 3025.0
            , eval (Div (Val 3) (Id "z")) [("z",7)] ~~> 0.42857142857142855
            , eval (Neg (Id "x")) [("x",0.37)] ~~> -0.37
            , eval (Sin (Val 2.4)) [] ~~> 0.675463180551151
            , eval (Cos (Val 2.4)) [] ~~> -0.7373937155412454
            , eval e1 [("x",0.37)] ~~> 1.85
            , eval e2 [("x",0.37), ("y", 8.2)] ~~> 1.3369
            , eval e3 [("x",0.37), ("y", 2.0)] ~~> 4.216153846153846
            , eval e4 [("x",0.37)] ~~> (-0.9323273456060345)
            , eval e5 [("x",0.37)] ~~> 0.6433720724587564
            , eval e6 [("x",0.37)] ~~> 0.8799171617597958
            , eval (Mul (Val 7) (Val 5)) [("x",123)] ~~> 35.0 -- 5*7 = 35
            ]

diffTests :: [Assertion]
diffTests = [ diff e1 "x" --> Add (Mul (Val 5.0) (Val 1.0))                       --these all wouldn't work I don't know why
                                  (Mul (Val 0.0) (Id "x"))
            , diff e2 "x" --> Add (Add (Add (Mul (Id "x") (Val 1.0))
                                            (Mul (Val 1.0) (Id "x")))
                                       (Val 0.0))
                                  (Neg (Val 0.0))
            , diff e2 "y" --> Add (Add (Add (Mul (Id "x") (Val 0.0))
                                            (Mul (Val 0.0) (Id "x")))
                                       (Val 1.0))
                                  (Neg (Val 0.0))
            , diff e4 "x" --> Neg (Neg (Mul (Sin (Id "x")) (Val 1.0)))
            , diff e5 "x" --> Mul (Cos (Add (Val 1.0)
                                            (Log (Mul (Val 2.0) (Id "x")))))
                                  (Add (Val 0.0)
                                       (Div (Add (Mul (Val 2.0) (Val 1.0))
                                                 (Mul (Val 0.0) (Id "x")))
                                            (Mul (Val 2.0) (Id "x"))))
            , diff e6 "x" --> Div (Add (Add (Mul (Val 3.0)
                                                 (Add (Mul (Id "x") (Val 1.0))
                                                      (Mul (Val 1.0) (Id "x"))))
                                            (Mul (Val 0.0)
                                                 (Mul (Id "x") (Id "x"))))
                                       (Val 0.0))
                                  (Add (Mul (Val 3.0) (Mul (Id "x") (Id "x")))
                                       (Val 2.0))
            ]

maclaurinTests :: [Assertion]
maclaurinTests = [ maclaurin (Sin (Id "x")) 2 2 ~~> 2.0
                 , maclaurin (Sin (Id "x")) 2 3 ~~> 2.0
                 , maclaurin (Sin (Id "x")) 2 5 ~~> 0.6666666666666667
                 , maclaurin (Sin (Id "x")) 2 7 ~~> 0.9333333333333333
                 , maclaurin (Sin (Id "x")) 2 9 ~~> 0.9079365079365079
                 , maclaurin (Cos (Id "x")) 4 9 ~~> -0.39682539682539764
                 , maclaurin (Sin (Id "x")) 2 0 ~~> 0.0 --0th maclaurin series =0
                 ]

-------------------------------------------------------------------------------
-- HELPERS

-- Why is this here? we _really_ want you to be using pattern matching at this
-- point.
deriving instance Eq Expr

{-|
This function just matches up a bunch of assertions to a numerical naming
system, allowing us to distinguish them.

If we wanted, we could provide descriptions to them instead...
-}
numberedTests :: [Assertion] -> [TestTree]
numberedTests = zipWith (\n -> testCase ("#" ++ show n)) ([1..] :: [Integer])
