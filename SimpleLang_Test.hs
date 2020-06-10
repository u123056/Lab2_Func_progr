{-# LANGUAGE OverloadedStrings #-}

module Lab2_Test where

import EasyTest

import SimpleLang
import Poly

allTests = tests [
    scope "applyPoly" $ expectAll [
        applyPoly (P [1, 2, 3]) 2 == 17
        , applyPoly (P [1, 2, 3, 0, 0]) 2 == 17
        , applyPoly (P []) 2 == 0
        , applyPoly (P [0, 0, 0]) 2 == 0
        , applyPoly (P [5]) 2 == 5
    ]
    , scope "PolyEqual" $ expectAll [
        P [1, 2, 3] == P [1, 2, 3]
        , P [1, 2, 3] == P [1, 2, 3, 0, 0]
        , P [1, 2, 3, 0, 0] == P [1, 2, 3]
        , P [1, 2, 3, 0, 0] /= P [1, 2, 3, 0, 0, 1]
        , P [] == P [0, 0, 0]
        , P [] == P []
    ]
    , scope "showPoly" $ expectAll [
        show (P []) == "0"
        , show (P [1]) == "1"
        , show (P [-1.0]) == "-1.0"
        , show (P [0.0, 0.0, 0.0]) == "0"
        , show (P [-5, 2, 3]) == "3 * x^2 + 2 * x - 5"
        , show (P [1, 0, -3, 0]) == "-3 * x^2 + 1"
        , show (P [-1, 2]) == "2 * x - 1"
        , show (P [-1, -2, 0]) == "-2 * x - 1"
        , show (P [-1, 1]) == "x - 1"
        , show (P [1, 1]) == "x + 1"
        , show (P [1, 1, -1]) == "-x^2 + x + 1"
    ]
    , scope "plus" $ expectAll [
        plus (P [0, 1]) (P [1, 2, 3]) == P [1, 3, 3]
        , plus (P [0, 1, 0]) (P [0, -1]) == P []
    ]
    , scope "times" $ expectAll [
        times (P [3, 1, 2]) (P [7, 5]) == P [21, 22, 19, 10]
        , times (P []) (P [1, 2]) == P []
        , times (P [1, 2]) (P []) == P []
        , times (P [1, 2, 0, 1]) (P [0, 2, 0]) == P [0, 2, 4, 0, 2]
    ]
    , scope "negate" $ expectAll [
        negate (P [-1, 2, -3]) == P [1, -2, 3]
    ]
    , scope "fromInteger" $ expectAll [
        -3.0 * x * x + 1.0 == P [1.0, 0, -3.0]
        , P [3.5] + 4 == P [7.5]
        , P [3] + 4.5 == P [7.5]
    ]
    , scope "deriv" $ expectAll [
        deriv (P [1]) == P []
        , deriv (P []) == P []
        , deriv (P [10, 3, 0, 5]) == P [3, 0, 15]
    ]
    , scope "nderiv" $ expectAll [
        nderiv 0 (P [1, 2, 3]) == P [1, 2, 3]
        , nderiv 1 (P [1, 2, 3]) == P [2, 6]
        , nderiv 2 (P [1, 2, 3]) == P [6]
        , nderiv 3 (P [1, 2, 3]) == P []
    ]
    , scope "state" $ expectAll [
        empty "xdf" == 0
        , (let state1 = extend empty "a" 1 in (state1 "a", state1 "b")) == (1, 0)
    ]
    , scope "eval" $ expectAll [
        eval empty (Val 5) == 5
        , eval (extend empty "a" 5) (Op (Val 2) Plus (Var "a")) == 7
        , eval (extend empty "a" 5) (Op (Val 2) Minus (Var "a")) == -3
        , eval (extend empty "a" 5) (Op (Val 2) Times (Var "a")) == 10
        , eval (extend empty "a" 5) (Op (Val 12) Divide (Var "a")) == 2
        , eval (extend empty "a" 5) (Op (Val 5) Gt (Var "a")) == 0
        , eval (extend empty "a" 5) (Op (Val 5) Ge (Var "a")) == 1
        , eval (extend empty "a" 5) (Op (Val 12) Lt (Var "a")) == 0
        , eval (extend empty "a" 5) (Op (Val 5) Le (Var "a")) == 1
        , eval (extend empty "a" 1) (Op (Val 1) Eql (Var "a")) == 1

        , (let extA = extend empty "a" 5
               extB = extend empty "b" 7
               twoArg arg = case arg of
                            "a" -> extA arg
                            "b" -> extB arg
           in eval twoArg (Op (Var "b") Plus (Var "a"))) == 12
    ]
    , scope "desugar" $ expectAll [
        desugar (Incr "A") == DAssign "A" (Op (Var "A") Plus (Val 1))
    ]
    , scope "run" $ expectAll [
        runSimpler empty (DAssign "A" (Val 10)) "A" == 10
        , SimpleLang.run (extend empty "A" 4) (Skip) "A" == 4
        , SimpleLang.run empty (Incr "A") "A" == 1
        , SimpleLang.run empty (Block [
                                       Incr "A"
                                       , Incr "A"
                                      ]
                                ) "A" == 2

        , SimpleLang.run (extend empty "A" 4) (If (Op (Var "A") Ge (Val 10))
                                                  (Assign "D" (Val 50))
                                                  (Assign "D" (Val 200))
                                               ) "D" == 200

        , SimpleLang.run (extend empty "A" 5) (While (Op (Var "A") Le (Val 12))
                                                     (Assign "A" (Op (Var "A") Plus (Val 3)))
                                               ) "A" == 14

        , SimpleLang.run empty (For (Assign "A" (Val 5))
                                    (Op (Var "A") Le (Val 12))
                                    (Assign "A" (Op (Var "A") Plus (Val 3)))
                                    Skip
                                ) "A" == 14

        , (let s = SimpleLang.run (extend empty "In" 4) factorial in s "Out") == 24
    ]
    , scope "factorial" $ expectAll [
        (SimpleLang.run (extend empty "In" 1) factorial) "Out" == 1
        , (SimpleLang.run (extend empty "In" 2) factorial) "Out" == 2
        , (SimpleLang.run (extend empty "In" 3) factorial) "Out" == 6
        , (SimpleLang.run (extend empty "In" 5) factorial) "Out" == 120
    ]
    , scope "squareRoot" $ expectAll [
        (SimpleLang.run (extend empty "A" 37) squareRoot) "B" == 6
        , (SimpleLang.run (extend empty "A" 36) squareRoot) "B" == 6
    ]
    , scope "fibonacci" $ expectAll [
        (SimpleLang.run (extend empty "In" 0) fibonacci) "Out" == 1
        , (SimpleLang.run (extend empty "In" 1) fibonacci) "Out" == 1
        , (SimpleLang.run (extend empty "In" 2) fibonacci) "Out" == 2
        , (SimpleLang.run (extend empty "In" 3) fibonacci) "Out" == 3
        , (SimpleLang.run (extend empty "In" 4) fibonacci) "Out" == 5
        , (SimpleLang.run (extend empty "In" 5) fibonacci) "Out" == 8
    ]
 ]

main = EasyTest.run allTests
