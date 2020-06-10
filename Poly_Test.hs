{-# LANGUAGE OverloadedStrings #-}

import EasyTest
import Poly

allTests = tests
    [ scope "applyPoly" $ tests
        [ expect $ applyPoly x 0 == 0
        , expect $ applyPoly x 5 == 5
        , expect $ applyPoly 10 1 == 10
        ]
    ]

main = EasyTest.run allTests