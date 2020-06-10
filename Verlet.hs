{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Verlet where

class Point t where
    infixl 6 .+
    (.+) :: t -> t -> t
    infixl 7 .*
    (.-) :: t -> t -> t
    (.*) :: Double -> t -> t
    infixl 6 .-
    (.-) x y = x .+ ((-1.0) .* y)
    pointMax :: t -> Double

instance Point Double where
    x .+ y = x + y
    x .* y = x * y
    pointMax = abs

instance Point (Double, Double) where
    (x1, y1) .+ (x2, y2) = (x1 + x2, y1 + y2)
    c .* (x, y) = (c * x, c * y)
    pointMax (x, y) = max (abs x) (abs y)

instance Point [Double] where
    xs .+ ys = zipWith (+) xs ys
    x .* ys = map (*x) ys
    pointMax xs = maximum $ map abs xs

verlet :: Point t => (t -> t) -> t -> t -> Double -> [t]
verlet fA x0 v0 dt =
    let x1 = x0 .+ dt .* v0 .+ (0.5 * dt * dt) .*(fA x0)
        verlet' xPrev xCurr =
                let xNext = 2.0 .* xCurr .- xPrev .+ (dt * dt) .* (fA xCurr)
                in  xCurr : verlet' xCurr xNext
    in  x0 : verlet' x0 x1
