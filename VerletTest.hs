import EasyTest
import Verlet

eqPoints :: (Point t) => [t] -> [t] -> Double
eqPoints x y =
    let helper a b = pointMax $ a .- b
    in maximum $ zipWith helper x y

allTests = tests [
    scope "1D case" $ expectAll [
        let
            acceleration :: Double -> Double
            acceleration _ = 0.0
            ans1D = take 5 $ verlet acceleration 0 2 0.1
        in eqPoints ans1D [0.0, 0.2, 0.4, 0.6, 0.8] < 1e-4
    ]
    ,
    scope "2D case" $ expectAll [
        let
            acceleration :: (Double, Double) -> (Double, Double)
            acceleration _ = (0.0, 0.0)
            ans2D = take 5 $ verlet acceleration (0,0) (2,1) 0.1
        in eqPoints ans2D [(0.0, 0.0), (0.2, 0.1), (0.4, 0.2), (0.6, 0.3), (0.8, 0.4)] < 1e-4
    ]
    ,
    scope "3D case" $ expectAll [
        let
            acceleration :: [Double] -> [Double]
            acceleration _ = [0.0, 0.0, 0.0]
            ans3D = take 5 $ verlet acceleration [0,0,0] [3,2,1] 0.1
        in eqPoints ans3D [[0.0, 0.0, 0.0], [0.3, 0.2, 0.1], [0.6, 0.4, 0.2], [0.9, 0.6, 0.3], [1.2, 0.8, 0.4]] < 1e-4
    ]
 ]

main = EasyTest.run allTests
