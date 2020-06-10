-- Не забудьте добавить тесты.

{-# OPTIONS_GHC -Wall #-}
module Poly where

-- Многочлены
-- a -- тип коэффициентов, список начинается со свободного члена.
-- Бонус: при решении следующих заданий подумайте, какие стали бы проще или
-- сложнее при обратном порядке коэффициентов (и добавьте комментарий).
newtype Poly a = P [a]

-- Задание 1 -----------------------------------------

-- Определите многочлен $x$.
x :: Num a => Poly a
x = P [0, 1]

-- Задание 2 -----------------------------------------

-- Функция, считающая значение многочлена в точке
applyPoly :: Num a => Poly a -> a -> a
applyPoly (P []) _      = 0
applyPoly (P (x:xs)) x0 = x + x0 * (applyPoly (P xs) x0)

-- Задание 3 ----------------------------------------

-- Определите равенство многочленов
-- Заметьте, что многочлены с разными списками коэффициентов
-- могут быть равны! Подумайте, почему.
instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P []) (P []) = True
    (==) (P x) (P []) = all (== 0) x
    (==) (P []) (P y) = all (== 0) y
    (==) (P (x:xs)) (P (y:ys)) = (x == y) && ((==) (P xs) (P ys))
 
-- Задание 4 -----------------------------------------

-- Определите перевод многочлена в строку. 
-- Это должна быть стандартная математическая запись, 
-- например: show (3 * x * x + 1) == "3 * x^2 + 1").
-- (* и + для многочленов можно будет использовать после задания 6.)
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P p) = let ps = reverse $ filter ((/= 0) . fst) $ zip p ([0..] :: [Integer])
                     showPoly [] = ["0"]
                     showPoly (a:as) = showFst a : map (wrap) (as) where
                        showSign coef = if signum coef == 1
                                        then " + "
                                        else " - "

                        showPow (_, 0) = ""
                        showPow (_, 1) = "x"
                        showPow (_, n) = "x^" ++ show n

                        showVal (coef, n) isFirst = showVal' (abs coef) isFirst where
                            showVal' 1 True = case (signum coef, n) of
                                                (_, 0) -> show coef
                                                (-1, _) -> "-"
                                                _ -> ""
                            showVal' _ True = show coef ++ if n == 0 then "" else " * "
                            showVal' 1 False = if n == 0 then show $ abs coef else ""
                            showVal' coef' False = show coef' ++ if n == 0 then "" else " * "

                        showFst coefN = showVal coefN True ++ showPow coefN
                        wrap coefN = showSign (fst coefN) ++ showVal coefN False ++ showPow coefN
                  in concat $ showPoly ps
-- Задание 5 -----------------------------------------

-- Определите сложение многочленов
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P p1) (P p2) = let 
                        plus1 []       y        = y
                        plus1 x        []       = x
                        plus1 (x : xs) (y : ys) = (x + y) : plus1 xs ys
                     in P $ plus1 p1 p2

-- Задание 6 -----------------------------------------

-- Определите умножение многочленов
times :: Num a => Poly a -> Poly a -> Poly a
times (P p1) (P p2) = let 
                        times1 (P []      ) _      = P []
                        times1 (P (x : xs)) (P ys) = P (map (x *) ys) + times1 (P xs) (P (0 : ys))
                      in times1 (P p1) (P p2)

-- Задание 7 -----------------------------------------

-- Сделайте многочлены числовым типом
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P p) = P (map negate p)
    fromInteger x = P [fromIntegral x]
    -- Эти функции оставить как undefined, поскольку для 
    -- многочленов они не имеют математического смысла
    abs    = undefined
    signum = undefined

-- Задание 8 -----------------------------------------

-- Реализуйте nderiv через deriv
class Num a => Differentiable a where
    -- взятие производной
    deriv  :: a -> a
    -- взятие n-ной производной
    nderiv :: Int -> a -> a
    nderiv n pol = iterate deriv pol !! n

-- Задание 9 -----------------------------------------

-- Определите экземпляр класса типов
instance Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P (_:ps)) = let 
                        deriv1 [] _ = []
                        deriv1 (x:xs) n = x * n : deriv1 xs (n + 1)
                       in P $ deriv1 ps 1 
