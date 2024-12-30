module Part1.Tasks where

import Util(notImplementedYet)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = taylorSin (normalize x)
  where
    normalize angle
      | angle > pi = normalize (angle - 2 * pi)
      | angle < -pi = normalize (angle + 2 * pi)
      | otherwise = angle

    taylorSin z = sum $ take 10 $ zipWith (\n k -> (-1) ^ n * (z ^ k) / fromIntegral (factorial k)) [0..] [1,3..]

    factorial 0 = 1
    factorial n = n * factorial (n - 1)

myCos :: Double -> Double
myCos x = taylorCos (normalize x)
  where
    normalize angle
      | angle > pi = normalize (angle - 2 * pi)
      | angle < -pi = normalize (angle + 2 * pi)
      | otherwise = angle

    taylorCos z = sum $ take 10 $ zipWith (\n k -> (-1) ^ n * (z ^ k) / fromIntegral (factorial k)) [0..] [0,2..]

    factorial 0 = 1
    factorial n = n * factorial (n - 1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
  | month < 1 || month > 12 = False
  | day < 1 || day > daysInMonth = False
  | otherwise = True
  where
    daysInMonth = case month of
        2 -> if isLeapYear year then 29 else 28
        4 -> 30
        6 -> 30
        9 -> 30
        11 -> 30
        _ -> 31
    isLeapYear y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || y `mod` 400 == 0


myPow :: Integer -> Integer -> Integer
myPow _ 0 = 1
myPow base exp
  | exp < 0 = error "Negative exponents not supported"
  | even exp = let half = myPow base (exp `div` 2) in half * half
  | otherwise = base * myPow base (exp - 1)

isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = null [x | x <- [2..floor (sqrt (fromIntegral n))], n `mod` x == 0]

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = abs (sum (zipWith crossProduct points (tail points ++ [head points])) / 2)
-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
  where
    crossProduct (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
  | not isTriangle = -1
  | a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2 + c^2 == a^2 = 2
  | a^2 + b^2 > c^2 && a^2 + c^2 > b^2 && b^2 + c^2 > a^2 = 1
  | otherwise = 0
  where
    isTriangle = a + b > c && a + c > b && b + c > a
