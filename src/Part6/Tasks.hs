{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Data.Map (Map)
import qualified Data.Map as M

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
    sparseMatrixWidth :: Int,
    sparseMatrixHeight :: Int,
    sparseMatrixElements :: Map (Int, Int) a
} deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
    getElem :: mx -> Int -> Int -> Int
    setElem :: Int -> Int -> Int -> mx -> mx
    width :: mx -> Int
    height :: mx -> Int
    zeroMatrix :: Int -> Int -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
    getElem x _ _ = x
    setElem v _ _ _ = v
    width _ = 1
    height _ = 1
    zeroMatrix _ _ = 0

instance Matrix [[Int]] where
    getElem mx i j = (mx !! i) !! j
    setElem v i j mx =
        take i mx ++ [take j (mx !! i) ++ [v] ++ drop (j + 1) (mx !! i)] ++ drop (i + 1) mx
    width mx = length (head mx)
    height mx = length mx
    zeroMatrix w h = replicate h (replicate w 0)

instance Matrix (SparseMatrix Int) where
    getElem (SparseMatrix _ _ elems) i j = M.findWithDefault 0 (i, j) elems
    setElem v i j (SparseMatrix w h elems) =
        SparseMatrix w h (if v == 0 then M.delete (i, j) elems else M.insert (i, j) v elems)
    width (SparseMatrix w _ _) = w
    height (SparseMatrix _ h _) = h
    zeroMatrix w h = SparseMatrix w h M.empty

-- Единичная матрица
eye :: Matrix m => Int -> m
eye n = foldl (\mx i -> setElem 1 i i mx) (zeroMatrix n n) [0 .. n - 1]

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero = zeroMatrix

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix mx1 mx2 = zeroMatrix (width mx1) (height mx2)

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant mx
    | width mx /= height mx = error "Matrix is not square"
    | otherwise = 1