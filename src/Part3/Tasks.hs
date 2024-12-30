module Part3.Tasks where

import Data.List (group, sort, maximumBy)
import Data.Ord (comparing)
import qualified Data.Map as Map
import Data.List (partition)
import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = map f [n..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = iterate f x

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq nums = 
    let digits = concatMap (map (read . (:[])) . show . abs) nums
    in head $ maximumBy (comparing length) (group (sort digits))

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy _ [] = []
grokBy f (x:xs) =
    let key = f x
        (same, rest) = partition ((== key) . f) xs
    in (key, x : same) : grokBy f rest
