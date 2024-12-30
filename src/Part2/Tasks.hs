module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) lhs rhs = BinaryTerm Plus lhs rhs

(|-|) :: Term -> Term -> Term
(|-|) lhs rhs = BinaryTerm Minus lhs rhs

(|*|) :: Term -> Term -> Term
(|*|) lhs rhs =
    case lhs of
        BinaryTerm Plus l r -> BinaryTerm Plus l (r |*| rhs)
        BinaryTerm Minus l r -> BinaryTerm Minus l (r |*| rhs)
        _ -> BinaryTerm Times lhs rhs


-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of
    Variable name
        | name == varName -> replacement
        | otherwise -> Variable name
    IntConstant val -> IntConstant val
    BinaryTerm op lhv rhv -> BinaryTerm op (replaceVar varName replacement lhv) (replaceVar varName replacement rhv)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (IntConstant val) = IntConstant val
evaluate (Variable name) = Variable name
evaluate (BinaryTerm op lhv rhv) =
    case (evaluate lhv, evaluate rhv) of
        (IntConstant l, IntConstant r) -> IntConstant (evalOp op l r)
        (newLhv, newRhv) -> BinaryTerm op newLhv newRhv
  where
    evalOp :: BinaryOp -> Int -> Int -> Int
    evalOp Plus = (+)
    evalOp Minus = (-)
    evalOp Times = (*)

