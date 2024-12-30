module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist xs = impl REmpty xs
    where
        impl acc [] = acc
        impl acc (x : xs) = impl (acc :< x) xs

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    show = show . rlistToList

instance Eq a => Eq (ReverseList a) where
    REmpty == REmpty = True
    (a :< b) == (a' :< b') = b == b' && a == a'
    _ == _ = False

instance Semigroup (ReverseList a) where
    a <> REmpty = a
    a <> (l :< r) = (a <> l) :< r

instance Monoid (ReverseList a) where
    mempty = REmpty

instance Functor ReverseList where
    fmap _ REmpty = REmpty
    fmap f (xs :< x) = fmap f xs :< f x

instance Applicative ReverseList where
    pure x = REmpty :< x
    REmpty <*> y = REmpty
    (fx :< f) <*> y = (fx <*> y) <> (f <$> y)

instance Monad ReverseList where
    REmpty >>= _ = REmpty
    (xs :< x) >>= f = (xs >>= f) <> f x