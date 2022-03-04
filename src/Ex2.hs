{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Ex2 where

type Square      = Square' Nil
data Square' t a = Zero (t (t a)) | Succ (Square' (Cons t) a)

data Nil    a = Nil
data Cons t a = Cons a (t a)

--EXERCISE 1.1
emptyMatrix :: Square a
emptyMatrix = Zero Nil


matrix_1 :: Square Integer
matrix_1 = Succ $ Succ $ Zero $ Cons row_1 $ Cons row_2 Nil where
  row_1 = Cons 1 $ Cons 0 Nil
  row_2 = Cons 0 $ Cons 1 Nil

matrix_2 :: Square Integer
matrix_2 = Succ $ Succ $ Succ $ Zero $ Cons row_1 $ Cons row_2 $ Cons row_3 Nil where
  row_1 = Cons 1 $ Cons 2 $ Cons 3 Nil
  row_2 = Cons 4 $ Cons 5 $ Cons 6 Nil
  row_3 = Cons 7 $ Cons 8 $ Cons 9 Nil

-- EXERCISE 1.2
eqNil :: (a -> a -> Bool) -> (Nil a -> Nil a -> Bool)
eqNil _ Nil Nil = True

eqCons :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool))
       -> (a -> a -> Bool)
       -> (Cons t a -> Cons t a -> Bool)
eqCons eqT eqA (Cons x xs) (Cons y ys) = eqA x y && eqT eqA xs ys

-- Without type checking the type checker gives an error 'Couldn't match type a with b'
-- It expects a type b -> b -> Bool (the type of eqT) where b can be different from a
-- By adding the forall b. we indicate that the function works for ANY b

-- EXERCISE 1.3
eqSquare' :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool))
          -> (a -> a -> Bool)
          -> (Square' t a -> Square' t a -> Bool)
eqSquare' eqT eqA (Succ xs) (Succ ys) = eqSquare' (eqCons eqT) eqA xs ys
eqSquare' _   _   _         _         = False

-- By adding forall we indicate that b in the outer eqSquare' definition is the same as the
-- b in the inner definition, meaning that in inner eqSquare' b is not forAll b, but just b

-- EXERCISE 1.4
eqSquare :: (a -> a -> Bool) -> Square a -> Square a -> Bool
eqSquare = eqSquare' eqNil

instance Eq a => Eq (Square a) where
  (==) = eqSquare (==)

--mapSquare :: (a -> b) -> Square a -> Square b
--mapSquare f (Zero t) = Zero t

--instance Functor Square where
--  fmap = mapSquare
