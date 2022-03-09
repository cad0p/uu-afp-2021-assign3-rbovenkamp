{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

module Ex2 where

type Square = Square' Nil
data Square' t a = Zero (t (t a)) | Succ (Square' (Cons t) a)

data Nil    a = Nil
data Cons t a = Cons a (t a)

--EXERCISE 2.1
matrix_1 :: Square Integer
matrix_1 = Succ $ Succ $ Zero $ Cons row_1 $ Cons row_2 Nil where
  row_1 = Cons 1 $ Cons 0 Nil
  row_2 = Cons 0 $ Cons 1 Nil

matrix_2 :: Square Integer
matrix_2 = Succ $ Succ $ Succ $ Zero $ Cons row_1 $ Cons row_2 $ Cons row_3 Nil where
  row_1 = Cons 1 $ Cons 2 $ Cons 3 Nil
  row_2 = Cons 4 $ Cons 5 $ Cons 6 Nil
  row_3 = Cons 7 $ Cons 8 $ Cons 9 Nil

-- EXERCISE 2.2
eqNil :: (a -> a -> Bool) -> (Nil a -> Nil a -> Bool)
eqNil _ Nil Nil = True

eqCons :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool))
       -> (a -> a -> Bool)
       -> (Cons t a -> Cons t a -> Bool)
eqCons eqT eqA (Cons x xs) (Cons y ys) = eqA x y && eqT eqA xs ys

-- Without type checking the type checker gives an error 'Couldn't match type a with b'
-- It expects a type b -> b -> Bool (the type of eqT) where b can be different from a
-- By adding the forall b. we indicate that the function works for ANY b

-- EXERCISE 2.3
eqSquare' :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool))
          -> (a -> a -> Bool)
          -> (Square' t a -> Square' t a -> Bool)
eqSquare' eqT eqA (Succ xs) (Succ ys) = eqSquare' (eqCons eqT) eqA xs ys
eqSquare' _   _   _         _         = False

-- By adding forall we indicate that b in the outer eqSquare' definition is the same as the
-- b in the inner definition, meaning that in inner eqSquare' b is not forAll b, but just b

-- EXERCISE 2.4
eqSquare :: (a -> a -> Bool) -> Square a -> Square a -> Bool
eqSquare = eqSquare' eqNil

instance Eq a => Eq (Square a) where
  (==) = eqSquare (==)

mapNil :: (a -> b) -> (Nil a -> Nil b)
mapNil _ Nil = Nil

mapCons :: ((a -> b) -> (t a -> t b))
        -> (a -> b) -> Cons t a -> Cons t b
mapCons mapT f (Cons x xs) = Cons (f x) (mapT f xs)

mapSquare' :: (forall c d. (c -> d) -> (t c -> t d))
           -> (a -> b) -> Square' t a -> Square' t b
mapSquare' mapT f (Succ xs) = Succ (mapSquare' (mapCons mapT) f xs)
mapSquare' mapT f (Zero xs) = Zero (mapT (mapT f) xs)

mapSquare :: (a -> b) -> Square a -> Square b
mapSquare = mapSquare' mapNil

instance Functor Square where
  fmap = mapSquare

-- EXERCISE 2.5

-- type Square a = Square' Nil a
-- instance Functor Square where
--    fmap = mapSquare

-- The error we get with above declaration is that square should have one argument, so we
-- change the instance declaration:

--instance Functor (Square a) where
--  fmap = mapSquare

-- Which gives us the error that we expect a kind * -> * but square has kind *
-- Can't really figure out why this restriction is there
