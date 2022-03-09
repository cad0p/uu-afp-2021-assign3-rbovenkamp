{-# OPTIONS_GHC -fmax-simplifier-iterations=1 #-}

module Ex1 (Ex1.foldr) where

-- EXERCISE 1.1
fix :: (a -> a) -> a
fix f = f (fix f)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = fix (\foldr' f x ys -> case ys of
    []       -> x
    (y':ys') -> foldr' f (f y' x) ys')

-- EXERCISE 1.2
-- y = \f -> (\x -> f (x x)) (\x -> f (x x))

-- Error:
-- Cannot construct the infinite type t0 ~ t0 -> t
-- Expected type: t0 -> t
-- Actual type:   (t0 -> t) -> t

-- Explanation:
-- y is a lambda that takes one parameter f
-- f returns the function (\x -> f (x x)) applied to (\x -> f (x x)) which is the same exact function
-- In this return value the function f is again applied to (x x) where x is also a function
-- The type annotation of f would be f :: a -> b
-- The type annotation of x would be x :: c -> b since it returns a result from f
-- Thus (x x) :: (c -> b) -> b
-- And f (x x) :: ((c -> b) -> b) -> b
-- c remains a free type variable so the general type of the function cannot be constructed
-- If we can force c to be b instead then this would work which is what we do with the F+unF

-- newtype instead of data because the haskell linter wanted me to do this
newtype F a = F {unF :: F a -> a}

y :: (a -> a) -> a
y f = (\x -> f (unF x x)) (F {unF = \x -> f (unF x x)})
