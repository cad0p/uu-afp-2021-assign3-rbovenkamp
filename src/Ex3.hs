{-# LANGUAGE MultiParamTypeClasses #-}

module Ex3 where

import           Control.Monad          ((>=>))
import           Control.Monad.RWS.Lazy (when)
import qualified Control.Monad.RWS.Lazy as R
import           Control.Monad.State    (MonadState, get, put)


data Teletype a = End a
                | Get (Char -> Teletype a)
                | Put Char (Teletype a)

-- EXERCISE 3.1
getLine :: Teletype String
getLine = Get (getter "") where
  getter :: String -> Char -> Teletype String
  getter str '\n' = End (str ++ ['\n'])
  getter str c    = Get (getter (str ++ [c]))

-- EXERCISE 3.2
instance Functor Teletype where
  fmap f (End x)   = End (f x)
  fmap f (Get g)   = Get (fmap f . g)
  fmap f (Put c x) = Put c (fmap f x)

instance Applicative Teletype where
  pure = End
  (<*>) (End f)   x = f <$> x
  (<*>) (Put c f) x = Put c (f <*> x)
  (<*>) (Get g)   x = Get (\c -> g c <*> x)

instance Monad Teletype where
  return = End
  (>>=) (End x)   f = f x
  (>>=) (Get g)   f = Get (g >=> f)
  (>>=) (Put c x) f = Put c (x >>= f)

-- EXERCISE 3.3
getChar :: Teletype Char
getChar = Get End

putChar :: Char -> Teletype ()
putChar c = Put c (End ())

-- EXERCISE 3.4
instance MonadState Char Teletype where
  get = Ex3.getChar
  put = Ex3.putChar

-- This definition allows us to use any Monad as state whereas the regular State definition
-- doesn't allow us to easily do everything with that part of the state

-- EXERCISE 3.5
runConsole :: Teletype a -> IO a
runConsole (End x)   = return x
runConsole (Get g)   = do c <- Prelude.getChar;
                          runConsole $ g c;
runConsole (Put c x) = do Prelude.putChar c;
                          runConsole x;

-- EXERCISE 3.6
type TeletypeRW = R.RWS String () String

runRWS :: Teletype a -> TeletypeRW a
runRWS (End x) = return x
runRWS (Get g) = do cs <- R.ask;
                    when (null cs) $ error "No termination with given input";
                    R.local (\_ -> tail cs) $ do
                      runRWS (g (head cs));
runRWS (Put c x) = do R.modify $ (++) [c];
                      runRWS x;

mockConsole :: Teletype a -> String -> (a, String)
mockConsole x cs = (ret, out) where
  rws = runRWS x
  (ret, out, _) = R.runRWS rws cs []
