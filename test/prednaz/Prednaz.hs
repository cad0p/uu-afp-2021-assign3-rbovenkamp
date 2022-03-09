{-# LANGUAGE LambdaCase #-}

-- SOURCE: https://gist.github.com/prednaz/11e6e08c98dc0c984eb4635ac6037a33#file-uu-afp-2021-assign3-tests-hs-L19-L62

import           Ex1                    (foldr)
import           Ex2                    (Cons (Cons), Nil (Nil), Square,
                                         Square' (Succ, Zero), mapNil,
                                         mapSquare')
import           Ex3                    (Teletype (End, Get, Put), getChar,
                                         getLine, mockConsole, putChar, runRWS)

import           Prelude                hiding (foldr, getChar, getLine,
                                         putChar)
import qualified Prelude                (foldr, getChar, getLine, putChar)


import           Control.Exception      (evaluate)
import           Control.Monad.RWS.Lazy (RWS, ask, get, local, modify, put,
                                         when)
import qualified Control.Monad.RWS.Lazy as RWS
import           Data.Char              (isLower)
import           Test.Hspec

runQuestion1 :: String -> (String, String)
-- adjust `E3.question1` if necessary
runQuestion1 input = take2 $ RWS.runRWS (myRunRWS getLine) input []

-- | whether the newline is included in the result
-- for quesiton 1 of exercise 3
includeNewline :: Bool
includeNewline = True

main :: IO ()
main = hspec $ do
  describe "exercise 1" $ do
    it "foldr works for empty string" $ foldr (:) [] "" `shouldBe` ""
    it "foldr non empty string" $ foldr (:) [] "abcd" `shouldBe` "abcd"
    -- it "y can evaluate fib 10" $
      -- -- y causes ticks issue, so not possible to test (you can try!)
      -- y (\fib -> \case {0 -> 0; 1 -> 1; n -> fib (n-1) + fib (n-2);})
      --   (10 :: Integer)
      -- `shouldBe`
      -- (55 :: Integer)
  describe "exercise 2" $ do
    it "mapSquare' and mapNil" $
      mapSquare' mapNil (== 0) matrix0 == matrix1
    it "fmap" $ fmap (== 0) matrix0 == matrix1
  describe "exercise 3" $ do
    describe "question 1" $ do
      it "ab" $
        evaluate (forceTuple $ runQuestion1 "ab")
        `shouldThrow`
        anyException
      it "\\n" $ runQuestion1 "\n" `shouldBe` ("" <> newline, "")
      it "\\nab" $ runQuestion1 "\nab" `shouldBe` ("" <> newline, "")
      it "ab\\n" $ runQuestion1 "ab\n" `shouldBe` ("ab" <> newline, "")
      it "ab\\ncd" $ runQuestion1 "ab\ncd" `shouldBe` ("ab" <> newline, "")
      it "ab\\cdn\\nef" $
        runQuestion1 "ab\ncd\nef" `shouldBe` ("ab" <> newline, "")
    describe "question 2" $
      testTeletype (\i -> take2 $ RWS.runRWS (myRunRWS teletype) i [])
    describe "question 2, 3" $
      testTeletype (\i -> take2 $ RWS.runRWS (myRunRWS teletype2And3) i [])
    describe "question 2, 4" $
      testTeletype (\i -> take2 $ RWS.runRWS (myRunRWS teletype2And4) i [])
    -- runRWS gets stuck
    describe "runRWS" $
      testTeletype (\i -> take2 $ RWS.runRWS (runRWS teletype) i [])
    describe "runRWS, mockConsole" $
      testTeletype (mockConsole teletype)

matrix0 :: Square Integer
matrix0 =
  Succ $
  Succ $
  Zero $
  Cons (Cons 1 $ Cons 0 Nil) $
  Cons (Cons 0 $ Cons 1 Nil)
  Nil

matrix1 :: Square Bool
matrix1 =
  Succ $
  Succ $
  Zero $
  Cons (Cons False $ Cons True Nil) $
  Cons (Cons True $ Cons False Nil)
  Nil

testTeletype ::
  (String -> (String, String)) ->
  SpecWith ()
testTeletype run = do
  it "aBcDe" $
    evaluate (forceTuple $ run "aBcDe") `shouldThrow` anyException
  it "." $ run "." `shouldBe` ("", "")
  it ".a" $ run ".a" `shouldBe` ("", "")
  it "aBcDe." $ run "aBcDe." `shouldBe` ("aBcDe", "ace")
  it "aBcDe.f" $ run "aBcDe.f" `shouldBe` ("aBcDe", "ace")
  it "aBcDe.f.g" $ run "aBcDe.f.g" `shouldBe` ("aBcDe", "ace")

teletype :: Teletype String
teletype =
  Get
    (\c ->
      if c == '.'
      then End []
      else (if isLower c then Put c else id) $ mapTeletype (c :) teletype
    )

teletype2 :: Teletype String
teletype2 =
  do
    c <- Get End
    if c == '.'
      then pure []
      else
        when (isLower c) (Put c $ End ()) *> fmap (c :) teletype2

teletype2And3 :: Teletype String
teletype2And3 =
  do
    c <- getChar
    if c == '.'
      then pure []
      else
        when (isLower c) (putChar c) *> fmap (c :) teletype2And3

teletype2And4 :: Teletype String
teletype2And4 =
  do
    c <- get
    if c == '.'
      then pure []
      else
        when (isLower c) (put c) *> fmap (c :) teletype2And4

myQuestion1 :: Teletype String
myQuestion1 =
  Get
    (\c ->
      if c == '\n' then End [c] else mapTeletype (c :) myQuestion1
    )

myRunRWS :: Teletype a -> RWS String () String a
myRunRWS =
  \case
    End a -> pure a
    Get f ->
      ask
        >>= \case
          c : cs -> local (const cs) (myRunRWS $ f c)
          []     -> error "missing input"
    Put char t -> modify (<> [char]) *> myRunRWS t

mapTeletype :: (a -> b) -> Teletype a -> Teletype b
mapTeletype f =
  \case
    End a      -> End (f a)
    Get g      -> Get (mapTeletype f . g)
    Put char t -> Put char (mapTeletype f t)

newline :: String
newline = if includeNewline then "\n" else ""

forceTuple :: (a, b) -> (a, b)
forceTuple t@(a, b) = a `seq` b `seq` t

take2 :: (a, b, c) -> (a, b)
take2 (a, b, _c) = (a, b)
