{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck
--import Test.HUnit
import qualified Data.Map.Strict as M

import MiniLambda
import MiniLambda.Parser

cons = lambda "x" <.> lambda "y" <.> lambda "f" <.> "f" @@ "x" @@ "y"
car = lambda "t" <.> "t" @@ (lambda "x" <.> lambda "_" <.> "x")
cdr = lambda "t" <.> "t" @@ (lambda "_" <.> lambda "y" <.> "y")

main = hspec $ do
  describe "MiniLambda" $ do
    describe "normalize" $ do
      let t = (cons @@ "1" @@ "2")
      it "can reduce car" $ do
        (normalize $ car @@ t) `shouldBe` "1"
      it "can reduce cdr" $ do
        (normalize $ cdr @@ t) `shouldBe` "2"
      it "performs eta reductions" $ do
        (normalize $ lambda "x" <.> "f" @@ "x") `shouldBe` "f"
    describe "normalizeWith" $ do
      let testPrelude = M.fromList [("cons", cons)
                                  , ("car", car)
                                  , ("cdr", cdr)
                                  ]
      it "keeps the car/cons invariant" $ do
        (normalizeWith testPrelude $ (car @@ (cons @@ "x" @@ "y"))) `shouldBe` "x"
      it "keeps the cdr/cons invariant" $ do
        (normalizeWith testPrelude $ (cdr @@ (cons @@ "x" @@ "y"))) `shouldBe` "y"
