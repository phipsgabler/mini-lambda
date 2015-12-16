{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck
--import Test.HUnit
import qualified Data.Map.Strict as M
import Control.Applicative

import MiniLambda
import MiniLambda.Parser

validIdentifier :: Gen String
validIdentifier = (listOf1 . elements $ ['!'..'&'] ++ ['*'..'-'] ++ ['/'..'['] ++ [']'..'~'])

instance Arbitrary Expr where
  arbitrary = oneof $ [Var <$> validIdentifier
                     , App <$> arbitrary <*> arbitrary
                     , Lambda <$> validIdentifier <*> arbitrary
                     ]

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
      -- it "performs eta reductions" $ do
      --   (normalize $ lambda "x" <.> "f" @@ "x") `shouldBe` "f"
    describe "normalizeWith" $ do
      let testPrelude = M.fromList [("cons", cons)
                                  , ("car", car)
                                  , ("cdr", cdr)
                                  ]
      it "keeps the car/cons invariant" $ do
        (normalizeWith testPrelude $ (car @@ (cons @@ "x" @@ "y"))) `shouldBe` "x"
      it "keeps the cdr/cons invariant" $ do
        (normalizeWith testPrelude $ (cdr @@ (cons @@ "x" @@ "y"))) `shouldBe` "y"
    describe "freeIn" $ do
      it "filters out bound variables" $ do
        ("x" `freeIn` (lambda "x" <.> "x")) `shouldBe` False
      it "applies for unbound body variables" $ do
        ("x" `freeIn` (lambda "y" <.> "x")) `shouldBe` True
      it "conjuncts in applications -- true" $ do
        ("x" `freeIn` ((lambda "x" <.> "x") @@ "x")) `shouldBe` True
      it "conjuncts in applications -- false" $ do
        ("x" `freeIn` ("y" @@ (lambda "x" <.> "y"))) `shouldBe` False
    describe "substitute" $ do
      it "ignores bound variables" $ do
        let id = (lambda "x" <.> "x")
        (substitute "x" "y" id) `shouldBe` id
    describe "Parser" $ do
      describe "parseExpression" $ do
        it "is the inverse of show" $ property $ do
          \expr -> case parseExpression $ show expr of
                     Left _ -> False
                     Right expr' -> expr' == expr
        it "is robust against scattered whitespace" $ do
          parseExpression "  ( \\ x .  ( x    x  )   ) " `shouldBe` (Right $ lambda "x" <.> "x" @@ "x")
