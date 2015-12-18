{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as M
import Control.Applicative

import MiniLambda
import MiniLambda.Parser
import MiniLambda.Definitions

validIdentifier :: Gen String
validIdentifier = listOf1 . elements $ ['!'..'&'] ++ ['*'..'-'] ++ ['/'..'['] ++ [']'..'~']

instance Arbitrary Expr where
  arbitrary = sized sizedExprGenerator
    where sizedExprGenerator 0 = Var <$> validIdentifier
          sizedExprGenerator n = oneof $ [ App <$> smallerExpr <*> smallerExpr
                                         , Lambda <$> validIdentifier <*> smallerExpr
                                         ]
                             where smallerExpr = sizedExprGenerator (n `div` 2)

main = hspec $ do
  describe "MiniLambda" $ do
    describe "evalFull" $ do
      let t = (cons @@ "1" @@ "2")
      it "can reduce car" $ do
        (evalFull $ car @@ t) `shouldBe` "1"
      it "can reduce cdr" $ do
        (evalFull $ cdr @@ t) `shouldBe` "2"
      it "avoids names capture" $ do
        (evalFull $ twice @@ twice) `shouldBe` -- (λx.(λx'.(x (x (x (x x'))))))
          (lambda "x" <.> lambda "x'" <.> ("x" @@ ("x" @@ ("x" @@ ("x" @@ "x'")))))
      -- it "performs eta reductions" $ do
      --   (evalFull $ lambda "x" <.> "f" @@ "x") `shouldBe` "f"
    describe "evalFullWith" $ do
      let testPrelude = M.fromList [("cons", cons)
                                  , ("car", car)
                                  , ("cdr", cdr)
                                  ]
      it "keeps the car/cons invariant" $ do
        (evalFullWith testPrelude $ (car @@ (cons @@ "x" @@ "y"))) `shouldBe` "x"
      it "keeps the cdr/cons invariant" $ do
        (evalFullWith testPrelude $ (cdr @@ (cons @@ "x" @@ "y"))) `shouldBe` "y"
    describe "freeIn" $ do
      it "filters out bound variables" $ do
        ("x" `freeIn` (lambda "x" <.> "x")) `shouldBe` False
      it "applies for unbound body variables" $ do
        ("x" `freeIn` (lambda "y" <.> "x")) `shouldBe` True
      it "conjuncts in applications -- true" $ do
        ("x" `freeIn` ((lambda "x" <.> "x") @@ "x")) `shouldBe` True
      it "conjuncts in applications -- false" $ do
        ("x" `freeIn` ("y" @@ (lambda "x" <.> "y"))) `shouldBe` False
    -- describe "substitute" $ do
    --   it "ignores bound variables" $ do
    --     let id = (lambda "x" <.> "x")
    --     (substitute "x" "y" id) `shouldBe` id
    describe "Parser" $ do
      describe "parseExpression" $ do
        it "is the inverse of show" $ property $ do
          \expr -> case parseExpression $ show expr of
                     Left _ -> False
                     Right expr' -> expr' == expr
        it "is robust against scattered whitespace" $ do
          parseExpression "  ( \\ x .  ( x    x  )   ) " `shouldBe` (Right $ lambda "x" <.> "x" @@ "x")
        it "can handle unicode lambdas and primes" $ do
           parseExpression "(λx.(λx'.(x x')))" `shouldBe` 
                           (Right $ lambda "x" <.> lambda "x'" <.> "x" @@ "x'")
