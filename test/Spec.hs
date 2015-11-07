{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

--import Test.QuickCheck
import Test.HUnit
import MiniLambda
import MiniLambda.Parser

cons = lambda "x" <.> lambda "y" <.> lambda "f" <.> "f" @@ "x" @@ "y"
car = lambda "t" <.> "t" @@ (lambda "x" <.> lambda "_" <.> "x")
cdr = lambda "t" <.> "t" @@ (lambda "_" <.> lambda "y" <.> "y")

testTuples1 = TestCase $ do
  let t = (cons @@ "1" @@ "2")
  assertEqual
    "The car of (1,2) is 1"
    (normalize $ car @@ t)
    "1"
  assertEqual
    "The cdr of (1,2) is 2"
    (normalize $ cdr @@ t)
    "2"
    
testTuples2 = TestCase $ assertEqual
  "car/cons invariant is kept"
  (normalize $ lambda "x" <.> lambda "y" <.> car @@ (cons @@ "x" @@ "y"))
  (lambda "x" <.> lambda "y" <.> "x")

etaEquivalence = TestCase $ assertEqual
  "Eta-minimality"
  (normalize $ lambda "x" <.> "f" @@ "x")
  ("f")


-- testParserBasic = TestCase $ do
--   assertEqual

main = runTestTT $ TestList [ testTuples1
                            , testTuples2
                            , etaEquivalence
                            --, testParserBasic
                            ]
