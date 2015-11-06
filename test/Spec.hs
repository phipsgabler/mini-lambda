{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

--import Test.QuickCheck
import Test.HUnit
import MiniLambda

testOmega = let omega = (lambda "x" <.> "x" @@ "x")
            in TestCase $ assertEqual 
              "Omega should reduce to itself"
              (normalize (omega @@ omega)) 
              (omega @@ omega)

cons = lambda "x" <.> lambda "y" <.> lambda "f" <.> "f" @@ "x" @@ "y"
car = lambda "t" <.> "t" @@ (lambda "x" <.> lambda "_" <.> "x")
cdr = lambda "t" <.> "t" @@ (lambda "_" <.> lambda "y" <.> "y")

testTuples = TestCase $ do
  let t = (cons @@ "1" @@ "2")
  assertEqual
    "A constructed tuple normalized to itself"
    (normalize t)
    t
  assertEqual
    "The car of (1,2) is 1"
    (normalize $ car @@ t)
    "1"
  assertEqual
    "The cdr of (1,2) is 2"
    (normalize $ cdr @@ t)
    "2"
    

-- return []
-- main = $quickCheckAll

main = runTestTT $ TestList [ testOmega
                            , testTuples
                            ]
