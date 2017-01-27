module Main where

import Prelude
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified URLDecoders as A
import qualified Network.HTTP.Types.URI as B
import qualified Data.Text.Encoding as C
import qualified Data.HashMap.Strict as D


main =
  defaultMain $
  testGroup "All tests" $
  [
    testCase "Simple" $ do
      assertEqual "" (Right (D.fromList [("a", ["b"])])) (A.query "a=b")
    ,
    testCase "Absent value" $ do
      assertEqual "" (Right (D.fromList [("a", [])])) (A.query "a")
    ,
    testCase "Empty" $ do
      assertEqual "" (Right (D.fromList [])) (A.query "")
    ,
    testCase "Empty key" $ do
      assertEqual "" (Right (D.fromList [("", ["1"])])) (A.query "=1")
    ,
    testCase "Empty value" $ do
      assertEqual "" (Right (D.fromList [("a", [""])])) (A.query "a=")
      assertEqual "" (Right (D.fromList [("a", [""])])) (A.query "a=&")
    ,
    testCase "Array" $ do
      assertEqual "" (Right (D.fromList [("a", ["c", "b"])])) (A.query "a=b&a=c")
      assertEqual "" (Right (D.fromList [("a", ["c", "b"])])) (A.query "a[]=b&a[]=c")
      assertEqual "" (Right (D.fromList [("a", ["c", "b"])])) (A.query "a=b;a=c")
      assertEqual "" (Right (D.fromList [("a", ["c", "b"])])) (A.query "a[]=b;a[]=c")
    ,
    testCase "Unicode" $ do
      assertEqual "" (Right (D.fromList [("a", ["Держатель"])])) (A.query "a=%D0%94%D0%B5%D1%80%D0%B6%D0%B0%D1%82%D0%B5%D0%BB%D1%8C")
      assertEqual "" (Right (D.fromList [("Держатель", ["1"])])) (A.query "%D0%94%D0%B5%D1%80%D0%B6%D0%B0%D1%82%D0%B5%D0%BB%D1%8C=1")
    ,
    testCase "Ending" $ do
      assertEqual "" (Right (D.fromList [("a", ["b"])])) (A.query "a=b?blablabla")
      assertEqual "" (Right (D.fromList [("a", ["b"])])) (A.query "a=b#blablabla")
    ,
    testCase "Failure" $ do
      assertEqual "" (Left "Broken array declaration at key \"a\"") (A.query "a[")
      assertEqual "" (Left "Unexpected character: \"]\"") (A.query "a]")
      assertEqual "" (Left "Broken array declaration at key \"a\"") (A.query "a[]b")
  ]
