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


main =
  defaultMain (testGroup "All tests" allTests)
  where
    allTests =
      [httpTypesWithUTF8, httpTypesOnBytes]

httpTypesWithUTF8 =
  testProperty "\"http-types\" with UTF8 equality" (\text -> text === A.utf8 (B.urlEncode True (C.encodeUtf8 text)))

httpTypesOnBytes =
  testProperty "\"http-types\" with UTF8 equality" property
  where
    property bytes =
      bytes === C.encodeUtf8 (A.utf8 (B.urlEncode True bytes))

