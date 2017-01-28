module Main where

import Prelude
import Criterion.Main
import qualified URLDecoders as A
import qualified Data.Text.Encoding as B
import qualified Data.Text.Encoding.Error as C
import qualified Data.HashMap.Strict as D
import qualified Network.HTTP.Types.URI as E


main =
  defaultMain $
  [
    bgroup "url-decoders" $
    [
      subjectBenchmark "ascii" urlDecodersASCIISubject
      ,
      subjectBenchmark "utf8" urlDecodersUTF8Subject
    ]
    ,
    bgroup "http-types" $
    [
      subjectBenchmark "ascii" httpTypesASCIISubject
      ,
      subjectBenchmark "utf8" httpTypesUTF8Subject
    ]
  ]

subjectBenchmark :: NFData a => String -> Subject a -> Benchmark
subjectBenchmark title subject =
  bgroup title $
  [
    b "Small input" smallInput
    ,
    b "Medium input" mediumInput
    ,
    b "Large input" largeInput
  ]
  where
    b title input =
      bench title $ nf subject $ input


-- * Subjects
-------------------------

type Subject a =
  ByteString -> HashMap a [a]

urlDecodersASCIISubject :: Subject ByteString
urlDecodersASCIISubject =
  either (error . show) id . A.asciiQuery

urlDecodersUTF8Subject :: Subject Text
urlDecodersUTF8Subject =
  either (error . show) id . A.utf8Query

httpTypesASCIISubject :: Subject ByteString
httpTypesASCIISubject =
  foldl' step D.empty .
  E.parseQuery
  where
    step map (key, value) =
      D.insertWith (<>) key (maybe [] (: []) value) map

httpTypesUTF8Subject :: Subject Text
httpTypesUTF8Subject =
  foldl' step D.empty .
  E.parseQuery
  where
    step map (key, value) =
      D.insertWith (<>) (decodeText key) (maybe [] ((: []) . decodeText) value) map
    decodeText =
      B.decodeUtf8With C.lenientDecode


-- * Inputs
-------------------------

{-# NOINLINE smallInput #-}
smallInput :: ByteString
!smallInput =
  inputOfSize 3
  
{-# NOINLINE mediumInput #-}
mediumInput :: ByteString
!mediumInput =
  inputOfSize 10

{-# NOINLINE largeInput #-}
largeInput :: ByteString
!largeInput =
  inputOfSize 100

inputOfSize :: Int -> ByteString
inputOfSize size =
  E.renderQuery False $
  E.queryTextToQuery $
  map (\i -> ("abc" <> (fromString . show) i, Just "Ф漢УЙФ漢УЙФ漢УЙ")) $
  [0 .. size]
