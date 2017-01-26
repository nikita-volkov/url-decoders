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
    subjectBenchmark "urlDecodersSubject" urlDecodersSubject
    ,
    subjectBenchmark "httpTypesSubject" httpTypesSubject
  ]

subjectBenchmark :: String -> Subject -> Benchmark
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

type Subject =
  ByteString -> HashMap Text [Text]

urlDecodersSubject :: Subject
urlDecodersSubject =
  either (error . show) id . A.query

httpTypesSubject :: Subject
httpTypesSubject =
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
  map (\i -> ("abc" <> (fromString . show) i, Just "Ф漢УЙ")) $
  [0 .. size]
