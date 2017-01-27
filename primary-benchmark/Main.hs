module Main where

import Prelude
import Criterion.Main
import qualified URLDecoders as A
import qualified Network.HTTP.Types.URI as E


main =
  defaultMain $
  [
    bench "" $ nf (either (error . show) id . A.query) $! inputOfSize 10
  ]


-- * Inputs
-------------------------

inputOfSize :: Int -> ByteString
inputOfSize size =
  E.renderQuery False $
  E.queryTextToQuery $
  map (\i -> ("abc" <> (fromString . show) i, Just "Ф漢УЙ")) $
  [0 .. size]
