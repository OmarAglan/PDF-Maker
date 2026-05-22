-- | This module provides language specific customization to be included in the
-- LaTeX code.
module Babel where

import Codec.Binary.UTF8.String
import Data.ByteString hiding
  ( any,
    concat,
    drop,
    dropWhile,
    elem,
    hPutStr,
    init,
    intercalate,
    isPrefixOf,
    length,
    map,
    null,
    reverse,
    tails,
    take,
    takeWhile,
    zip,
  )
import Data.List.Split (splitOn)
import qualified Data.Map
import Data.Maybe
import Static

-- | returns the language specific customization to be included in the LaTeX
-- file.
makeBabel ::
  -- | the language code of the wiki currently being processed (en for English,
  -- de for German, etc.)
  Maybe String ->
  -- | The hostname or language code to be used if the first parameter is the
  -- Nothing value of the maybe monad.
  [Char] ->
  -- | The LaTeX code to be included in the LaTeX document when generating
  -- content for the language determined by the first two parameters.
  String
makeBabel b x =
  case Data.Map.lookup (fromMaybe xx b) m of
    Just v -> decode . unpack $ v
    _ -> case Data.Map.lookup "en" m of
      Just w -> decode . unpack $ w
      _ -> ""
  where
    m = Data.Map.fromList babelFiles
    xx =
      case splitOn "." x of
        (z : _) -> z
        _ -> "en"
