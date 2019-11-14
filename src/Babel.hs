
module Babel where
import Static
import Data.List.Split (splitOn)
import qualified Data.Map
import Data.Maybe
import Codec.Binary.UTF8.String
import Data.ByteString
       hiding (take, reverse, dropWhile, takeWhile, drop, map, concat,
               elem, zip, intercalate, init, tails, isPrefixOf, any, length, null,
               hPutStr)

makeBabel :: Maybe String -> [Char] -> String
makeBabel b x
  = case Data.Map.lookup (fromMaybe xx b) m of
        Just v -> decode . unpack $ v
        _ -> case Data.Map.lookup "en" m of
                 Just w -> decode . unpack $ w
                 _ -> ""
  where m = Data.Map.fromList babelFiles
        xx
          = case splitOn "." x of
                (z : _) -> z
                _ -> "en"
