module HtmlParser where
import MediaWikiParser
import MediaWikiParseTree
import qualified Data.Map as Map
import Text.HTML.TagSoup


parseHtml :: String -> [Anything Char]
parseHtml = parseit minparsers





data Frame = Frame{tag :: Tag String, ds :: [Anything Char]}

parseHtmlFast :: String -> [Anything Char]
parseHtmlFast = makebrackets . parseTags

makebrackets :: [Tag String] -> [Anything Char]
makebrackets l = go l [Frame{tag = (TagOpen "root" []), ds = []}]
  where go [] ys
          = reverse (ds (head (closeTags ((length ys) - 1) ys)))
        go ((TagOpen n a) : xs) ys
          = go xs (Frame{tag = (TagOpen n a), ds = []} : ys)
        go ((TagClose n) : xs) ys
          = case getfrm n ys 1 of
                Just m -> let midys = closeTags m ys in
                            go xs (openTags (m - 1) ys midys)
                _ -> go xs ys
        go ((TagText n) : xs) (y : ys)
          = go xs (y{ds = (reverse (map C n)) ++ (ds y)} : ys)
        go (_ : xs) ys = go xs ys
        
        closeTags :: Int -> [Frame] -> [Frame]
        closeTags 0 ys = ys
        closeTags m (y : ys)
          = case tag y of
                TagOpen n a -> case ys of
                                   (yy : yys) -> closeTags (m - 1)
                                                   (yy{ds =
                                                         (Environment Tag
                                                            (TagAttr n (Map.fromList a))
                                                            (reverse (ds y)))
                                                           : (ds yy)}
                                                      : yys)
                                   _ -> ys
                _ -> ys
        closeTags _ _ = []
        openTags 0 _ zs = zs
        openTags 1 (y : _) zs = Frame{tag = tag y, ds = []} : zs
        openTags n (y : ys) zs
          = Frame{tag = tag y, ds = []} : (openTags (n - 1) ys zs)
        openTags _ _ zs = zs
        
        getfrm :: String -> [Frame] -> Int -> Maybe Int
        getfrm _ [] _ = Nothing
        getfrm n (y : ys) m
          = case (tag y) of
                TagOpen nn _ | nn == n -> Just m
                _ -> getfrm n ys (m + 1)

