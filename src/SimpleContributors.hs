{-DHUN| module to load information on the contributors in images from the wiki website DHUN-}
module SimpleContributors where
import MediaWikiParseTree
import MediaWikiParser
import LatexRenderer
import Data.List
import Text.ParserCombinators.Parsec.Combinator hiding (count)
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Prim
import Data.Map hiding (filter, map, delete)
import Network.URL
import UrlAnalyse
import Codec.Binary.UTF8.String
import Data.Maybe
import Data.Functor.Identity
import Text.Parsec.Prim hiding (try, runParser)
import Control.Monad
import Licenses
import Tools
import ImperativeState
import HtmlParser (parseHtmlFast)

makeUrl2 :: String -> String -> [Char]
makeUrl2 theLemma theHost
  = (unify . exportURL)
      (URL{url_path = "w/index.php",
           url_params =
             [("title", (replace2 theLemma "%" "%25")), ("offset", ""),
              ("limit", "500000"), ("action", "history")],
           url_type =
             Absolute
               (Host{protocol = HTTP True, host = theHost, port = Nothing})})

makeUrl4 :: String -> [Char]
makeUrl4 uuu
  = fromMaybe uuu
      (do uu <- (importURL uuu)
          ti <- Data.List.lookup "title" (url_params uu)
          return $
            (unify . exportURL)
              (URL{url_path = (url_path uu),
                   url_params =
                     [("title", (replace2 ti "%" "%25")), ("offset", ""),
                      ("limit", "500000"), ("action", "history")],
                   url_type = url_type uu}))

makeUrl3 :: String -> String -> [Char]
makeUrl3 theLemma theHost
  = (unify . exportURL)
      (URL{url_path = "w/index.php", url_params = [("title", theLemma)],
           url_type =
             Absolute
               (Host{protocol = HTTP True, host = theHost, port = Nothing})})

deepGet2 :: [Char] -> [Anything a] -> [Anything a]
deepGet2 tag ll = concat $ map go ll
  where go (Environment Tag (TagAttr t m) l)
          | t == tag =
            [Environment Tag (TagAttr tag m) l] ++ (deepGet2 tag l)
        go (Environment _ _ l) = (deepGet2 tag l)
        go _ = []

getLicense :: [Anything Char] -> Maybe [Char]
getLicense l = (go l)
  where go :: [Anything Char] -> Maybe String
        go ll = msum (map (dg ll) licenses)
        dg ll (x, c)
          = case deepGet "a" "href" x ll of
                (_ : _) -> Just c
                _ -> Nothing

getAuthor :: [Anything Char] -> Maybe [Anything Char]
getAuthor x = listToMaybe (concat (map go (deepGet2 "tr" x)))
  where go (Environment _ _ l)
          = let gg = (deepGet "td" "id" "fileinfotpl_aut" l) in
              case gg of
                  (f : _) -> case delete f (deepGet2 "td" l) of
                                 [Environment _ _ ll] -> [ll]
                                 _ -> []
                  _ -> []
        go _ = []

simpleContributors ::
                   [Char] ->
                     [Char] ->
                       Maybe URL ->
                         ImperativeState -> IO [(String, String, Int, Maybe String)]
simpleContributors theLemma theHost uu st
  = do let theUrl3
             = case uu of
                   Just u -> exportURL u
                   _ -> makeUrl3 theLemma theHost
       yy <- geturl theUrl3
       let gg = (deepGet "li" "id" "ca-history" (parseHtmlFast yy))
       let theUrl
             = makeUrl4
                 (case gg of
                      ((Environment Tag (TagAttr _ _) l) : []) -> case deepGet2 "a" l of
                                                                      [Environment Tag
                                                                         (TagAttr _ mm) _] -> case
                                                                                                Data.Map.lookup
                                                                                                  "href"
                                                                                                  mm
                                                                                                of
                                                                                                  (Just
                                                                                                     x) -> "https://"
                                                                                                             ++
                                                                                                             theHost
                                                                                                               ++
                                                                                                               (replace2
                                                                                                                  x
                                                                                                                  "&amp;"
                                                                                                                  "&")
                                                                                                  _ -> []
                                                                      _ -> []
                      _ -> [])
       xx <- geturl theUrl
       let y = decodeString yy
       let x = decodeString xx
       let dd
             = ((deepGet "a" "class" "new mw-userlink" (parseHtmlFast x))
                  ++ (deepGet "a" "class" "mw-userlink" (parseHtmlFast x)))
                 :: [Anything Char]
       let ll = (filter pre (map go dd))
       let n = (nub ll) :: [(String, String)]
       let out = map go2 (zip (map (count ll) n) n)
       let ht = (parseHtmlFast y)
       case (getAuthor ht) of
           Just zz -> return
                        [(fst
                            (treeToLaTeX3 zz
                               initialState{urld = analyseNetloc (hostname . fullUrl $ st)}),
                          "", 1 :: Int, getLicense ht)]
           _ -> return out
  where go :: Anything Char -> (String, String)
        go (Environment Tag (TagAttr _ m) l)
          = ((shallowFlatten (deepFlatten l)), findWithDefault "" "href" m)
        go _ = ("", "")
        go2 (c, (a, h)) = (a, h, c, Nothing)

count :: (Eq a) => [a] -> a -> Int
count l s = length (filter (== s) l)

pre :: (String, String) -> Bool
pre s
  = case (runParser ipaddr () "" (fst s)) of
        Right _ -> False
        Left _ -> True

intdigit :: Parser Int
intdigit
  = do a <- digit
       case reads [a] of
           [(i, [])] -> return i
           _ -> pzero

ipnum3 :: ParsecT String () Identity Int
ipnum3
  = do a <- intdigit
       b <- intdigit
       c <- intdigit
       return (a * 100 + b * 10 + c)

ipnum2 :: ParsecT String () Identity Int
ipnum2
  = do a <- intdigit
       b <- intdigit
       return (a * 10 + b)

ipnum1 :: Parser Int
ipnum1 = do intdigit

ipnum :: ParsecT [Char] () Identity ()
ipnum
  = do n <- (try (ipnum3)) <|> (try (ipnum2)) <|> ipnum1
       if ((n <= 255) && (n >= 0)) then return () else pzero

ipaddr ::
       Text.Parsec.Prim.ParsecT [Char] () Data.Functor.Identity.Identity
         ()
ipaddr = try (ipv4addr) <|> ipv6addr

ipv4addr ::
         Text.Parsec.Prim.ParsecT [Char] () Data.Functor.Identity.Identity
           ()
ipv4addr
  = do _ <- ipnum
       _ <- char '.'
       _ <- ipnum
       _ <- char '.'
       _ <- ipnum
       _ <- char '.'
       _ <- ipnum
       return ()

ipv6num ::
        Text.Parsec.Prim.ParsecT [Char] () Data.Functor.Identity.Identity
          ()
ipv6num
  = try ((ipv4addr)) <|>
      do _ <- try (hexDigit) <|> return '0'
         _ <- try (hexDigit) <|> return '0'
         _ <- try (hexDigit) <|> return '0'
         _ <- try (hexDigit) <|> return '0'
         return ()

ipv6addr ::
         Text.Parsec.Prim.ParsecT [Char] () Data.Functor.Identity.Identity
           ()
ipv6addr
  = do _ <- try (ipv6num) <|> return ()
       _ <- char ':'
       _ <- try (ipv6num) <|> return ()
       _ <- try (char ':') <|> return ':'
       _ <- try (ipv6num) <|> return ()
       _ <- try (char ':') <|> return ':'
       _ <- try (ipv6num) <|> return ()
       _ <- try (char ':') <|> return ':'
       _ <- try (ipv6num) <|> return ()
       _ <- try (char ':') <|> return ':'
       _ <- try (ipv6num) <|> return ()
       _ <- try (char ':') <|> return ':'
       _ <- try (ipv6num) <|> return ()
       _ <- try (char ':') <|> return ':'
       _ <- try (ipv6num) <|> return ()
       _ <- eof
       return ()
