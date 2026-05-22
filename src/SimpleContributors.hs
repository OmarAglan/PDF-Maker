-- | module to load information on the contributors in images from the wiki
-- website
module SimpleContributors where

import Codec.Binary.UTF8.String
import Control.DeepSeq
import Control.Monad
import Data.Functor.Identity
import Data.List
import Data.Map hiding (delete, filter, map)
import Data.Maybe
import HtmlParser (parseHtmlFast)
import ImperativeState
import LatexRenderer
import Licenses
import MediaWikiParseTree
import MediaWikiParser
import Network.URL
import Text.Parsec.Char
import Text.Parsec.Prim hiding (runParser, try)
import Text.ParserCombinators.Parsec.Combinator hiding (count)
import Text.ParserCombinators.Parsec.Prim
import Tools
import UrlAnalyse

-- | Generate an URL linking to the version history of a page form the URL to
-- the page itself.
makeUrl4 ::
  -- | The URL to the page.
  String ->
  -- | The URL to the version history of the page given in the first parameter.
  [Char]
makeUrl4 uuu =
  fromMaybe
    uuu
    ( do
        uu <- (importURL uuu)
        ti <- Data.List.lookup "title" (url_params uu)
        return $
          (unify . exportURL)
            ( URL
                { url_path = (url_path uu),
                  url_params =
                    [ ("title", (replace2 (replace2 ti "%27" "'") "%" "%25")),
                      ("offset", ""),
                      ("limit", "500000"),
                      ("action", "history")
                    ],
                  url_type = url_type uu
                }
            )
    )

-- | Generate an URL from a lemma on a wiki (e.g. an name of an article on
-- Wikipedia) and a host name where the server of the wiki resides.
makeUrl3 ::
  -- | The lemma on the wiki
  String ->
  -- | The host name of the server serving the wiki
  String ->
  -- | The URL linking to the lemma on the server.
  [Char]
makeUrl3 theLemma theHost =
  (unify . exportURL)
    ( URL
        { url_path = "w/index.php",
          url_params = [("title", (replace2 (replace2 theLemma "%27" "'") "%" "%25"))],
          url_type =
            Absolute
              (Host {protocol = HTTP True, host = theHost, port = Nothing})
        }
    )

-- | Get all tag with a given tag name from a parse tree as a list of parse tree
-- elements.
deepGet2 ::
  -- | The name of the tag to be looked for.
  [Char] ->
  -- | The parse tree to be investigated.
  [Anything a] ->
  -- | The list of parse tree element with the given tag name.
  [Anything a]
deepGet2 tag ll = concat $ map go ll
  where
    go (Environment Tag (TagAttr t m) l)
      | t == tag =
          [Environment Tag (TagAttr tag m) l] ++ (deepGet2 tag l)
    go (Environment _ _ l) = (deepGet2 tag l)
    go _ = []

-- | Get the license of a media file from the parse tree of the media
-- description page e.g. an image description page on wikimedia commons.
getLicense ::
  -- | The parse tree representing the media description page .
  [Anything Char] ->
  -- | A Just value of the maybe monad containing the acronym of the licenses
  -- under which the media file is licensed.
  Maybe [Char]
getLicense l = (go (killTree l))
  where
    go :: [Anything Char] -> Maybe String
    go ll = msum (map (dg ll) licenses)
    dg ll (x, c) =
      case deepGet "a" "href" x ll of
        (_ : _) -> Just c
        _ -> Nothing
    killTree ll = concat (map killNode ll)

    killNode (Environment Tag (TagAttr "footer" _) _) = []
    killNode (Environment x y ll) =
      [Environment x y (killTree ll)]
    killNode x = [x]

-- | Get an author as a parse tree section from the information template on a
-- media file, e.g. an image on wikimedia commons.
getAuthor ::
  -- | The parse tree to be inspected. E.g. as downloaded and parsed from an
  -- image description page on wikimedia commons.
  [Anything Char] ->
  -- | The section of the parse tree containing the information on the author of
  -- the media file
  Maybe [Anything Char]
getAuthor x = listToMaybe (concat (map go (deepGet2 "tr" x)))
  where
    go (Environment _ _ l) =
      let gg = (deepGet "td" "id" "fileinfotpl_aut" l) ++ (deepGet "th" "id" "fileinfotpl_aut" l)
       in case gg of
            (f : _) -> case delete f (deepGet2 "td" l) of
              ((Environment _ _ ll) : _) -> [ll]
              _ -> []
            _ -> []
    go _ = []

-- | Find out the contributors of a media file.
simpleContributors ::
  -- | The lemma (name of the article) under which the media file of which the
  -- contributors shall be determined is stored on the wiki.
  [Char] ->
  -- | The host name of the wiki server where the article is stored
  [Char] ->
  -- | The URL under which the media file of which the contributors shall be
  -- determined is stored.
  Maybe URL ->
  -- | The state of the part of the program with imperative flow of control (see
  -- `ImperativeState.ImperativeState`)
  ImperativeState ->
  -- | A list of tuples. The first element of each tuple is the name of a
  -- contributor in LaTeX notation. The second element of the tuple is a link to
  -- the wiki page of the contributor. The third element of each tuple is the
  -- number of contributions of the given contributor to the given wiki page.
  -- The fourth element of each tuple is the abbreviation of the license of the
  -- media file as a Just value of the Maybe monad if it could be determined and
  -- a Nothing value of the Maybe monad otherwise.
  IO [(String, String, Int, Maybe String)]
simpleContributors theLemma theHost uu st =
  do
    let theUrl3 =
          case uu of
            Just u -> exportURL u
            _ -> makeUrl3 theLemma theHost
    yy <- geturl theUrl3
    let gg = (deepGet "li" "id" "ca-history" (parseHtmlFast yy))
    let theUrl =
          makeUrl4
            ( case gg of
                ((Environment Tag (TagAttr _ _) l) : []) -> case deepGet2 "a" l of
                  [ Environment
                      Tag
                      (TagAttr _ mm)
                      _
                    ] -> case Data.Map.lookup
                      "href"
                      mm of
                      ( Just
                          x
                        ) ->
                          if (Data.List.take 8 x == "https://")
                            then (replace2 x "&amp;" "&")
                            else
                              "https://"
                                ++ theHost
                                ++ ( replace2
                                       x
                                       "&amp;"
                                       "&"
                                   )
                      _ -> []
                  _ -> []
                _ -> []
            )
    let y = decodeString yy
    out <- getCtrbFromUrl theUrl
    let ht = (parseHtmlFast y)
    case (getAuthor ht) of
      Just zz ->
        return
          [ ( fst
                ( treeToLaTeX3
                    zz
                    initialState {urld = analyseNetloc (hostname . fullUrl $ st)}
                ),
              "",
              1 :: Int,
              getLicense ht
            )
          ]
      _ -> return out
  where
    go :: Anything Char -> (String, String)
    go (Environment Tag (TagAttr _ m) l) =
      ((shallowFlatten (deepFlatten l)), findWithDefault "" "href" m)
    go _ = ("", "")
    go2 (c, (a, h)) = (a, h, c, Nothing)
    getCtrbFromUrl :: String -> IO [(String, String, Int, Maybe String)]
    getCtrbFromUrl theUrl =
      do
        xx <- geturl theUrl
        let x = decodeString xx
        let ff = (force (parseHtmlFast x))
        let dd =
              ( ( (deepGet "a" "class" "new mw-userlink" ff)
                    ++ (deepGet "a" "class" "mw-userlink" ff)
                )
              ) ::
                [Anything Char]
        let ll = (filter pre (map go dd))
        let n = (nub ll) :: [(String, String)]
        let nextlink = nub (deepGet "a" "class" "mw-nextlink" ff)
        next <- case nextlink of
          [Environment Tag (TagAttr _ m) _] -> case findWithDefault "" "href" m of
            "" -> return []
            nexturl ->
              getCtrbFromUrl
                ( "https://"
                    ++ theHost
                    ++ ( replace2
                           nexturl
                           "&amp;"
                           "&"
                       )
                )
          _ -> return []
        return ((map go2 (zip (map (count ll) n) n)) ++ next)

-- | Count the number of occurrences of the item given as second parameter in
-- the list given as first parameter.
count ::
  (Eq a) =>
  -- | The list in which the occurrences of the item given in the second
  -- parameter shall be counted.
  [a] ->
  -- | The item the occurrences of which in the list given in the first
  -- parameter shall be counted.
  a ->
  -- | The number of occurrences in the list given as first of the item given in
  -- the second parameter
  Int
count l s = length (filter (== s) l)

-- | A predicate returning True if the first element of the pair given as input
-- parameter is an not IP address.
pre ::
  -- | A pair. The first element is checked for being an IP address.
  (String, String) ->
  -- | True if the first element of the pair given as first parameter is an not
  -- IP address.
  Bool
pre s =
  case (runParser ipaddr () "" (fst s)) of
    Right _ -> False
    Left _ -> True

-- | A parser for a single integer digit.
intdigit :: Parser Int
intdigit =
  do
    a <- digit
    case reads [a] of
      [(i, [])] -> return i
      _ -> pzero

-- | A parser for a part of an IPv4 address, which is a three digits.
ipnum3 :: ParsecT String () Identity Int
ipnum3 =
  do
    a <- intdigit
    b <- intdigit
    c <- intdigit
    return (a * 100 + b * 10 + c)

-- | A parser for a part of an IPv4 address, which is two digits.
ipnum2 :: ParsecT String () Identity Int
ipnum2 =
  do
    a <- intdigit
    b <- intdigit
    return (a * 10 + b)

-- | A parser for a part of an IPv4 address, which is a single digit.
ipnum1 :: Parser Int
ipnum1 = do intdigit

-- | A parser for a part of an IPv4 address
ipnum :: ParsecT [Char] () Identity ()
ipnum =
  do
    n <- (try (ipnum3)) <|> (try (ipnum2)) <|> ipnum1
    if ((n <= 255) && (n >= 0)) then return () else pzero

-- | A parser for an IPv4 address or an IPv6 address.
ipaddr ::
  Text.Parsec.Prim.ParsecT
    [Char]
    ()
    Data.Functor.Identity.Identity
    ()
ipaddr = try (ipv4addr) <|> ipv6addr

-- | A parser for an IPv4 address.
ipv4addr ::
  Text.Parsec.Prim.ParsecT
    [Char]
    ()
    Data.Functor.Identity.Identity
    ()
ipv4addr =
  do
    _ <- ipnum
    _ <- char '.'
    _ <- ipnum
    _ <- char '.'
    _ <- ipnum
    _ <- char '.'
    _ <- ipnum
    return ()

-- | A parser for a four digit part of and IPv6 address.
ipv6num ::
  Text.Parsec.Prim.ParsecT
    [Char]
    ()
    Data.Functor.Identity.Identity
    ()
ipv6num =
  try ((ipv4addr))
    <|> do
      _ <- try (hexDigit) <|> return '0'
      _ <- try (hexDigit) <|> return '0'
      _ <- try (hexDigit) <|> return '0'
      _ <- try (hexDigit) <|> return '0'
      return ()

-- | A parser for an IPv6 address.
ipv6addr ::
  Text.Parsec.Prim.ParsecT
    [Char]
    ()
    Data.Functor.Identity.Identity
    ()
ipv6addr =
  do
    _ <- try (ipv6num) <|> return ()
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
