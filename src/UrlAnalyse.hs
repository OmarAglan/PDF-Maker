{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | module for processing URLs and downloading their content with respect to
-- MediaWiki
module UrlAnalyse where

import Codec.Binary.UTF8.String
import Control.Exception
import Control.Exception as X
import Control.Monad
import qualified Data.ByteString as BStr
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal
import qualified Data.ByteString.UTF8 as UTF8Str
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Serialize
import Data.Tree.NTree.TypeDefs
import GHC.Generics
import Network.HTTP.Client
import qualified Network.HTTP.Client.Internal as I
import Network.HTTP.Client.TLS
import qualified Network.HTTP.Types as T
import qualified Network.HTTP.Types.Version as V
import Network.URL as URL
import Text.XML.HXT.Core
import Tools (replace2)

deriving instance Read URL.URL

deriving instance Read URL.Host

deriving instance Read URL.Protocol

deriving instance Read URL.URLType

deriving instance Serialize URL

deriving instance Generic URL

deriving instance Serialize URLType

deriving instance Generic URLType

deriving instance Serialize Host

deriving instance Generic Host

deriving instance Serialize Protocol

deriving instance Generic Protocol

-- | This represents the main URL of a wiki page. So the URL of the wiki page
-- that should be converted to latex. It is a tuple wrapped into the maybe
-- monad, to deal with case in which the URL could not be parsed. The first
-- element of the tuple is just the main URL parsed with Network.URL The second
-- element is a list of URLs. These URLs are possible base URLs for wiki pages.
-- So https://en.wikipedia.org/wiki/Foobar has got the main URL
-- https://en.wikipedia.org/wiki/Foobar and one of the base URLs is
-- https://en.wikipedia.org/wiki/ and an other base URL is
-- https://en.wikipedia.org . Base URLs are important since the wiki source
-- related to the main URL might include sub page in wiki notation that is
-- [[John_Dow]]. The actual URL to look up John_Dow is the baseurl plus John_Dow
-- so that is https://en.wikipedia.org/wiki/John_Dow . Since also images in the
-- commons and similar things are possible, there are usually some baseurls to
-- be looked at. This way the baseurls have to be a list.
type WikiUrl = Maybe (URL, [URL])

-- | Converts a `FullWikiUrl` to a `WikiUrl`
wikiUrl ::
  -- | The `FullWikiUrl` to be converted
  FullWikiUrl ->
  -- | Returns a `WikiUrl` pointing to the same article as the `FullWikiUrl`
  -- given in the first parameter
  WikiUrl
wikiUrl fu = return (url fu, alternatives fu)

-- | A type describing a reference to an article on a MediaWiki server. The
-- entry `url` is the URL under which the article is located. The entry
-- `alternatives` is a list of baseurls of the wiki. See documentation on type
-- 'WikiUrl' for more information about baseurls. The entry `hostname` contains
-- the hostname of the server. The entry lemma contains the lemma (that is the
-- name of the article on the wiki)
data FullWikiUrl = FullWikiUrl
  { -- | URL where the wiki article is located
    url :: URL,
    -- | List of base URLs where to look for neighboring articles. See
    -- documentation on type `WikiUrl` for more information on base URLs .
    alternatives :: [URL],
    -- | The host name of the server the wiki is running on. For example
    -- en.wikipedia.org .
    hostname :: String,
    -- | The lemma on the wiki the URL points to. For example the URL
    -- https://en.wikipedia.org/wiki/John_Dow has the lemma John_Dow .
    lemma :: String
  }
  deriving (Eq, Ord, Serialize, Generic, Show)

-- | base instance of type `FullWikiUrl`, to be filled with useful data using
-- the record syntax.
fullWikiUrlZero ::
  -- | empty instance of the type `FullWikiUrl`.
  FullWikiUrl
fullWikiUrlZero =
  FullWikiUrl
    { url =
        URL
          { URL.url_type =
              Absolute
                ( Host
                    { URL.protocol = HTTP True,
                      URL.host = "",
                      URL.port = Nothing
                    }
                ),
            url_path = "",
            url_params = []
          },
      alternatives = [],
      hostname = "",
      lemma = ""
    }

-- | returns the list of baseurls of an WikiUrl. The list may be empty if the
-- WikiUrl has none. See documentation on the type `WikiUrl` to understand what
-- a baseurl is.
parses ::
  -- | The `WikiUrl` to be converted to a list of its baseurls.
  WikiUrl ->
  -- | Returns the list of baseurls of the the `WikiUrl` given as first
  -- parameter. Returns the empty list if the list of baseurls could not be
  -- determined.
  [URL]
parses u =
  do
    uu <- maybeToList u
    snd uu

-- | takes a baseurl and creates the beginning URL under which the wiki source
-- code any lemma can be retrieved. So the URL
-- https://en.wikipedia.org/wiki/FoorBar has the baseurls
-- https://en.wikipedia.org and https://en.wikipedia.org/wiki . The wiki source
-- of the lemma John_Dow can be retrieved from the wiki via the URL
-- https://en.wikipedia.org/wiki/Special:Export/John_Dow. This function return
-- the beginning of an URL where the wiki source code can be downloaded. In our
-- example it returns https://en.wikipedia.org/wiki/Special:Export. See also
-- documentation on the type `WikiUrl` on what a baseurl is.
modpath ::
  -- | the baseurl to the wiki
  URL ->
  -- | the beginning of an URL to download the wiki source code from.
  URL
modpath u =
  u
    { url_path =
        if p /= []
          then p ++ "/Special:Export"
          else "Special:Export"
    }
  where
    p = (url_path u)

-- | takes a baseurl an return the beginning of the URL of the special page on
-- the wiki to expand the templates useful for command line option -m. See
-- documentation on type `WikiUrl` for more information on what a baseurl is.
-- For example the base url https://en.wikipedia.org/wiki/ get converted to
-- https://en.wikipedia.org/wiki/Special:ExpandTemplates
modpathForExpansion ::
  -- | the baseurl to the wiki
  URL ->
  -- | the beginning of an Url to expand the templates on the wiki
  URL
modpathForExpansion u =
  u
    { url_path =
        ( if p /= []
            then p ++ "/Special:ExpandTemplates"
            else "Special:ExpandTemplates"
        ),
      url_params = []
    }
  where
    p = (url_path u)

-- | Loads a web page via https. Returns a String.
geturl ::
  -- | URL of the web page to be downloaded.
  String ->
  -- | The result of the request is returned as a String wrapped in the IO
  -- monad.
  IO String
geturl u =
  if u == ""
    then return ([])
    else
      Control.Exception.catch
        ( do
            req1 <- parseRequest u
            let req0 = req1 {requestHeaders = (T.hUserAgent, UTF8Str.fromString "mediawiki2latex") : (requestHeaders req1)}
            let req =
                  ( ( urlEncodedBody
                        ( map
                            (\(a, b) -> (UTF8Str.fromString a, UTF8Str.fromString b))
                            []
                        )
                    )
                      req0
                  )
                    { method = method req0
                    }
            manager <- newManager tlsManagerSettings
            res <- (httpLbs req manager) `X.catch` (statusExceptionHandler req)
            return ((unpackChars (responseBody res)))
        )
        fun
  where
    fun :: ErrorCall -> IO String
    fun _ = return ""
    statusExceptionHandler :: I.Request -> SomeException -> IO (Network.HTTP.Client.Response L.ByteString)
    statusExceptionHandler r _ = (return (I.Response {I.responseOriginalRequest = r, responseBody = L.empty, responseStatus = T.Status {T.statusCode = 200, T.statusMessage = BStr.empty}, responseVersion = V.http09, responseHeaders = [], responseCookieJar = I.CJ [], I.responseClose' = I.ResponseClose (return ())}))

-- | Loads the data stored under an URL from the web. Returns a ByteString.
-- Mainly useful for loading HTML for further processing, as well as binary
-- image files.
geturl2 ::
  -- | the URL to download the content from.
  String ->
  -- | the result of the request wrapped into the IO monad.
  IO BStr.ByteString
geturl2 uuu =
  let u = replace2 uuu "%20" "_"
   in if u == ""
        then return (BStr.pack [])
        else
          Control.Exception.catch
            ( do
                req1 <- parseRequest u
                let req0 = req1 {requestHeaders = (T.hUserAgent, UTF8Str.fromString "mediawiki2latex") : (requestHeaders req1)}
                let req =
                      ( ( urlEncodedBody
                            ( map
                                (\(a, b) -> (UTF8Str.fromString a, UTF8Str.fromString b))
                                []
                            )
                        )
                          req0
                      )
                        { method = method req0
                        }
                manager <- newManager tlsManagerSettings
                res <- (httpLbs req manager) `X.catch` (statusExceptionHandler req)
                return (L.toStrict (responseBody res))
            )
            fun
  where
    fun :: ErrorCall -> IO BStr.ByteString
    fun _ = return (BStr.pack [])
    statusExceptionHandler :: I.Request -> SomeException -> IO (Network.HTTP.Client.Response L.ByteString)
    statusExceptionHandler r _ = (return (I.Response {I.responseOriginalRequest = r, responseBody = L.empty, responseStatus = T.Status {T.statusCode = 200, T.statusMessage = BStr.empty}, responseVersion = V.http09, responseHeaders = [], responseCookieJar = I.CJ [], I.responseClose' = I.ResponseClose (return ())}))

-- | loads the wiki source code stored under a lemma in on a server running
-- MediaWiki.
geturl4 ::
  -- | the lemma for which the wiki source code shall be downloaded
  String ->
  -- | the beginning of the URL to the special:export page on the wiki
  String ->
  -- | the wiki source code of the wiki article at the given lemma
  IO String
geturl4 s u =
  if u == ""
    then return ([])
    else
      Control.Exception.catch
        ( do
            req2 <- parseRequest (u ++ "/" ++ s)
            let req1 = req2 {requestHeaders = (T.hUserAgent, UTF8Str.fromString "mediawiki2latex") : (requestHeaders req2)}

            let req0 = req1 {queryString = UTF8Str.fromString "", path = if (head . reverse $ s) == '?' then (UTF8Str.fromString . (++ "%3F") . UTF8Str.toString) (path req1) else path req1}
            let req =
                  ( ( urlEncodedBody
                        ( map
                            (\(a, b) -> (UTF8Str.fromString a, UTF8Str.fromString b))
                            [("mw-input-pages", s), ("curonly", "1"), ("wpExportTemplates", "0"), ("wpDownload", "1")]
                        )
                    )
                      req0
                  )
            manager <- newManager tlsManagerSettings
            res <- (httpLbs req manager) `X.catch` (statusExceptionHandler req)
            return ((unpackChars (responseBody res)))
        )
        fun
  where
    fun :: ErrorCall -> IO String
    fun _ = return ""
    statusExceptionHandler :: I.Request -> SomeException -> IO (Network.HTTP.Client.Response L.ByteString)
    statusExceptionHandler r _ = (return (I.Response {I.responseOriginalRequest = r, responseBody = L.empty, responseStatus = T.Status {T.statusCode = 200, T.statusMessage = BStr.empty}, responseVersion = V.http09, responseHeaders = [], responseCookieJar = I.CJ [], I.responseClose' = I.ResponseClose (return ())}))

-- | Loads the wiki source code of a wiki article from a server running
-- MediaWiki, with all MediaWiki templates expanded into wiki source code.
geturl3 ::
  -- | beginning of the URL to Special:Expand page on the server.
  String ->
  -- | the wiki source code including the templates which should be expanded.
  String ->
  -- | the name of the lemma on the wiki server where the wiki source code given
  -- in the second parameter is hosted on the MediaWiki server.
  String ->
  -- | The wiki source code given in the second parameter with all templates
  -- expanded by MediaWiki.
  IO String
geturl3 u d s =
  if u == ""
    then return ([])
    else
      Control.Exception.catch
        ( do
            req1 <- parseRequest u
            let req0 = req1 {requestHeaders = (T.hUserAgent, UTF8Str.fromString "mediawiki2latex") : (requestHeaders req1)}
            let req =
                  ( urlEncodedBody
                      ( map
                          (\(a, b) -> (UTF8Str.fromString a, UTF8Str.fromString b))
                          [ ("wpInput", d),
                            ("removecomments", "1"),
                            ("removenowiki", "1"),
                            ("generate_xml", "0"),
                            ("contexttitle", s)
                          ]
                      )
                  )
                    req0
            manager <- newManager tlsManagerSettings
            res <- (httpLbs req manager) `X.catch` (statusExceptionHandler req)
            return ((unpackChars (responseBody res)))
        )
        fun
  where
    fun :: ErrorCall -> IO String
    fun _ = return ""
    statusExceptionHandler :: I.Request -> SomeException -> IO (Network.HTTP.Client.Response L.ByteString)
    statusExceptionHandler r _ = (return (I.Response {I.responseOriginalRequest = r, responseBody = L.empty, responseStatus = T.Status {T.statusCode = 200, T.statusMessage = BStr.empty}, responseVersion = V.http09, responseHeaders = [], responseCookieJar = I.CJ [], I.responseClose' = I.ResponseClose (return ())}))

-- | Helper function to get the actual wiki source as string out of a part of
-- and xml tree returned by the xml parser. Only used for the function
-- getTextContent.
toText ::
  -- | the XML tree (returned by the MediaWiki server) to extract the wiki
  -- source code from.
  [NTree XNode] ->
  -- | the wiki source code extracted from the XML tree.
  Maybe String
toText [NTree _ [NTree (XText l) []]] = Just l
toText _ = Nothing

-- | This function gets the actual wiki source of a wiki page out of the result
-- String returned by the Special:Export function of mediawiki. You should not
-- call this function directly since it may break the flow of control. Better
-- use the function getTextContent2.
getTextContent ::
  -- | the content from the MediaWiki servers export page containing the wiki
  -- source code
  String ->
  -- | Returns the Just value of the Maybe monad containing the wiki source code
  -- extracted from the response of the MediaWiki server given as first
  -- parameter if it could be extracted. Returns the Nothing value of the Maybe
  -- monad otherwise.
  IO (Maybe String)
getTextContent z =
  do
    h <-
      runX
        ( (readString [withValidate no, withParseHTML yes] z)
            >>> (deep (isElem >>> hasName "text"))
        )
    x <- return . toText $ h
    return (seq x x)

-- | This function gets the actual wiki source of a wiki page out of the result
-- String returned by the Special:Export function of mediawiki. This function
-- returns its result wrapped in a maybe monad so it can return the maybe value
-- Nothing in case of failure but does not break the flow of control. It is also
-- wrapped in the IO monad since the xml parser used is bound to the IO monad.
getTextContent2 ::
  -- | the content from the MediaWiki servers export page containing the wiki
  -- source code
  String ->
  -- | Returns the Just value of the Maybe monad containing the wiki source code
  -- extracted from the response of the MediaWiki server given as first
  -- parameter if it could be extracted. Returns the Nothing value of the Maybe
  -- monad otherwise.
  IO (Maybe String)
getTextContent2 z =
  catchJust mycatchfun (getTextContent z) (\_ -> return Nothing)

-- | This function extracts the expanded wiki source of out of the result String
-- returned by the Special:ExpandTemplates function of MediaWiki. This function
-- returns its result wrapped in a maybe monad so it can return the maybe value
-- Nothing in case of failure but does not break the flow of control. It is also
-- wrapped in the IO monad since the XML parser used is bound to the IO monad.
-- You should not call this function directly since it may break the flow of
-- control. Better use the function getExpandedTextContent2.
getExpandedTextContent ::
  -- | The String returned MediaWiki servers Special:ExpandTemplates page.
  String ->
  -- | Returns a Just value of the Maybe monad containing the wiki source code
  -- with all templates expanded by the MediaWiki Server if it could be
  -- extracted. Returns the Nothing value of the Maybe monad otherwise.
  IO (Maybe String)
getExpandedTextContent z =
  do
    h <-
      runX
        ( (readString [withValidate no, withParseHTML yes] z)
            >>> ( deep
                    ( isElem
                        >>> hasName "textarea"
                        >>> (hasAttrValue "id" (\g -> g == "output"))
                    )
                )
        )
    x <- return . toText $ h
    return (seq x x)

-- | this function extracts the expanded wiki source of out of the result String
-- returned by the Special:ExpandTemplates function of mediawiki. This function
-- returns its result wrapped in a maybe monad so it can return the maybe value
-- Nothing in case of failure but does not break the flow of control. It is also
-- wrapped in the IO monad since the XML parser used is bound to the IO monad.
-- Possible IO errors are caught an rethrown as Nothing in the Maybe Monad.
getExpandedTextContent2 ::
  -- | The String returned MediaWiki servers Special:ExpandTemplates page.
  String ->
  -- | Returns a Just value of the Maybe monad containing the wiki source code
  -- with all templates expanded by the MediaWiki Server if it could be
  -- extracted. Returns the Nothing value of the Maybe monad otherwise.
  IO (Maybe String)
getExpandedTextContent2 z =
  catchJust
    mycatchfun
    (getExpandedTextContent z)
    (\_ -> return Nothing)

-- | exception predicate interested in all exceptions. Thats means we catch all
-- exceptions. This needed in getTexTContent2 and getExpandedTextContent2 so
-- that the maybe value nothing is returned in case of an exception but the
-- control flow is not interrupted.
mycatchfun ::
  -- | The Exception to be caught
  SomeException ->
  -- | return a Just value of the Maybe Monad containing the void type
  Maybe ()
mycatchfun _ = return ()

-- | gets the wiki source code of a lemma on a wiki.
getpage ::
  -- | the lemma on the wiki (example: John_Dow for
  -- https://en.wikipedia.org/wiki/John_Dow ).
  String ->
  -- | a 'WikiUrl' specifying from which wiki the data shall be downloaded. Hint
  -- a 'WikiUrl' can be created by calling the function 'analyse' .
  WikiUrl ->
  -- | returns the wiki source code of the lemma as a Just value of the Maybe
  -- monad if it could be retrieved, otherwise returns the Nothing value of the
  -- Maybe monad in case of failure. The return value is wrapped in the IO monad
  -- in any case since the download requires an IO action.
  IO (Maybe String)
getpage sss u =
  do
    let ss = replace2 (replace2 sss " " "_") "%" "%25"
    l <- mapM ((geturl4 ss) . unify . exportURL . modpath) (parses u)
    ll <- mapM getTextContent2 l
    lll <- return (seq ll ll)
    return $
      (listToMaybe $ concat (map maybeToList lll))
        >>= (return . decodeString)

-- | Loads a page from a wiki when mediawiki2latex is running with command line
-- option --bookmode. Loads the HTML (generated by MediaWiki) for a lemma from a
-- wiki pointed to by a `WikiUrl`
getBookpage ::
  -- |  The lemma to load from the wiki.
  String ->
  -- | The `WikiUrl` to the MediaWiki server hosting the wiki.
  WikiUrl ->
  -- | The HTML for the given lemma returned by the MediaWiki server specified
  -- by the given `WikiUrl`.
  IO (Maybe String)
getBookpage ss u =
  do
    l <- mapM ((geturl2) . unify) ((map attach) (map exportURL (parses u)))
    lll <- return (seq l l)
    return $
      (listToMaybe $ concat (map maybeToList (map go lll)))
        >>= (return)
  where
    go x = if (x == (UTF8Str.fromString [])) then Nothing else Just (UTF8Str.toString x)
    attach x = case reverse x of
      '/' : xs -> ((reverse xs) ++ ("/" ++ ss))
      xs -> ((reverse xs) ++ ("/" ++ ss))

-- | Loads the wiki source code of an article form a MediaWiki server when
-- mediawiki2latex is running with the --mediawiki option. This function
-- downloads the original wiki source code without expanding the templates. The
-- templates are going to be expanded later by call to `getExpandedPage`.
getpage2 ::
  -- | The lemma to load .
  String ->
  -- | The `WikiUrl` to the MediaWiki server hosting the wiki.
  WikiUrl ->
  -- | Return a Just value of the Maybe monad containing a pair if successful.
  -- The first element of the pair is the wiki source of the article. The second
  -- element of it is the URL under which the article was downloaded. In case of
  -- failure the Nothing value of the Maybe monad is returned.
  IO (Maybe (String, URL))
getpage2 sss u =
  do
    let ss = replace2 (replace2 sss " " "_") "%" "%25"
    l <- mapM ((geturl4 ss) . unify . exportURL . modpath) (parses u)
    ll <- mapM getTextContent2 l
    lll <- return (seq ll ll)
    return $ (listToMaybe $ concat (map go (zip lll (parses u))))
  where
    go (Just xx, uu) = [(decodeString xx, uu)]
    go _ = []

-- | This function expands all templates in a wikitext source using MediaWiki.
getExpandedPage ::
  -- | The lemma to be processed.
  String ->
  -- | The wiki source code of the article stored under the lemma.
  String ->
  -- | The third parameter is URL to Special:ExpandTemplates page on the
  -- mediawiki server.
  URL ->
  -- | Returns a Just value of the Maybe monad containing the wiki source code
  -- with all templates expanded by MediaWiki if successful. Return the Nothing
  -- value of the Maybe Monad otherwise.
  IO (Maybe String)
getExpandedPage ss d u =
  do
    l <-
      mapM
        ((\x -> geturl3 x d ss) . unify . exportURL . modpathForExpansion)
        [u]
    ll <- mapM getExpandedTextContent2 l
    lll <- return (seq ll ll)
    return $
      (listToMaybe $ concat (map maybeToList lll))
        >>= (return . decodeString)

-- | unescapes the special character underscore and % from url notation.
unify ::
  -- | The string to be unescaped
  String ->
  -- | Returns the unescaped string
  String
unify ('%' : ('2' : ('0' : xs))) = '_' : unify xs
unify ('%' : ('2' : ('5' : xs))) = '%' : unify xs
unify (x : xs) = x : (unify xs)
unify [] = []

-- | converts an url given as String into a 'WikiUrl'. See description on type
-- 'WikiUrl' on what that means.
analyse ::
  -- | a string containing an URL to be converted into a 'WikiUrl' for further
  -- processing.
  String ->
  -- | return the 'WikiUrl'.
  WikiUrl
analyse s =
  do
    vv <- v
    ww <- importURL "https://commons.wikimedia.org/wiki"
    return (vv, (reverse z) ++ [ww])
  where
    v = importURL s

    z :: [URL]
    z = do
      u <- maybeToList $ v
      l <- return $ splitOn "/" $ (unify (url_path u))
      x <-
        ( map
            ( \i ->
                if (length l) > i then [intercalate "/" (take i l)] else mzero
            )
            [2, 0, 1]
          )
      map (\i -> u {url_path = i}) x

-- | converts an URL given as String into the lemma it points to on the wiki.
-- For example the URL https://en.wikipedia.org/wiki/John_Dow gets converted to
-- John_Dow
getLemma ::
  -- | The URL to be converted to a lemma.
  String ->
  -- | Returns the lemma as a Just value of the Maybe monad, if the conversion
  -- was successful or the Nothing value of the Maybe monad if the conversion
  -- was not possible.
  Maybe String
getLemma s = z
  where
    v = importURL s

    z :: Maybe String
    z = do
      u <- v
      l <- return $ splitOn "/" $ (unify (url_path u))
      let x = if (length l) > 1 then drop 1 l else l
      let xx =
            if "index.php" `elem` x
              then case dropWhile (/= "index.php") x of
                (_ : ys) -> ys
                _ -> []
              else x
      return $ intercalate "/" xx

-- | converts an URL given as String into the host (as string) it points to on
-- the wiki.
getHost ::
  -- | The URL to the wiki article.
  String ->
  -- | The hostname of the wiki server the URL given as first parameter points
  -- to.
  Maybe String
getHost s = z
  where
    v = importURL s

    z :: Maybe String
    z = do
      u <- v
      case (url_type u) of
        Absolute h -> return (URL.host h)
        _ -> mzero

-- | Parse an URL supplied as string in the first parameter into a 'FullWikiUrl'
-- which is returned. See documentation on the types 'WikiUrl' and 'FullWikiUrl'
-- for more information.
analyseFull ::
  -- | String representation of and the URL to be analyzed
  String ->
  -- | A Just value of the Maybe monad containing a 'FullWikiUrl' if the URL
  -- could be parsed successfully or a Nothing value of the Maybe monad if it
  -- could not.
  Maybe FullWikiUrl
analyseFull theUrl =
  do
    ana <- analyse theUrl
    l <- getLemma theUrl
    h <- getHost theUrl
    return $
      FullWikiUrl
        { url = fst ana,
          alternatives = snd ana,
          hostname = h,
          lemma = l
        }
