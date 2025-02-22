{-# LANGUAGE DefaultSignatures, DeriveAnyClass, DeriveGeneric, StandaloneDeriving #-}
{-DHUN| module for processing urls and downloading their content with repect to mediawiki DHUN-}
module UrlAnalyse
       (getpage, analyse, analyseFull, unify, WikiUrl, getLemma,
        FullWikiUrl, hostname, url, alternatives, lemma, wikiUrl, geturl,
        parses, geturl2, fullWikiUrlZero, getExpandedPage, getpage2, getBookpage)
       where
import Network.HTTP.Client
import Data.ByteString.Lazy.Internal
import Network.URL as URL
import Control.Monad
import Data.Maybe
import Data.List.Split
import Data.List
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs
import Codec.Binary.UTF8.String
import Control.Exception
import qualified Network.HTTP.Client.Internal as I
import qualified Data.ByteString.Lazy as L
import Control.Exception as X
import qualified Data.ByteString as BStr
import qualified Data.ByteString.UTF8 as UTF8Str
import qualified Network.HTTP.Types as T
import qualified Network.HTTP.Types.Version as V
import Data.Serialize
import GHC.Generics
import Network.HTTP.Client.TLS
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




{-DHUN| This represents the main url of a wiki page. So the url of the wiki page that should be converted to latex. It is a tuple wrapped into the maybe monad, to deal with case in which the url could not be parsed. The first element of the tuple is just the main url parsed with Network.URL The second element is a list of urls. These URLs are possible base urls for wiki pages. So en.wikipedia.org/wiki/Foobar has got the main url en.wikipedia.org/wiki/Foobar and one of the base urls us en.wikipedia.org/wiki/. Base urls are important since the wiki source related to the main url might include subpage in wiki notation that is [[JohnDow]]. The actual url to look up JohnDow is the baseurl plus JohnDow so that is en.wikipedia.org/wiki/JohnDow Since also images in the commons and similar things are possible, the are usually some baseurls to be looked at. This way the base URLs have to be a list. DHUN-}
 
type WikiUrl = Maybe (URL, [URL])
 
{-DHUN| same as WikiURL. two additional parameters. the host parameter is name of host. And lemma is the name of the page on the wiki DHUN-}
 
wikiUrl :: FullWikiUrl -> WikiUrl
wikiUrl fu = return (url fu, alternatives fu)
 
{-DHUN| A type describing a reference to an article on a MediaWiki server. The entry url is the url under which the article is located. The entry alternatives is a list of baseurls of the wiki. See documentation on type WikiUrl for more information about baseurls. The entry hostname contains the hostname of the server. The entry lemma contains the lemma (that is the name of the article on the wiki) DHUN-}


data FullWikiUrl = FullWikiUrl{url :: URL, alternatives :: [URL],
                               hostname :: String, lemma :: String}
                 deriving (Eq, Ord, Serialize, Generic, Show)
 

{-DHUN| base instance of type FullWikiUrl, to be filled with useful data using the record syntax DHUN-}

fullWikiUrlZero :: FullWikiUrl
fullWikiUrlZero
  = FullWikiUrl{url =
                  URL{URL.url_type =
                        Absolute
                          (Host{URL.protocol = HTTP True, URL.host = "",
                                URL.port = Nothing}),
                      url_path = "", url_params = []},
                alternatives = [], hostname = "", lemma = ""}
 
{-DHUN| returns the list of baseurls of an WikiURL. The list may be empty if the WikiURL has none. See documentation on the type 'WikiURL' to understand what a baseurl is. DHUN-}
 
parses :: WikiUrl -> [URL]
parses u
  = do uu <- maybeToList u
       snd uu
 
{-DHUN| takes a baseurl and a wiki lemma and creates the url under which the wiki source code of the lemma can be retrieved. So the Url en.wikipedia.org/wiki/FoorBar has possible many baseurls. One of which is  en.wikipedia.org/wiki/. The wiki source of the lemma JonDow can be retrieved from the wiki via the url en.wikipedia.org/wiki/Special:Export/JohnDow. Which is just what these function returns. See also documentation on the type 'WikiUrl' on what a baseurl is. DHUN-}
 
modpath :: URL -> URL
modpath u
  = u{url_path =
        if p /= [] then p ++ "/Special:Export" else
          "Special:Export"}
  where p = (url_path u)
 

{-DHUN| modify an URL to point to the special page on the wiki to expand the templates useful for command line option -m DHUN-}


modpathForExpansion :: URL -> URL
modpathForExpansion u
  = u{url_path =
        (if p /= [] then p ++ "/Special:ExpandTemplates" else
           "Special:ExpandTemplates"),
      url_params = []}
  where p = (url_path u)
 
{-DHUN| load a webpage via http the url to the webpage has to be given as string on the first input parameter. The result of the request is returned as a String wrapped in the IO monad. DHUN-}
 
geturl :: String -> IO String
geturl u
  = if u=="" then return ([]) else Control.Exception.catch
      (do req1 <- parseRequest u
          let req0= req1{requestHeaders=(T.hUserAgent,UTF8Str.fromString "mediawiki2latex"): (requestHeaders req1)} 
          let req
                = ((urlEncodedBody
                     (map (\ (a, b) -> (UTF8Str.fromString a, UTF8Str.fromString b))
                        []))
                    req0) {method=method req0}
          manager <- newManager tlsManagerSettings
          res <- (httpLbs req manager)`X.catch` (statusExceptionHandler req)
          return ((unpackChars (responseBody res))))
      fun
 where  
        fun :: ErrorCall -> IO String
        fun _ = return ""
        statusExceptionHandler ::  I.Request->SomeException -> IO (Network.HTTP.Client.Response L.ByteString)
        statusExceptionHandler r _ = (return (I.Response {I.responseOriginalRequest=r, responseBody=L.empty,responseStatus=T.Status {T.statusCode=200,T.statusMessage=BStr.empty}, responseVersion=V.http09,responseHeaders=[],responseCookieJar=I.CJ [],I.responseClose'=I.ResponseClose (return ())}))
 

{-DHUN| Loads the data stored under an URL from the web. Result will be a ByteString. Mainly useful for loading HTML for further processing, as well as binary image files. DHUN-}


geturl2 :: String -> IO BStr.ByteString
geturl2 u
  = if u=="" then return (BStr.pack []) else Control.Exception.catch
      (do req1 <- parseRequest u
          let req0= req1{requestHeaders=(T.hUserAgent,UTF8Str.fromString "mediawiki2latex"): (requestHeaders req1)} 
          let req
                = ((urlEncodedBody
                     (map (\ (a, b) -> (UTF8Str.fromString a, UTF8Str.fromString b))
                        []))
                    req0) {method=method req0}
          manager <- newManager tlsManagerSettings
          res <- (httpLbs req manager)`X.catch` (statusExceptionHandler req)
          return  (L.toStrict (responseBody res)))
      fun
 where  
        fun :: ErrorCall -> IO BStr.ByteString
        fun _ = return (BStr.pack [])
        statusExceptionHandler ::  I.Request->SomeException -> IO (Network.HTTP.Client.Response L.ByteString)
        statusExceptionHandler r _ = (return (I.Response {I.responseOriginalRequest=r,responseBody=L.empty,responseStatus=T.Status {T.statusCode=200,T.statusMessage=BStr.empty}, responseVersion=V.http09,responseHeaders=[],responseCookieJar=I.CJ [],I.responseClose'=I.ResponseClose (return ())}))

{-DHUN| loads the wiki sourcecode strored under a lemma in on a server running mediawiki. The first parameter is the lemma to look up. The second parameter is the URL to the special:export page on the server. The return value is the source wikitext DHUN-}


geturl4 :: String -> String -> IO String
geturl4 s u
  = if u=="" then return ([]) else Control.Exception.catch
      (do req2 <- parseRequest (u++"/"++s)
          let req1= req2{requestHeaders=(T.hUserAgent,UTF8Str.fromString "mediawiki2latex"): (requestHeaders req2)} 

          let req0 = req1 {queryString=UTF8Str.fromString "",path=if (head.reverse$ s)=='?' then (UTF8Str.fromString.(++"%3F").UTF8Str.toString) (path req1) else path req1}
          let req
                = ((urlEncodedBody
                     (map (\ (a, b) -> (UTF8Str.fromString a, UTF8Str.fromString b))
                        [("mw-input-pages",s),("curonly","1"),("wpExportTemplates","0"),("wpDownload","1")]))
                    req0) 
          manager <- newManager tlsManagerSettings
          res <- (httpLbs req manager)`X.catch` (statusExceptionHandler req)
          return ((unpackChars (responseBody res))))
      fun
 where  
        fun :: ErrorCall -> IO String
        fun _ = return ""
        statusExceptionHandler ::  I.Request->SomeException -> IO (Network.HTTP.Client.Response L.ByteString)
        statusExceptionHandler r _ = (return (I.Response {I.responseOriginalRequest=r,responseBody=L.empty,responseStatus=T.Status {T.statusCode=200,T.statusMessage=BStr.empty}, responseVersion=V.http09,responseHeaders=[],responseCookieJar=I.CJ [],I.responseClose'=I.ResponseClose (return ())}))


{-DHUN| loads the wikisource of a wiki article from a server running mediawiki, with all mediawiki templates expanded into wiki text. The first parameter is the url to special:expand templates page on the server. The second parameter is the wikitext source including the mediawiki templates to be expanded. The third parameter is the name of the lemma on the server. DHUN-}


geturl3 :: String -> String -> String -> IO String
geturl3 u d s
  = if u=="" then return ([]) else Control.Exception.catch
      (do req1 <- parseRequest u
          let req0= req1{requestHeaders=(T.hUserAgent,UTF8Str.fromString "mediawiki2latex"): (requestHeaders req1)} 
          let req
                = (urlEncodedBody
                     (map (\ (a, b) -> (UTF8Str.fromString a, UTF8Str.fromString b))
                        [("wpInput", d), ("removecomments", "1"), ("removenowiki", "1"),
                         ("generate_xml", "0"), ("contexttitle", s)]))
                    req0
          manager <- newManager tlsManagerSettings
          res <- (httpLbs req manager) `X.catch` (statusExceptionHandler req)
          return ((unpackChars (responseBody res))))
      fun
 where  
        fun :: ErrorCall -> IO String
        fun _ = return ""
        statusExceptionHandler ::  I.Request->SomeException -> IO (Network.HTTP.Client.Response L.ByteString)
        statusExceptionHandler r _ = (return (I.Response {I.responseOriginalRequest=r,responseBody=L.empty,responseStatus=T.Status {T.statusCode=200,T.statusMessage=BStr.empty}, responseVersion=V.http09,responseHeaders=[],responseCookieJar=I.CJ [],I.responseClose'=I.ResponseClose (return ())}))
 
 
{-DHUN| helper function to get the actual wiki source as string out of a part of and xml tree returned by the xml parser. Only used for the function getTextContent DHUN-}
 
toText :: [NTree XNode] -> Maybe String
toText [NTree _ [NTree (XText l) []]] = Just l
toText _ = Nothing
 
{-DHUN| this function gets the actual wiki source of a wiki page out of the result String returned by the Special:Export function of mediawiki. You should not call this function directly since it may break the flow of control. Better use the function getTextContent2 DHUN-}
 
getTextContent :: String -> IO (Maybe String)
getTextContent z
  = do h <- runX
              ((readString [withValidate no, withParseHTML yes] z) >>>
                 (deep (isElem >>> hasName "text")))
       x <- return . toText $ h
       return (seq x x)
 
{-DHUN| this function gets the actual wiki source of a wiki page out of the result String returned by the Special:Export function of mediawiki. This function returns its result wrapped in a maybe monad so it can return the maybe value Nothing in case of failure but does not break the flow of control. It is also wrapped in the IO monad since the xml parser used is bound to the IO monad  DHUN-}
 
getTextContent2 :: String -> IO (Maybe String)
getTextContent2 z
  = catchJust myfun (getTextContent z) (\ _ -> return Nothing)
 
{-DHUN| this function extracts the expanded wiki source of out of the result String returned by the Special:ExpandTemplates function of mediawiki. This function returns its result wrapped in a maybe monad so it can return the maybe value Nothing in case of failure but does not break the flow of control. It is also wrapped in the IO monad since the xml parser used is bound to the IO monad  DHUN-}

getExpandedTextContent :: String -> IO (Maybe String)
getExpandedTextContent z
  = do h <- runX
              ((readString [withValidate no, withParseHTML yes] z) >>>
                 (deep
                    (isElem >>> hasName "textarea" >>>
                       (hasAttrValue "id" (\ g -> g == "output")))))
       x <- return . toText $ h
       return (seq x x)
 
{-DHUN| this function extracts the expanded wiki source of out of the result String returned by the Special:ExpandTemplates function of mediawiki. This function returns its result wrapped in a maybe monad so it can return the maybe value Nothing in case of failure but does not break the flow of control. It is also wrapped in the IO monad since the xml parser used is bound to the IO monad. Possible IO errors are caught an rethrown as Nothing in the Maybe Monad  DHUN-}


getExpandedTextContent2 :: String -> IO (Maybe String)
getExpandedTextContent2 z
  = catchJust myfun (getExpandedTextContent z)
      (\ _ -> return Nothing)
 
{-DHUN| exception predicate interested in all exceptions. Thats means we catch all exceptions. This needed in getTexTContent2 so that the maybe value nothing is returned in case of an exception but the control flow is not interrupted DHUN-}
 
myfun :: SomeException -> Maybe ()
myfun _ = return ()
 
{-DHUN| gets the wiki source code of a lemma on a wiki. The first parameter is the lemma. So JohnDow for en.wikipedia/wiki/JohnDow. The second parameter is the wikiurl. A wikiurl specifies the wiki from which the data should be downloaded. See documentation on the type 'WikiUrl' for more information. Also see documentation on the function 'analyze' to see how to create a 'WikiUrl'. This function returns the wiki source code of the lemma as String it is wrapped in the IO monad since it does a http request it is also wrapped in the Maybe monad since it may not be able to retrieve the source. DHUN-}
 
getpage :: String -> WikiUrl -> IO (Maybe String)
getpage ss u
  = do l <- mapM ((geturl4 ss ). unify . exportURL . modpath) (parses u)
       ll <- mapM getTextContent2 l
       lll <- return (seq ll ll)
       return $
         (listToMaybe $ concat (map maybeToList lll)) >>=
           (return . decodeString)
 
{-DHUN| Loads a page from a wiki when mediawiki2latex is running with command line option --bookmode. The first parameter is the lemma to load from the wiki the second parameter is the WikiUrl to the server hosting the wiki DHUN-}


getBookpage :: String -> WikiUrl -> IO (Maybe String)
getBookpage ss u
  = do l <- mapM ((geturl2) . unify)  ((map attach) (map exportURL (parses u)))
       lll <- return (seq l l)
       return $
         (listToMaybe $ concat (map maybeToList (map go lll))) >>=
           (return)
  where
    go x =if (x==(UTF8Str.fromString [])) then Nothing else Just (UTF8Str.toString x)
    attach x = case reverse x of
                 '/':xs -> ((reverse xs)++("/"++ss))
                 xs-> ((reverse xs)++("/"++ss))

{-DHUN| Loads the wikitext of an article form a mediawiki server when mediawiki2latex is running with the --mediawiki option. This function downloads the orignial wikitext source without expanding the templates. This is going to happen later by call to getExpandedPage. The first parmeter is lemma to load. The second paramerter is the WikiUrl to the server hosting the wiki. The return value is a pair. The first element of it is the wikitext source of the article. The second element of it is the URL under which the article was downloaded DHUN-}

getpage2 :: String -> WikiUrl -> IO (Maybe (String, URL))
getpage2 ss u
  = do l <- mapM ((geturl4 ss) . unify . exportURL . modpath) (parses u)
       ll <- mapM getTextContent2 l
       lll <- return (seq ll ll)
       return $ (listToMaybe $ concat (map go (zip lll (parses u))))
  where go (Just xx, uu) = [(decodeString xx, uu)]
        go _ = []
 
{-DHUN| This function expands all templates in a wikitext source using MediaWiki. The first parameter is lemma to be processed. The second parameter is the wikitext source of the article stored under the lemma. The third parameter is url to Special:ExpandTemplates page on the mediawiki server. The return value is the wikitext source with all templates expanded by MediaWiki DHUN-}

getExpandedPage :: String -> String -> URL -> IO (Maybe String)
getExpandedPage ss d u
  = do l <- mapM
              ((\ x -> geturl3 x d ss) . unify . exportURL . modpathForExpansion)
              [u]
       ll <- mapM getExpandedTextContent2 l
       lll <- return (seq ll ll)
       return $
         (listToMaybe $ concat (map maybeToList lll)) >>=
           (return . decodeString)
 
{-DHUN| unescapes the special character underscore and % from url notation DHUN-}
 
unify :: [Char] -> [Char]
unify ('%' : ('2' : ('0' : xs))) = '_' : unify xs
unify ('%' : ('2' : ('5' : xs))) = '%' : unify xs
unify (x : xs) = x : (unify xs)
unify [] = []
 
{-DHUN| converts an url given as String into a WikiURL. See description on type 'WikiURL' on what that means. DHUN-}
 
analyse :: String -> WikiUrl
analyse s
  = do vv <- v
       ww <- importURL "https://commons.wikimedia.org/wiki"
       return (vv, (reverse z) ++ [ww])
  where v = importURL s
         
        z :: [URL]
        z = do u <- maybeToList $ v
               l <- return $ splitOn "/" $ (unify (url_path u))
               x <- (map
                       (\ i ->
                          if (length l) > i then [intercalate "/" (take i l)] else mzero)
                       [2, 0, 1])
               map (\ i -> u{url_path = i}) x
 
{-DHUN| converts an url given as String into the lemma it points to on the wiki.  DHUN-}
 
getLemma :: String -> Maybe String
getLemma s = z
  where v = importURL s
         
        z :: Maybe String
        z = do u <- v
               l <- return $ splitOn "/" $ (unify (url_path u))
               let x = if (length l) > 1 then drop 1 l else l
               let xx
                     = if "index.php" `elem` x then
                         case dropWhile (/= "index.php") x of
                             (_ : ys) -> ys
                             _ -> []
                         else x
               return $ intercalate "/" xx
 
{-DHUN| converts an url given as String into the host (as string) it points to on the wiki.  DHUN-}
 
getHost :: String -> Maybe String
getHost s = z
  where v = importURL s
         
        z :: Maybe String
        z = do u <- v
               case (url_type u) of
                   Absolute h -> return (URL.host h)
                   _ -> mzero

{-DHUN| Parse an URL supplied as string in the first parameter into a FullWikiUrl which is returned. See documentation on the types WikiUrl and FullWikiUrl for more information DHUN-}


analyseFull :: String -> Maybe FullWikiUrl
analyseFull theUrl
  = do ana <- analyse theUrl
       l <- getLemma theUrl
       h <- getHost theUrl
       return $
         FullWikiUrl{url = fst ana, alternatives = snd ana, hostname = h,
                     lemma = l}
