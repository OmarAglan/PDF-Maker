{-DHUN| helper functions to convert from parse tree notation to latex notation DHUN-}
module WikiLinkHelper where
import Tools
import qualified Data.Map as Map
import qualified Data.Set as Set
import MagicStrings
import MyState
import MediaWikiParseTree
import Data.List
import Network.HTTP
import WikiHelper
import Data.Char
import Data.List.Split
import Control.Monad

{-DHUN| takes a list of language prefixes and sister project prefixes and return a the first one or two elements of that list as tuple of Strings wrapped into maybe monads, returns nothing at the appropriate elements to the tuple if the list does not have enough elements. The first element of the tuple is populated first DHUN-}

getprefixes :: [String] -> (Maybe String, Maybe String)
getprefixes ss
  = case ss of
        [] -> (Nothing, Nothing)
        (x : xs) -> (Just $ filter (not . isSpace) (map toLower x),
                     case xs of
                         [] -> Nothing
                         (y : _) -> Just $ filter (not . isSpace) (map toLower y))

{-DHUN| in the wiki source a heading is given by == foo bar == where the number of space gives the level of the heading. In latex this is done with string like 'section. This function translates from wiki notation to latex notation DHUN-}

getsec :: String -> String
getsec "=" = "chapter"
getsec "==" = "section"
getsec "===" = "subsection"
getsec "====" = "subsubsection"
getsec "=====" = "paragraph"
getsec "======" = "subparagraph"
getsec _ = "subparagraph"

{-DHUN| in the wiki source a heading is given by == foo bar == where the number of space gives the level of the heading. In latex this is done with string like 'section'. Some of with need additional commands after the actual heading command. Those are returned by this function  DHUN-}

getsecpost :: String -> String
getsecpost "=" = "\n\\myminitoc\n"
getsecpost "==" = ""
getsecpost "===" = ""
getsecpost "====" = ""
getsecpost "=====" = "{$\\text{ }$}\\newline"
getsecpost "======" = "{$\\text{ }$}\\newline"
getsecpost _ = "{$\\text{ }$}\\newline"

{-DHUN| a predicate that returns true if the string is a valid image size in wiki notation. In the wiki an image is include like [[Image:FoorBar.png|300px]] where 300px means that the image should be displayed at a width of 300 pixel. This function takes strings and looks whether it looks like an integer number flowed by 'px' and is thus a valid width definition for an image in wiki notation DHUN-}

isImageSize :: String -> Bool
isImageSize x
  = if (isSuffixOf "px" x) then
      if (reads (take ((length x) - 2) x)) == ([] :: [(Float, [Char])])
        then False else True
      else False

{-DHUN| this predicate returns true if the string seems to be a caption of an image in wiki notation. And image give like [[Image:JohnDow.png|foo bar]] where 'foo bar' is the caption of the image. So this function just tests whether the string looks like one of the reserved keyword of mediawiki or like an image size definition (see function isImageSize) and return true if none of that fits DHUN-}

isCaption :: String -> Bool
isCaption x
  = if isImageSize x then False else
      if x `elem` ["thumb", "right", "left", "center"] then False else
        True

{-DHUN| escapes all charters of a string for use a a link with the hyperref package in latex DHUN-}

escapelink :: String -> String
escapelink s = concat (map chartransforlink s)

{-DHUN| analyzes a host name. If it belongs to a wikimedia project it returns an UrlInfo value otherwise it returns a WikiBaseUrl value. In any case it is the type WikiUrlData. This information is needed when building writing down links to subpages in latex or downloading images from the wiki. DHUN-}

analyseNetloc :: String -> WikiUrlData
analyseNetloc nl = myurldata
  where langm
          = do p <- prefix
               guard $ p `elem` foreignPrefixes
               guard $ (length splits) > 1
               prefix
        wtypem
          = case langm of
                Nothing -> do p <- prefix
                              guard $ (length splits) > 1
                              guard $ Set.member p wikiset
                              prefix
                Just _ -> do guard $ (length splits) > 2
                             p2 <- prefix2
                             guard $ Set.member p2 wikiset
                             prefix2
        myurldata
          = case (langm, wtypem) of
                (Just lang, Just wtype) -> UrlInfo
                                             (WikiUrlInfo{language = lang, wikitype = wtype})
                _ -> BaseUrl (WikiBaseUrl nl)
        splits = splitOn "." nl
        (prefix, prefix2) = getprefixes splits
        wikiset = Set.fromList . Map.elems . Map.fromList $ multilangwikis

{-DHUN| splits a wikilink in caption and link target and returns the target. So a wkilink is given like [[FooBar|John Dow]]. This link links to the lemma FooBar and is displayed with the caption 'John Dow'. So this function returns FooBar for that link. DHUN-}

localWikiLinkLocation :: String -> String
localWikiLinkLocation s = headSplitEq '|' s

{-DHUN| This function takes the contend of a wikilink in wiki markup notation as first input parameter in parse tree notation. It takes the state of the Latex Renderer as second input parameter. It returns the absolute url of the given wikilink as string DHUN-}

wikiLinkLocationesc :: [Anything Char] -> MyState -> String
wikiLinkLocationesc l st
  = getUrlFromWikiLinkInfoesc .
      getWikiLinkInfo (shallowFlatten l) . urld
      $ st

{-DHUN| converts a WikiLinkInfo value pointing to a page on a wiki to the full url of that page as string. It is escaped for the use with the hyperref latex package DHUN-}

getUrlFromWikiLinkInfoesc :: WikiLinkInfo -> String
getUrlFromWikiLinkInfoesc i
  = case (urldata i) of
        UrlInfo x -> "https://" ++
                       (language x) ++
                         "." ++
                           (wikitype x) ++ ".org/wiki/" ++ (escapelink (urlEncode (page i)))
        BaseUrl (WikiBaseUrl x) -> "https://" ++
                                     x ++ "/wiki/" ++ (escapelink (urlEncode (page i)))

{-DHUN| converts a WikiLinkInfo value pointing to a page on a wiki to the full url of that page as string. It is not is escaped in any way, so ready for use in a webbrowers DHUN-}

getUrlFromWikiLinkInfo :: WikiLinkInfo -> String
getUrlFromWikiLinkInfo i
  = case (urldata i) of
        UrlInfo x -> "https://" ++
                       (language x) ++
                         "." ++ (wikitype x) ++ ".org/wiki/" ++ (urlEncode (page i))
        BaseUrl (WikiBaseUrl x) -> "https://" ++
                                     x ++ "/wiki/" ++ (urlEncode (page i))

{-DHUN| converts a WikiUrlData value pointing to a wiki and a string (like a part of a url) pointing to a page relative to that wiki and returns the full url of that page as string DHUN-}

wikiUrlDataToString :: WikiUrlData -> String -> String
wikiUrlDataToString w i
  = case w of
        UrlInfo x -> "https://" ++
                       (language x) ++ "." ++ (wikitype x) ++ ".org" ++ i
        BaseUrl (WikiBaseUrl x) -> "https://" ++ x ++ i

{-DHUN| This function takes the contend of a wikilink in wiki markup notation as first input parameter. So for the wiki link (mediawiki markup notation) [[en:JohnDow|John Dow]] this is 'en:JohnDow|John Dow' (the single quotes were added to make the string distinguishable from the text and are not part of the passed parameter). It takes the WikiUrlData value describing the wiki the page currently being processed belongs to as second parameter. It returns a WikiLinkInfo value describing the target of the link given as first parameter when evaluated with respect to the second parameter DHUN-}

getWikiLinkInfo :: String -> WikiUrlData -> WikiLinkInfo
getWikiLinkInfo s i = WikiLinkInfo{urldata = udata, page = pagen}
  where udata
          = case (langmm, wtypemm) of
                (Just l, Just w) -> UrlInfo WikiUrlInfo{language = l, wikitype = w}
                _ -> i
        wtypem
          = do p <- prefix
               guard $ (length splits) > 1
               Map.lookup p (Map.fromList allwikis)
        langm
          = case wtypem of
                Nothing -> do p <- prefix
                              _ <- find (== p) foreignPrefixes
                              guard $ (length splits) > 1
                              prefix
                Just _ -> do p2 <- prefix2
                             _ <- find (== p2) foreignPrefixes
                             guard $ (length splits) > 2
                             prefix2
        langmm
          = case langm of
                Nothing -> langi
                x -> x
        wtypemm
          = case wtypem of
                Nothing -> wtypei
                x -> x
        langi
          = case i of
                UrlInfo ui -> Just (language ui)
                BaseUrl _ -> Nothing
        wtypei
          = case i of
                UrlInfo ui -> Just (wikitype ui)
                BaseUrl _ -> Nothing
        pagen = intercalate ":" ((mapn langm) . (mapn wtypem) $ splits)
        mapn h
          = case h of
                Nothing -> id
                Just _ -> drop 1
        splits = splitOn ":" (headSplitEq '|' s)
        (prefix, prefix2) = getprefixes splits
