-- | module to download images form the wiki
module GetImages where

import Control.DeepSeq
import Control.Monad
import Control.Monad.State
import Data.ByteString as BStr (readFile, writeFile)
import qualified Data.ByteString as BStr
import qualified Data.ByteString.UTF8 as UTF8Str
import Data.List
import Data.List.Split (splitOn)
import Data.Map hiding (drop, filter, map, take)
import qualified Data.Map as Map
import Data.Maybe
import Data.Serialize as S (decode, encode)
import GHC.IO.Exception
import Hex
import HtmlParser (parseHtmlFast)
import ImperativeState
import MediaWikiParseTree
import MediaWikiParser
import Network.URL as URL
import SimpleContributors
import System.FilePath
import System.Process
import Tools
import UrlAnalyse
import WikiHelper

-- | Get an URL to a media description page on a wiki.
modpath2 ::
  -- | The name of the media file
  String ->
  -- | An URL to a Wiki currently being processed
  URL ->
  -- | The URL to the media decription page of the file on the wiki
  URL
modpath2 s u =
  u
    { url_path =
        if p /= [] then p ++ "/File:" ++ s else "/File:" ++ s
    }
  where
    pp = (url_path u)
    p = case reverse pp of
      ('/' : xs) -> (reverse xs)
      xs -> (reverse xs)

-- | Convert an URL to the original media file read from the media description
-- page of the media file to and ready to be passed to curl for download.
conv ::
  -- | A base URL see documentation on `WikiUrl` for details.
  URL ->
  -- | The URL read from the media description page.
  String ->
  -- | The URL ready to be used in curl for download.
  String
conv u s =
  if take 5 s == "http:"
    then s
    else
      if take 6 s == "https:"
        then "https:" ++ (drop 6 s)
        else
          if (take 2 s) == "//"
            then "https:" ++ s
            else
              ( replace2
                  ( replace2
                      ( exportURL
                          u
                            { url_path =
                                case s of
                                  ('/' : xs) -> xs
                                  _ -> s
                            }
                      )
                      "%27"
                      "'"
                  )
                  "%25"
                  "%"
              )

-- | Extract the URL to download a media file from the image description page of
-- the media file on the wiki
getImageUrl2 ::
  -- | A tuple the first element is the HTML version of the image description
  -- page of the media file on the wiki. The second element is a base URL to the
  -- wiki currently being processed (see see documentation on `WikiUrl` for
  -- details)
  (String, URL) ->
  -- | The URL to download the media file from the wiki server.
  Maybe String
getImageUrl2 (s, u) =
  (getImageUrl "fullImageLink" u s)
    `mplus` (getImageUrl "fullMedia" u s)

-- | Extract an URL from  to download a media file from the image description
-- page of the media file on the wiki for on key string that is close to the
-- place in the HTML page where the link is located. The method this function
-- uses is also called HTML scraping. So it does not a full parse of HTML page
-- since the page might not be valid UTF8. Instead it curs the HTML a certain
-- marker strings in order to produce the result.
getImageUrl ::
  -- | The key string which is located closely to the download link in the HTML
  -- page
  String ->
  -- | A base URL see documentation on `WikiUrl` for details.
  URL ->
  -- | The HTML version of the media description page downloaded from the wiki.
  String ->
  -- | The URL to download the media file from the MediaWiki server wrapped in a
  -- Just value of the Maybe monad if found. A Nothing value of the Maybe monad
  -- otherwise.
  Maybe String
getImageUrl fi u ss =
  if isInfixOf fil s
    then case splitOn fil s of
      (_ : (y : _)) -> case splitOn theHref y of
        (_ : (yy : _)) -> case splitOn q yy of
          (z : _) ->
            Just
              ( (conv u) . UTF8Str.toString $
                  (BStr.pack z)
              )
          _ -> Nothing
        _ -> Nothing
      _ -> Nothing
    else Nothing
  where
    s = BStr.unpack (UTF8Str.fromString ss)
    fil = BStr.unpack (UTF8Str.fromString fi)
    theHref = BStr.unpack (UTF8Str.fromString "href=\"")
    q = BStr.unpack (UTF8Str.fromString "\"")

-- | Function to calculate the download instruction for the curl program to
-- download a single image
getImagePage1 ::
  -- | The temporary image download directory.
  String ->
  -- | The WikiUrl of the wiki website currently being processed.
  WikiUrl ->
  -- | A tuple. The first element of the tuple is the image number so just an
  -- integer that can be used to identify the image uniquely). The second
  -- element of the tuple is image include string of the image from the wiki
  -- source, that is the text in between the square brackets but with the
  -- "File:" or similar at the beginning removed and with everything after the |
  -- removed. So just the plain image filename on the wiki.
  (Integer, String) ->
  -- |  A tuple. The first element of the tuple is a String that shall be passed
  -- as part of a configuration file to the curl program in order to download
  -- all possible image description pages from the wiki.  The second element of
  -- the tuple is a list of output files where the curl program shall store the
  -- image description page from the wiki on the local disk. The third element
  -- of the tuple is a list of URLs where the image description pages from the
  -- wiki are located on the web.
  (String, [String], [String])
getImagePage1 dir u (i, ss) =
  (concat (map go numberedurls), map moo numberedurls, myloadurls)
  where
    numberedurls = (zip [1 ..] myurls) :: [(Integer, String)]
    go (n, myurl) = myurl ++ "\"\noutput = " ++ (moo (n, myurl)) ++ "\n"
    moo (n, _) = (dir </> (show i)) ++ "." ++ (show n) ++ ".html"
    myurls = map (("url = \"" ++) . kds . unify . exportURL . modpath2 ss) (parses u) :: [String]
    myloadurls = (map (kds . unify . exportURL . modpath2 ss) (parses u)) :: [String]
    kds ('h' : 't' : 't' : 'p' : 's' : ':' : '/' : '/' : xs) = ('h' : 't' : 't' : 'p' : 's' : ':' : '/' : '/' : (kds xs))
    kds ('/' : '/' : xs) = '/' : (kds xs)
    kds (x : xs) = x : (kds xs)
    kds [] = []

-- | Determine the link to download a media file from the wiki server.
getImagePage2 ::
  -- | The directiry where the HTML versions of the media desciption pages are
  -- stored.
  String ->
  -- | The `WikiUrl` to the wiki article currently being processed.
  WikiUrl ->
  -- | A pair. Its first element is the image number. Its second element is the
  -- image name.
  (Integer, String) ->
  -- | A tuple wrapped into the a Just value of the Maybe monad if the URL to
  -- download the media file could be determined. A Nothing value of the Maybe
  -- monad otherwise. The first element of the tuple is a list of possible URLs
  -- where the media file can be downloaded. The second element of the tuple is
  -- the image number. The thrid element of the tuple is the URL to the media
  -- decription page where the link to download the media file was found. The
  -- fourth element of the tuple is the link to download the media file.
  IO (Maybe ([String], Integer, URL, String))
getImagePage2 dir u (i, ss) =
  do
    l <-
      (mapM (\k -> Tools.readFile (dir </> (show i) ++ "." ++ (show k) ++ ".html")) [1 .. (length (parses u))]) ::
        IO [String]

    let xx = (map (getImageUrl2) (zip l (parses u))) :: [Maybe String]
    let gg = (zip (parses u) xx) :: [(URL, Maybe String)]
    let yy = (map go gg) :: [[(URL, String)]]
    let zz = (listToMaybe (concat yy)) :: Maybe (URL, String)
    case zz of
      Just (du, x) ->
        return
          ( Just
              ( map (unify . exportURL . (modpath2 ss)) (parses u),
                i,
                modpath2 ss du,
                x
              )
          )
      _ -> return Nothing
  where
    go :: (URL, Maybe String) -> [(URL, String)]
    go (uu, Just x) = [(uu, x)]
    go _ = []

-- | It takes the temporary image download directory as first parameter. It
-- takes a tuple as second input parameter. The first element of the tuple is
-- the image number so just an integer that can be used to identify the image
-- uniquely) . The second element of the tuple is image include string of the
-- image from the wiki source, that is the text in between the square brackets
-- as second input parameter. It takes the WikiUrl of the wiki website currently
-- being processed as third parameter. See function `getImages1` in this module
-- for documentation on the returned data type
doImage1 ::
  String -> WikiUrl -> (Integer, String) -> IO (String, [String], [String])
doImage1 dir theWikiUrl img =
  do return (getImagePage1 dir theWikiUrl (fst img, theName))
  where
    theName =
      case dropWhile (/= ':') (takeWhile (/= '|') (snd img)) of
        (_ : xs) -> replace2 xs "%27" "'"
        _ -> []

-- | Determine the URL to download a media file from the wiki.
doImage2 ::
  -- | The directory where the HTML versions of the media description pages are
  -- stored on the local disk.
  String ->
  -- | The `WikiUrl` to the article currently being processed
  WikiUrl ->
  -- | A pair. Its first element is the image number. Its second element is the
  -- image inclusion string. The is what is written in between the double square
  -- brackets in the wiki code.
  (Integer, String) ->
  -- | The URL to download the media file from the wiki wrapped into a Just
  -- value of the Maybe monad if it could be determined. A Nothing value of the
  -- Maybe monad otherwise.
  IO (Maybe String)
doImage2 dir theWikiUrl img =
  do
    p <- getImagePage2 dir theWikiUrl (fst img, theName)
    case p of
      Just (_, _, _, x) -> return (Just x)
      _ -> return Nothing
  where
    theName =
      case dropWhile (/= ':') (takeWhile (/= '|') (snd img)) of
        (_ : xs) -> replace2 xs "%27" "'"
        _ -> []

-- | Determine the author and license information for the media description page
-- on the wiki and write it to the local disk. Also write the link to the
-- version history page of the media description page to disk. This function
-- makes a call from the mediawiki2latex executable to itself as a subprocess in
-- order to save memory since the memory of a subprocess is free upon its
-- termination.
getImgContribUrl ::
  -- | The host name of the MediaWiki server where the content is located
  String ->
  -- | The file name on the local disk where the HTML version of the media
  -- description page is located.
  String ->
  -- | The action running the subprocess to determine the author and license
  -- information of the media file as well as the link to the version history
  -- page of the image description page on the wiki.
  IO ()
getImgContribUrl theHost x =
  do
    _ <-
      system
        ( "mediawiki2latex -x "
            ++ (Hex.hex (show (fullconfigbase {imgctrburl = Just (x, theHost)})))
        )
    return ()

-- | Call the mediawiki2latex executable by itself to determine the contributor
-- information of a page on the wiki.
getImgContrib ::
  -- | The file where the version history page of the page on the wiki has been
  -- stored on the local disk.
  String ->
  -- | The IO action to determine the contributor information from the version
  -- histroy page.
  IO ()
getImgContrib theFileName =
  do
    _ <-
      system
        ( "mediawiki2latex -x "
            ++ (Hex.hex (show (fullconfigbase {ctrb = Just theFileName})))
        )
    return ()

-- | Determine the contributors of a page on the wiki from its version history
-- page and write it to disk. This function is called by the mediawiki2latex
-- executable calling itself in a new subprocess. This is done to save memory
-- since the memory of a subprocess is freed upon its termination.
getContribCallBack ::
  -- | The file where the version histroy page is stored on the local disk.
  [Char] ->
  -- | The action to parse the version histroy page and write the contributor
  -- information to disk.
  ImperativeMonad ()
getContribCallBack theFileName =
  do
    x <- liftIO (Tools.readFile theFileName)
    let ff = (force (parseHtmlFast x))
    let dd =
          ( ( (deepGet "a" "class" "new mw-userlink" ff)
                ++ (deepGet "a" "class" "mw-userlink" ff)
            )
          ) ::
            [Anything Char]

    let ll = (filter pre (map go dd))
    let n = (nub ll) :: [(String, String)]
    let out = (map go2 (zip (map (count ll) n) n)) :: [(String, String, Int)]
    liftIO (BStr.writeFile (theFileName ++ ".out") (S.encode out))
    return ()
  where
    go :: Anything Char -> (String, String)
    go (Environment Tag (TagAttr _ m) l) =
      ((shallowFlatten (deepFlatten l)), findWithDefault "" "href" m)
    go _ = ("", "")
    go2 (c, (a, h)) = (a, h, c)

-- | Determine the author(s) (contributor(s)) of a media file. This function is
-- called by mediawiki2latex calling itself in a subprocess in order to save
-- memory, since the memory of a subprocess is automatically freed upon its
-- termination.
runCtrbUrl ::
  -- | The filename of the file where the HTML code of the media description
  -- page from the wiki is stored on the local disk.
  String ->
  -- | The host name of the MediaWiki server where the content is located.
  String ->
  -- | The action to parse the HTML code, write the author and license
  -- information as well as the URL to the version history page of the media
  -- description page of the media file on the wiki to the local disk.
  ImperativeMonad ()
runCtrbUrl ctrbfilename theHost =
  do
    yy <- liftIO (Tools.readFile ctrbfilename)
    let ht = (force (parseHtmlFast yy))
    let aut = fromMaybe [] (getAuthor ht) :: [Anything Char]
    let lic = map C (fromMaybe [] (getLicense ht)) :: [Anything Char]
    -- liftIO (print lic) liftIO (print aut)
    liftIO (BStr.writeFile (ctrbfilename ++ ".author") (S.encode aut))
    liftIO (BStr.writeFile (ctrbfilename ++ ".license") (S.encode lic))
    let gg = (deepGet "li" "id" "ca-history" ht)
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
    liftIO (Tools.writeFile (ctrbfilename ++ ".histurl") theUrl)
    return ()

-- | Main function to download files with curl.
fullmake ::
  -- | The temporary image download directory.
  String ->
  -- | The number of the curl main run as string.
  String ->
  -- | The IO action downloading the files from the wiki server.
  IO ()
fullmake dir s = do
  _ <- system ("curl --parallel-max 2 --retry-all-errors --retry 3 -s -w \"%{http_code} %{url} %{filename_effective}\\n\" --compressed -K " ++ (dir </> ("curlrun" ++ s)) ++ " --parallel -L > " ++ (dir </> ("curloutput" ++ s ++ ".1")))
  mymake dir s 1

-- | Sub function to download files with curl. This function tries to download
-- the files as often as needed until all files were downloaded successfully.
mymake ::
  -- | The temporary image download directory.
  String ->
  -- | The number of the curl main run as string.
  String ->
  -- | The number of the curl sub run as string. If everything goes well only
  -- one sub run is needed. But if some downloads fail as many sub runs as
  -- needed will be done until all file were downloaded successfully.
  Integer ->
  -- | The IO action downloading the files from the wiki server.
  IO ()
mymake dir s n = do
  text <- Tools.readFile (dir </> ("curloutput" ++ s ++ "." ++ (show n)))
  let list = (filter ppred (map (splitOn " ") (splitOn "\n" text)))
  putStrLn ("Number of Failures " ++ (show (length list)))
  Tools.writeFile (dir </> ("curlrun" ++ s ++ "." ++ (show (n + 1)))) (concat ((map jjoin list) :: [String]))
  _ <- if (list == []) then (return ExitSuccess) else system ("curl --parallel-max 2 --retry-all-errors --retry 3 -s --compressed -K " ++ (dir </> ("curlrun" ++ s ++ "." ++ (show (n + 1)))) ++ " --parallel -L -w \"%{http_code} %{url} %{filename_effective}\\n\"  > " ++ (dir </> ("curloutput" ++ s ++ "." ++ (show (n + 1)))))
  if ((list == [])) then return () else (mymake dir s (n + 1))
  where
    ppred (x : _) | x == "429" = True
    ppred _ = False
    jjoin :: [String] -> String
    jjoin (_ : y : z : []) = "url = \"" ++ y ++ "\"\noutput = \"" ++ z ++ "\"\n"
    jjoin _ = []

-- | Main function to download images from the wiki server. Also gets license
-- and contributor information of the images.
getImages1 ::
  -- | The temporary image download directory.
  String ->
  -- | The names of the images. That is the list of image names. An image name
  -- is what is in between the double square brackets in the wiki text.
  [String] ->
  -- | The `WikiUrl` to the main article currently being processed.
  WikiUrl ->
  -- | Action to download the images returning a list of `ImageCredits` records.
  -- Each record represents the author and license information for an image.
  ImperativeMonad ([ImageCredits])
getImages1 dir imagess theWikiUrl =
  do
    liftIO $
      do
        let ddir = dir
        let thetheWikiUrl = theWikiUrl
        let iimages = ((zip [1 ..] (map (premap2 . premap) imagess)))
        helper <- (mapM (doImage1 ddir thetheWikiUrl) iimages)
        let l = map first helper
        let hosts = concat (repeat (map hof (parses theWikiUrl)))

        let imgdescfiles = concat (map second helper)
        let imgdescurls = (map (gogo . third) helper)
        myprint (" curl run (1/3)")
        Tools.writeFile (dir </> "curlrun1") (concat l)
        fullmake dir "1"
        res <- (mapM (doImage2 ddir thetheWikiUrl) iimages)
        let cnt = concat (map go (zip ([1 ..] :: [Integer]) res))
        Tools.writeFile (dir </> "curlrun2") (cnt)
        myprint (" curl run (2/3)")
        fullmake dir "2"
        _ <- mapM (\(theHost, theFile) -> getImgContribUrl theHost theFile) (zip hosts imgdescfiles)
        histories <- mapM fun imgdescfiles
        Tools.writeFile (dir </> "curlrun3") (concat histories)
        myprint (" curl run (3/3)")
        fullmake dir "3"
        authorList <- mapM fun2 imgdescfiles
        mapM (getCredits dir) (zip (makautlist authorList) (zip imagess (zip ([1 .. (length imagess)]) imgdescurls)))
  where
    premap2 x = case splitOn "?lang=" x of
      (y : _) -> y
      _ -> x
    premap x = case splitOn "?page=" x of
      (y : _) -> y
      _ -> x
    lu = (length (parses theWikiUrl))
    makautlist [] = []
    makautlist xs = (concat (take lu xs)) : (makautlist (drop lu xs))
    gogo (x : _) = x
    gogo _ = ""
    first (x, _, _) = x
    second (_, x, _) = x
    third (_, _, x) = x
    getCredits mydir (author, (wwi, (i, descurl))) =
      do
        credits <- mapM (getCredit author (mydir </> (show i))) [1 .. (length (parses theWikiUrl))]
        return ((joinImageCredits credits) {theDescUrl = descurl, wikiFilename = theName wwi, imageNumber = i})
    getCredit :: String -> String -> Int -> IO ImageCredits
    getCredit auth path j = do
      aut <- getTheAuthor path j
      lic <- getTheLicense path j
      return (ImageCredits {theAltAuthors = auth, theAuthor = if j == (length (parses theWikiUrl)) then commonize aut else aut, theLicense = lic, theDescUrl = "", imageNumber = 0, wikiFilename = ""})
    getTheAuthor path j = do
      t <- BStr.readFile (path ++ "." ++ (show j) ++ ".html.author")
      let r = (S.decode t) :: Either String [Anything Char]
      case r of
        Right rr -> return (killTree rr)
        _ -> return []
    commonize ((Environment Tag (TagAttr "a" m) l) : xs) = (Environment Tag (TagAttr "a" (Map.alter altfun "href" m)) (commonize l)) : (commonize xs)
    commonize ((Environment t s l) : xs) = (Environment t s (commonize l)) : (commonize xs)
    commonize (x : xs) = x : (commonize xs)
    commonize [] = []
    altfun Nothing = Nothing
    altfun (Just ('/' : 'w' : 'i' : 'k' : 'i' : '/' : xs)) = Just ("https://commons.wikimedia.org/wiki/" ++ xs)
    altfun (Just ('/' : 'w' : '/' : xs)) = Just ("https://commons.wikimedia.org/w/" ++ xs)
    altfun (Just x) = Just x
    getTheLicense path j = do
      t <- BStr.readFile (path ++ "." ++ (show j) ++ ".html.license")
      let r = (S.decode t) :: Either String [Anything Char]
      case r of
        Right rr -> return rr
        _ -> return []

    theName wi = case dropWhile (/= ':') (takeWhile (/= '|') wi) of
      (_ : xs) -> replace2 (replace2 xs "%27" "'") "%" "%25"
      _ -> []

    joinImageCredits :: [ImageCredits] -> ImageCredits
    joinImageCredits c = let au = intercalate [C ','] (nub ((filter (/= []) (map theAuthor c)))) in ImageCredits {theAuthor = if au == [] then (map C (intercalate "," (nub ((filter (/= []) (map theAltAuthors c)))))) else au, theLicense = intercalate [C ','] (nub (filter (/= []) (map theLicense c))), theDescUrl = "", wikiFilename = "", imageNumber = 0, theAltAuthors = ""}

    fun x = do
      s <- Tools.readFile (x ++ ".histurl")
      if s == "" then return "" else return ("url = \"" ++ s ++ "\"\noutput = \"" ++ x ++ ".history\"\n")
    fun2 x = do
      s <- Tools.readFile (x ++ ".histurl")
      if s == ""
        then return []
        else do
          getImgContrib (x ++ ".history")
          t <- BStr.readFile (x ++ ".history.out")
          let r = (S.decode t) :: Either String [(String, String, Int)]
          case r of
            Right xs -> return (intercalate ", " (map atos xs))
            _ -> return []
    atos (x, _, _) = x
    go (number, Just u) = "url = \"" ++ u ++ "\"\noutput = \"" ++ (dir </> (show number)) ++ "\"\n"
    go _ = ""
    hof u = case url_type u of
      Absolute h -> host h
      _ -> ""

    killTree ll = concat (map killNode ll)

    killNode (Environment Tag (TagAttr "table" m) l) =
      case (Map.lookup "class" m) of
        Just "commons-creator-table mw-collapsible mw-collapsed mw-content-ltr" -> case (deepGet2 "th" l) of
          (x : _) -> [x]
          _ -> []
        _ -> [Environment Tag (TagAttr "table" m) (killTree l)]
    killNode (Environment Tag (TagAttr "div" m) l) = case (Map.lookup "style" m) of
      Just "display: none;" -> []
      _ -> [Environment Tag (TagAttr "div" m) (killTree l)]
    killNode (Environment x y l) =
      [Environment x y (killTree l)]
    killNode x = [x]
