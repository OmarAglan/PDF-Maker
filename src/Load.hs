{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | module to download the wiki source text form the wiki website
module Load where

import Codec.Binary.UTF8.String as C
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.ByteString as B hiding
  ( concat,
    intercalate,
    isInfixOf,
    map,
    sort,
    takeWhile,
  )
import Data.List hiding (lookup)
import Data.List.Split
import Data.Map as Map hiding (map)
import Data.Maybe (fromMaybe)
import Data.Serialize as S (decode, encode)
import Data.String.HT
import Hex
import HtmlParser (parseHtml)
import ImperativeState
import MagicStrings
import MediaWikiParseTree
import Network.URL
import SimpleContributors
import System.Directory
import System.FilePath.Posix
import System.IO.Temp
import System.Process
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec hiding (try)
import Tools
import UrlAnalyse
import WikiHelper

-- | A parser to match the pairs of opening and closing brackets and the
-- substring in between them and replace the three with an application of a
-- function on the substring. See `runAction` for details. This parser matches
-- only if to the string it is applied to is starting after the opening bracket.
notendyet ::
  -- | The function to be applied to the substring.
  (String -> ImperativeMonad String) ->
  -- | The parser for the opening bracket.
  ParsecT String () ImperativeMonad String ->
  -- | The parser for the closing bracket.
  ParsecT String () ImperativeMonad String ->
  -- | The accumulator to accumulate characters between the opening and closing
  -- bracket. Has to be the empty string when called externally.
  String ->
  -- | The parser for matching all pairs of opening and closing brackets and the
  -- substrings in between each of the pair and replaces each triple (opening
  -- brackets, substring, closing bracket) with the application of function
  -- given in the first parameter on the substring.  See `runAction` for
  -- details. This parser only match if the string it is applied to starts
  -- immediately after the opening bracket.
  ParsecT String () ImperativeMonad String
notendyet action sstart eend aku =
  try
    ( do
        eof
        return aku
    )
    <|> try
      ( do
          _ <- eend
          r <- startToEnd action sstart eend
          a <- lift (action aku)
          return (a ++ r)
      )
    <|> do
      a <- anyChar
      notendyet action sstart eend (aku ++ [a])

-- | A parser to match the pairs of opening and closing brackets and the
-- substring in between them and replace the three with an application of a
-- function on the substring. See `runAction` for details. This parser matches
-- only if the string it is applied to begins with an opening bracket or is the
-- empty string itself.
beginning ::
  -- | The function to be applied to the substring.
  (String -> ImperativeMonad String) ->
  -- | The parser for the opening bracket.
  ParsecT String () ImperativeMonad String ->
  -- | The parser for the closing bracket.
  ParsecT String () ImperativeMonad String ->
  -- | The parser for matching all pairs of opening and closing brackets and the
  -- substrings in between each of the pair and replaces each triple (opening
  -- brackets, substring, closing bracket) with the application of function
  -- given in the first parameter on the substring.  See `runAction` for
  -- details. This parser only match if the string it is applied to is the empty
  -- string or starts with and opening bracket.
  ParsecT String () ImperativeMonad [Char]
beginning action sstart eend =
  try
    ( do
        eof
        return []
    )
    <|> do
      _ <- sstart
      ne <- notendyet action sstart eend []
      return (ne)

-- | A parser to match the pairs of opening and closing brackets and the
-- substring in between them and replace the three with an application of a
-- function on the substring. See `runAction` for details.
startToEnd ::
  -- | The function to be applied to the substring.
  (String -> ImperativeMonad String) ->
  -- | The parser for the opening bracket.
  ParsecT String () ImperativeMonad String ->
  -- | The parser for the closing bracket.
  ParsecT String () ImperativeMonad String ->
  -- | The parser for matching all pairs of opening and closing brackets and the
  -- substrings in between each of the pair and replaces each triple (opening
  -- brackets, substring, closing bracket) with the application of function
  -- given in the first parameter on the substring.  See `runAction` for
  -- details.
  ParsecT String () ImperativeMonad String
startToEnd action sstart eend =
  try
    ( do
        eof
        return []
    )
    <|> try (beginning action sstart eend)
    <|> do
      a <- anyChar
      s <- startToEnd action sstart eend
      return (a : s)

-- | Action to replace the brackets and the substring in between with the empty
-- string (see `runAction` for details).
zeroAction :: WikiUrl -> String -> ImperativeMonad String
zeroAction _ _ = return ""

-- | Looks at a string and does some replacement in it. It looks for opening and
-- closing brackets (given in the first and second parameter) and the substrings
-- between each pair of opening and closing brackets. A function given in the
-- third parameter is applied to the each substring and the each pair of
-- brackets including the substring between them is replaced by the result of
-- the application of the function given in the third parameter.
runAction ::
  -- | The opening bracket.
  String ->
  -- | The closing bracket.
  String ->
  -- | The function to be applied.
  (String -> ImperativeMonad String) ->
  -- | The input string to process.
  String ->
  -- | The resulting string wrapped in the ImperativeMonad
  ImperativeMonad String
runAction sstart eend action text =
  do
    x <-
      ( runParserT
          (startToEnd action (string sstart) (string eend))
          ()
          ""
          text
        )
    case x of
      Left _ -> return ""
      Right xs -> return xs

-- | A function to download the content of a linked sub website and replace the
-- link with the downloaded content. See also `runAction`
chapterAction ::
  -- | The `WikiUrl` the main website of which the sub website shall be
  -- downloaded
  WikiUrl ->
  -- | The contents of the wiki template which requests the inclusion of the sub
  -- website
  String ->
  -- | ImperativeMonad action downloading the sub website and returning it. Also
  -- the contributors of the sub website are accumulated in the state of the
  -- ImperativeMonad.
  ImperativeMonad String
chapterAction wurl text =
  do
    pp <- liftIO (getpage d (wurl))
    case pp of
      Just p -> do
        _ <- addContributors d Nothing
        noinclude wurl ("\n\ndhunparserurl " ++ d ++ "\n\n" ++ p)
      _ -> return ""
  where
    d = (trim (takeWhile (/= '|') text))

-- | A function to download the content of a linked sub website and replace the
-- link with the downloaded content. See also `runAction`
chapterAction2 ::
  -- | The `WikiUrl` the main website of which the sub website shall be
  -- downloaded
  WikiUrl ->
  -- | The lemma of the main website of which the sub website shall be
  -- downloaded
  String ->
  -- | The contents of the wiki template which requests the inclusion of the sub
  -- website
  String ->
  -- | ImperativeMonad action downloading the sub website and returning it. Also
  -- the contributors of the sub website are accumulated in the state of the
  -- ImperativeMonad.
  ImperativeMonad String
chapterAction2 wurl lema text =
  do
    pp <- liftIO (getpage d (wurl))
    case pp of
      Just p -> do
        _ <- addContributors d Nothing
        noinclude
          wurl
          ("\n\ndhunparserurl " ++ d ++ "\n\n" ++ "= " ++ e ++ " =\n" ++ p)
      _ -> return ""
  where
    e = (trim (takeWhile (/= '|') text))
    d = (removePrintVersion lema) ++ "/" ++ e

-- | A function to download the content of a linked sub website and replace the
-- link with the downloaded content. See also `runAction`
qIncludeAction ::
  -- | The `WikiUrl` the main website of which the sub website shall be
  -- downloaded
  WikiUrl ->
  -- | The contents of the wiki template which requests the inclusion of the sub
  -- website
  String ->
  -- | ImperativeMonad action downloading the sub website and returning it. Also
  -- the contributors of the sub website are accumulated in the state of the
  -- ImperativeMonad.
  ImperativeMonad String
qIncludeAction wurl text =
  if isInfixOf "Vorlage" text
    then return ("{{" ++ text ++ "}}")
    else do
      st <- get
      if d `Data.List.elem` (finishedLemmas st)
        then return []
        else do
          put (st {finishedLemmas = (d : (finishedLemmas st))})
          pp <- (liftIO (print d)) >> liftIO (getpage d (wurl))
          case pp of
            Just p -> do
              _ <- addContributors d Nothing
              noinclude wurl ("\n\ndhunparserurl " ++ d ++ "\n\n" ++ p ++ " ")
            _ -> return ""
  where
    d = (trim (takeWhile (/= '|') text))

-- | This function downloads the wiki source or HTML form the wiki server and
-- parses it to a parse tree. It writes the parse tree to disk. Also writes the
-- accumulated filenames of this storage files together with the the accumulated
-- contributor information to disk.
runqBookIncludeAction ::
  -- | The directory where to read the parameters file from and store the result
  -- to.
  String ->
  -- | The action as described above.
  ImperativeMonad ()
runqBookIncludeAction dir =
  do
    t <- liftIO $ B.readFile (dir </> "bookinput")
    let (cfg, wurl, text, acu, ad, fu) = (case S.decode t of Right k -> k :: (FullConfig, WikiUrl, String, Either [FilePath] [Anything Char], [(Map String Contributor)], FullWikiUrl); _ -> undefined)
    oldst <- get
    put oldst {loadacu = acu, fullUrl = fu}
    _ <- qBookIncludeActionbase cfg wurl text
    st <- get
    liftIO $ B.writeFile (dir </> "bookoutput") (S.encode (loadacu st, (audict st) ++ ad))

-- | This function downloads the wiki source or HTML form the wiki server and
-- parses it to a parse tree. It writes the parse tree to disk and accumulates
-- the name of the storage file in the loadacu. It also adds the contributors of
-- the current article to the list of contributors.
qBookIncludeActionbase ::
  -- | The Configuration the current run of mediawiki2latex .
  FullConfig ->
  -- | The `WikiUrl` where the main page of the book is stored .
  WikiUrl ->
  -- | The name of the sub page of the book to be processed (the lemma).
  String ->
  -- | Run the action as explained above and returns the wiki source code or
  -- HTML code downloaded with an added marker which is used to identify the sub
  -- page when parsing the wiki text.
  ImperativeMonad String
qBookIncludeActionbase cfg wurl text =
  if isInfixOf "Vorlage" text
    then return ("{{" ++ text ++ "}}")
    else
      if (isInfixOf "Category:" text) || (isInfixOf "Kategorie:" text)
        then return ""
        else do
          pp <- (liftIO (print d)) >> myfun
          case pp of
            Just p -> do
              _ <- addContributors d Nothing
              x <- noinclude wurl ("\n\ndhunparserurl " ++ d ++ "\n\n" ++ p)
              st <- get
              systempdir <- liftIO getTemporaryDirectory
              tempdir <-
                liftIO $
                  createTempDirectory systempdir "MediaWiki2LaTeXParser"
              liftIO $ Tools.writeFile (tempdir </> "input") x
              _ <-
                liftIO $
                  system
                    ( "mediawiki2latex -x "
                        ++ (Hex.hex (show (fullconfigbase {compile = Just tempdir, runMode = runMode cfg, latexTables = latexTables cfg})))
                    )
              case (loadacu st) of
                Right base -> do
                  t <- liftIO $ B.readFile (tempdir </> "output")
                  put st {loadacu = Right ((case S.decode t of Right k -> k; _ -> []) ++ base :: [Anything Char])}
                Left base -> put st {loadacu = Left (tempdir : base)}
              return x
            _ -> return ""
  where
    d = (trim (takeWhile (/= '|') text))
    myfun = case (runMode cfg) of
      HTML _ -> liftIO (getBookpage d (wurl))
      (ExpandedTemplates _) -> (loadMediaWiki d wurl) >>= (return . Just)
      _ -> (loadPlain d wurl Nothing) >>= (return . Just)

-- | This action downloads the wiki source or HTML from the MediaWiki server and
-- reads it into a parse tree. The operation of this action depends on the
-- selected output format. In case the output format is LaTeX or PDF (loadacu is
-- in Left case) it loads an parses each article separately and stores the parse
-- tree on disk and accumulates the filenames of the storage files in the
-- loadacu. In all other cases the parse trees will be accumulated in the
-- loadacu. It spawns sub processes for each article to be processed in order to
-- save memory since the memory of each subprocess is freed upon its
-- termination.
qBookIncludeAction ::
  -- | The configuration the current run of mediawiki2latex
  FullConfig ->
  -- | The `WikiUrl` of the books main page (the page that contains the links to
  -- the actual articles in the book)
  WikiUrl ->
  -- | The lemma (names of the article) of the current sub page of the book
  String ->
  -- | Run the action causing the side effects described above and return the
  -- wiki source or HTML loaded from the wiki server.
  ImperativeMonad String
qBookIncludeAction cfg wurl text =
  do
    sst <- get
    case (loadacu sst) of
      Right _ ->
        if isInfixOf "Vorlage" text
          then return ("{{" ++ text ++ "}}")
          else
            if (isInfixOf "Category:" text) || (isInfixOf "Kategorie:" text)
              then return ""
              else
                if (noparent cfg) && case text of { '/' : _ -> False; _ -> True } && (fromMaybe False (wurl >>= (return . not . ((flip Data.List.isPrefixOf) text) . (intercalate "/") . (Data.List.dropWhile (== "wiki")) . (splitOn "/") . url_path . fst)))
                  then return ""
                  else do
                    pp <- (liftIO (print d)) >> myfun
                    case pp of
                      Just p -> do
                        _ <- addContributors d Nothing
                        x <- noinclude wurl ("\n\ndhunparserurl " ++ d ++ "\n\n" ++ p)
                        st <- get
                        systempdir <- liftIO getTemporaryDirectory
                        tempdir <- liftIO $ createTempDirectory systempdir "MediaWiki2LaTeXParser"
                        liftIO (Tools.writeFile (tempdir </> "input") x)
                        _ <- liftIO $ system ("mediawiki2latex -x " ++ (Hex.hex (show (fullconfigbase {compile = Just tempdir, runMode = runMode cfg}))))
                        case (loadacu st) of
                          Right base -> do
                            t <- liftIO $ B.readFile (tempdir </> "output")
                            put st {loadacu = Right ((case S.decode t of Right k -> k; _ -> []) ++ base :: [Anything Char])}
                          Left base -> put st {loadacu = Left (tempdir : base)}
                        return x
                      _ -> return ""
      Left _ ->
        if isInfixOf "Vorlage" text
          then return ("{{" ++ text ++ "}}")
          else
            if (isInfixOf "Category:" text) || (isInfixOf "Kategorie:" text)
              then return ""
              else
                if (noparent cfg) && case text of { '/' : _ -> False; _ -> True } && (fromMaybe False (wurl >>= (return . not . ((flip Data.List.isPrefixOf) text) . (intercalate "/") . (Data.List.dropWhile (== "wiki")) . (splitOn "/") . url_path . fst)))
                  then return ""
                  else do
                    systempdir <- liftIO getTemporaryDirectory
                    tempdir <- liftIO $ createTempDirectory systempdir "MediaWiki2LaTeXBook"
                    liftIO $ B.writeFile (tempdir </> "bookinput") (S.encode (cfg, wurl, text, loadacu sst, audict sst, fullUrl sst))
                    _ <- liftIO $ system ("mediawiki2latex -x " ++ (Hex.hex (show (fullconfigbase {convert = Just (NewLoad tempdir)}))))
                    t <- liftIO $ B.readFile (tempdir </> "bookoutput")
                    oldst <- get
                    let (acu, au) = (case S.decode t of Right k -> k :: (Either [FilePath] [Anything Char], [(Map String Contributor)]); _ -> (Right [], []))
                    put oldst {loadacu = acu, audict = au}
                    return []
  where
    d = (trim (takeWhile (/= '|') text))
    myfun = case (runMode cfg) of
      HTML _ -> liftIO (getBookpage d (wurl))
      (ExpandedTemplates _) -> (loadMediaWiki d wurl) >>= (return . Just)
      _ -> (loadPlain d wurl Nothing) >>= (return . Just)

-- | Mapping from the language code (e.g. en for English) to the Heading used in
-- the list of contributors (e.g. Contributors for English).
langau ::
  -- | Map from language code to the heading for the list of contributors.
  Map String String
langau =
  fromList
    [ ("hi", "\2354\2375\2326\2325"),
      ("ja", "\33879\32773"),
      ("pl", "Autorzy"),
      ("lo", "\3737\3761\3713\3739\3760\3742\3761\3737"),
      ("fi", "Tekij\228"),
      ("sv", "F\246rfattare"),
      ("pt", "Autores"),
      ("ru", "\1040\769\1074\1090\1086\1088\1099"),
      ("ko", "\51200\51088"),
      ("tr", "Yazar"),
      ("sk", "Avtor"),
      ("hy", "\1344\1381\1394\1387\1398\1377\1391"),
      ("lt", "Autorius"),
      ("ta", "\2986\2975\3016\2986\3021\2986\3006\2995\2992\3021"),
      ("en", "Contributors"),
      ("ro", "Autor"),
      ("it", "Autori"),
      ("hr", "\192utor"),
      ("vo", "Lautan"),
      ("eo", "Verkinto"),
      ("hu", "Szerz\337"),
      ("is", "H\246fundur"),
      ("gd", "\217ghdar"),
      ("de", "Autoren"),
      ("ca", "Autor"),
      ("el", "\931\965\947\947\961\945\966\941\945\962"),
      ("bg", "\1040\1074\1090\1086\1088"),
      ("ce", "\1071\1079\1076\1072\1088\1093\1086"),
      ("nl", "Auteurs"),
      ("es", "Autores"),
      ("eu", "Egile"),
      ("fr", "Auteurs"),
      ("cs", "Autor"),
      ("br", "Aozer")
    ]

-- | Returns the header for the list of contributors in LaTeX version.
makeHeader ::
  -- | The `FullWikiUrl` of the wiki page to be processed.
  FullWikiUrl ->
  -- | The language code (e.g. en for English) for the wiki to be processed
  -- wrapped in a Just value of the Maybe monad if it could be determined. A
  -- Nothing value of the Maybe monad otherwise.
  Maybe String ->
  -- | The header of the list of contributors in LaTeX version.
  [Char]
makeHeader fullurl m =
  let mmm = m >>= (\yy -> Map.lookup yy langau)
   in "\\chapter{"
        ++ ( case mmm
               `mplus` ( case splitOn "." (hostname fullurl) of
                           (x : _) -> Map.lookup x langau
                           _ -> Nothing
                       )
               `mplus` (Map.lookup "en" langau) of
               Just x -> x
               _ -> "Contributors"
           )
        ++ "}\n"
        ++ "\\label{Contributors}\n"
        ++ "\\begin{longtable}{rp{0.6\\linewidth}}\n"
        ++ "\\textbf{Edits}&\\textbf{User}\\\\\n"

-- | Returns the header for the list of contributors in HTML version.
makeHeaderHTML ::
  -- | The `FullWikiUrl` of the wiki page to be processed.
  FullWikiUrl ->
  -- | The language code (e.g. en for English) for the wiki to be processed
  -- wrapped in a Just value of the Maybe monad if it could be determined. A
  -- Nothing value of the Maybe monad otherwise.
  Maybe String ->
  -- | The header of the list of contributors in HTML version.
  [Char]
makeHeaderHTML fullurl m =
  let mmm = m >>= (\yy -> Map.lookup yy langau)
   in "<h2>"
        ++ ( case mmm
               `mplus` ( case splitOn "." (hostname fullurl) of
                           (x : _) -> Map.lookup x langau
                           _ -> Nothing
                       )
               `mplus` (Map.lookup "en" langau) of
               Just x -> x
               _ -> "Contributors"
           )
        ++ "</h2>\n"
        ++ "<table rules=\"all\">"
        ++ "<tr><td>Edits</td><td>User</td></tr>"

-- | Returns the body of the list of contributors in LaTeX version.
makeBody ::
  -- | A mapping from the names of the contributor their respective contributor
  -- records.
  Map String Contributor ->
  -- | The URL to the wiki page currently being processed.
  URL ->
  -- | The body of the list of contributors in LaTeX version.
  [Char]
makeBody m u = concat (map go (sort (toList m)))
  where
    fun ('/' : xs) = xs
    fun xs = xs
    go (_, v) =
      (show (edits v))
        ++ "& \\mycontributorhref{"
        ++ ( concat
               (map chartransforlink (exportURL (u {url_path = (fun (href v))})))
           )
        ++ "}{"
        ++ (concat (map chartrans (name v)))
        ++ "}\\\\\n"

-- | Returns the body of the list of contributors in HTML version.
makeBodyHTML ::
  -- | A mapping from the names of the contributor their respective contributor
  -- records.
  Map String Contributor ->
  -- | The URL to the wiki page currently being processed.
  URL ->
  -- | The body of the list of contributors in HTML version.
  [Char]
makeBodyHTML m u = concat (map go (sort (toList m)))
  where
    fun ('/' : xs) = xs
    fun xs = xs
    go (_, v) =
      "<tr><td>"
        ++ (show (edits v))
        ++ "</td><td><a href=\""
        ++ (((exportURL (u {url_path = (fun (href v))}))))
        ++ "\">"
        ++ (concat (map chartrans (name v)))
        ++ "</a></td></tr>"

-- | Returns the list of contributors in LaTeX and HTML version.
makeContributors ::
  -- | The URL of the wiki page currently being processed.
  Maybe URL ->
  -- | Returns a tuple wrapped into the imperative monad. The first element of
  -- the tuple is the LaTeX version of the list of contributors. The second
  -- element of the tuple is the HTML version of the list of contributors.
  ImperativeMonad (String, String)
makeContributors uu =
  do
    st <- get
    li <- liftIO (return (audict st))
    let myaudict = contribsum li
    let theUrl =
          case uu of
            Just u -> exportURL u
            _ -> makeUrl3 (lemma (fullUrl st)) (hostname (fullUrl st))
    yy <- liftIO $ geturl theUrl
    let llang =
          case (deepGet2 "html" (parseHtml yy)) of
            ((Environment Tag (TagAttr _ m) _) : []) -> Map.lookup "lang" m
            _ -> Nothing
    return
      ( ( (makeHeader (fullUrl st) llang)
            ++ (makeBody (myaudict) (url (fullUrl st)))
            ++ "\\end{longtable}\n"
            ++ "\\pagebreak\n"
        ),
        (makeHeaderHTML (fullUrl st) llang)
          ++ (makeBodyHTML (myaudict) (url (fullUrl st)))
          ++ "</table>"
      )

-- | Parse an URL to a `FullWikiUrl`. Throw an error if the parsing was not
-- successful, return the `FullWikiUrl` otherwise. See also documentation of the
-- the function `analyseFull`.
parseUrl ::
  -- | The URL to be analyzed .
  String ->
  -- | The parsed version of the URL given in the first parameter.
  ImperativeMonad FullWikiUrl
parseUrl u =
  case analyseFull u of
    Just x -> return x
    _ -> throwError (WikiUrlParseError u)

-- | Determine the contributors of an article on the wiki and update the state
-- of ImperativeMonad according to this information. Return the license of the
-- wiki page wrapped in a Just value of the Maybe monad if it could be
-- determine. Return a Nothing value of the Maybe monad otherwise.
addContributors ::
  -- | The name of the article of which to determine the authors.
  [Char] ->
  -- | The URL to the main page on the wiki to be processed.
  Maybe URL ->
  -- | The license of the wiki page wrapped into a Just value of the Maybe monad
  -- if it could be determined. The Nothing value of the Maybe monad otherwise.
  -- The whole returned value is wrapped into the ImperativeMonad. The state of
  -- the ImperativeMonad is updated with new contributor information of the
  -- article currently being processed.
  ImperativeMonad ((Maybe String))
addContributors theLemma uu =
  do
    sst <- get
    let st = fullUrl sst
    thetheLemma <- liftIO $ return theLemma
    thetheHostname <- liftIO $ return (hostname st)
    thetheUU <- liftIO $ return uu
    au <-
      (liftIO ((((fun sst)) thetheLemma thetheHostname thetheUU))) ::
        ImperativeMonad ((Map String Contributor, Maybe String))
    auau <- liftIO (((return . fst)) au)
    lic <- liftIO (((return . snd)) au)
    put sst {audict = auau : (audict sst)}
    return lic
  where
    fun ssst lem ho uuu =
      do
        xx <- simpleContributors lem ho uuu ssst
        return (Data.List.foldl runGo2 Map.empty xx, myvalue xx)
    runGo2 mymap (author, theHref, theEdits, _) =
      Map.alter
        (infun author theHref (fromIntegral theEdits))
        author
        mymap
    myvalue yy =
      case yy of
        [(_, __, _, Just lic)] -> (Just lic)
        _ -> Nothing

    infun ::
      String ->
      String ->
      Integer ->
      Maybe Contributor ->
      Maybe Contributor
    infun a h e xx =
      case xx of
        Nothing -> Just Contributor {name = a, href = h, edits = e}
        Just old -> Just old {edits = (edits old) + e}

-- | replace all content between noinclude tags with empty string including the
-- tags itself.
noinclude ::
  -- | The WikiUrl.
  WikiUrl ->
  -- | The string to in which to remove the noinclude tags.
  String ->
  -- | The input string with the noinclude tags removed wrapped into
  -- the`ImperativeMonad`
  ImperativeMonad String
noinclude wurl =
  runAction "<noinclude>" "</noinclude>" (zeroAction wurl)

-- | Resolve the references to included sub pages when processing wiki text and
-- removed content between noinclude tags.
runActions ::
  -- | The `WikiUrl` the main page to be processed.
  WikiUrl ->
  -- | The name of the main article to be processed.
  String ->
  -- | The wiki source text of the article before the resolution of possibly
  -- included sub pages.
  String ->
  -- | The wiki source text with the references to sub pages resolved up to a
  -- certain maximum recursion depth wrapped in to imperative monad. Also
  -- updates the state of Imperative monad with the contributor information
  -- determined for each sub page.
  ImperativeMonad String
runActions wurl lema text =
  do
    x <- noinclude wurl text
    v <- runAction "{{Print entry|" "}}" (chapterAction2 wurl lema) x
    z <- runAction "{{print entry|" "}}" (chapterAction2 wurl lema) v
    a <- runAction "{{Print entry|" "}}" (chapterAction wurl) z
    b <- runAction "{{:" "}}" (qIncludeAction wurl) a
    c <- runAction "{{:" "}}" (qIncludeAction wurl) b
    d <- runAction "{{:" "}}" (qIncludeAction wurl) c
    e <- runAction "{{:" "}}" (qIncludeAction wurl) d
    f <- runAction "{{:" "}}" (qIncludeAction wurl) e
    g <- runAction "{{:" "}}" (qIncludeAction wurl) f
    h <- runAction "{{:" "}}" (qIncludeAction wurl) g
    i <- runAction "{{:" "}}" (qIncludeAction wurl) h
    j <- runAction "{{:" "}}" (qIncludeAction wurl) i
    k <- runAction "{{:" "}}" (qIncludeAction wurl) j
    runAction "{{:" "}}" (qIncludeAction wurl) k

-- | Run the actions to download and compile the sub pages of a book when
-- mediawiki2latex is running in book mode. This will involve calls of the
-- mediawiki2latex executable to the mediawiki2latex executable itself. This is
-- done in order to save memory since the memory of a subprocess will be freed
-- upon its termination.
runBookActions ::
  -- | The FullWikiUrl of the collection page of the book.
  FullWikiUrl ->
  -- | The contents of the collection page as wiki source text.
  String ->
  -- | The configuration for this run of mediawiki2latex.
  FullConfig ->
  -- | The IO action loading the sub pages of the book.
  ImperativeMonad String
runBookActions fu text cfg =
  do
    x <- noinclude wurl text
    runAction "[[" "]]" (qBookIncludeAction cfg wurl) x
  where
    wurl = wikiUrl fu

-- | Loads the wiki source code of a wiki page and resolve possibly included sub
-- pages. Also determines the contributors of the wiki page and any possibly
-- included sub pages.
loadPlain ::
  -- | The name of the article on the wiki .
  String ->
  -- | The `WikiUrl` to the article to be processed.
  WikiUrl ->
  -- | The URL to the article to be processed wrapped into a Just value of the
  -- Maybe monad if known. Nothing value of the Maybe monad otherwise
  Maybe URL ->
  -- | The wiki source of the page with sub page inclusions expanded wrapped
  -- into the ImperativeMonad. Also adds the contributor information of the wiki
  -- page and all included sub pages to the state of the ImperativeMonad.
  ImperativeMonad [Char]
loadPlain lema wurl uu =
  do
    pp <- liftIO (getpage lema wurl)
    case pp of
      Just p -> do
        _ <- addContributors lema uu
        runActions wurl lema p
      _ -> throwError (DownloadError lema (show wurl))

-- | Loads a book from the wiki. A book is a collection of wiki links to lemmas.
-- If the selected output format is either PDF or LaTeX source this will also
-- include the reading of the input code (wiki or HTML) to a parse tree and the
-- storage of this parse tree on disk. This is done in several subprocess in
-- order to save memory since the memory of each subprocess is freed upon its
-- termination.
loadBook ::
  -- | The state of the imperative monad on the start of this action.
  ImperativeState ->
  -- | The URL under which the collection is located.
  Maybe URL ->
  -- | The configuration for the run of mediawiki2latex.
  FullConfig ->
  -- | The action downloading the book and adding the contributor information to
  -- the state of the ImperativeMonad.
  ImperativeMonad [Char]
loadBook st uu cfg =
  let fu = fullUrl st
   in do
        pp <- liftIO (getpage (lemma fu) (wikiUrl fu))
        case pp of
          Just p -> do
            _ <- addContributors (lemma fu) uu
            runBookActions fu p cfg
          _ -> throwError (DownloadError (lemma fu) (exportURL (url fu)))

-- | Load an HTML page from the wiki. Also gets the contributor information of
-- that page from the wiki. See `loadPlain` for details of the contributor
-- determination process.
loadHTML ::
  -- | The state of the ImperativeMonad at the time when this function is
  -- called.
  ImperativeState ->
  -- | The HTML page as a String wrapped into the ImperativeMonad. Also the
  -- state of the ImperativeMonad is updated to account for the additionally
  -- determined contributors.
  ImperativeMonad String
loadHTML st =
  let fu = fullUrl st
   in do
        midst <- get
        (res, newst) <-
          liftIO
            (runStateT (runExceptT (loadPlain (lemma (fullUrl st)) (wikiUrl (fullUrl st)) (Just (url fu)))) midst)
        case res of
          Right _ -> put newst
          _ -> return ()
        x <- liftIO (geturl2 (exportURL (url fu)))
        return ("\n\ndhunparserurl " ++ (lemma (fullUrl st)) ++ "\n\n" ++ ((C.decode . unpack) x))

-- | Load the wiki code with the templates expanded by MediaWiki from a
-- MediaWiki server.
loadMediaWiki ::
  -- | The lemma on the wiki to be downloaded
  String ->
  -- | The WikiUrl of the lemma to be downloaded (see type `UrlAnalyse.WikiUrl`
  -- for details)
  WikiUrl ->
  -- | The wiki source text with the templates expanded wrapped into the
  -- `ImperativeMonad` since the download requires an IO action and can possibly
  -- fail.
  ImperativeMonad [Char]
loadMediaWiki lema wurl =
  do
    pp <- liftIO (getpage2 lema wurl)
    case pp of
      Just (ss, u) -> do
        _ <- addContributors lema Nothing
        s <-
          liftIO
            ( getExpandedPage
                lema
                ( replace2
                    ( replace2
                        (replace2 ss "<ref name=" "dhunnamedrefopendhun")
                        "<ref"
                        "dhunrefopendhun"
                    )
                    "</ref>"
                    "dhunrefclosedhun"
                )
                u
            )
        case s of
          Just sss ->
            return
              ( multireplace
                  sss
                  [ ("&lt;", "<"),
                    ("&gt;", ">"),
                    ("&amp;", "&"),
                    ("dhunrefopendhun", "<ref"),
                    ("dhunnamedrefopendhun", "<ref name="),
                    ("dhunrefclosedhun", "</ref>")
                  ]
              )
          _ -> do
            liftIO (print "Error")
            throwError
              (DownloadError lema (show wurl))
      _ -> throwError (DownloadError lema (show wurl))

-- | main function to download content form the wiki. It takes the RunMode as
-- only parameter. In case of HTML the HTML from the website is loaded. In all
-- other cases the wiki source text is downloaded. In case of ExpandedTemplates
-- the templates are also expanded by MediaWiki running on the wiki website
load :: FullConfig -> ImperativeMonad String
load cfg =
  do
    st <- get
    case (runMode cfg) of
      HTML Yes -> do
        if (outputType cfg) `Data.List.elem` [ZipArchive, PlainPDF] then put (st {loadacu = Left []}) else return ()
        loadBook st Nothing cfg
      UserTemplateFile Yes _ -> loadBook st Nothing cfg
      StandardTemplates Yes -> loadBook st Nothing cfg
      ExpandedTemplates Yes -> loadBook st Nothing cfg
      HTML No -> loadHTML st
      UserTemplateFile No _ -> loadPlain (lemma (fullUrl st)) (wikiUrl (fullUrl st)) Nothing
      StandardTemplates No -> loadPlain (lemma (fullUrl st)) (wikiUrl (fullUrl st)) Nothing
      ExpandedTemplates No -> loadMediaWiki (lemma (fullUrl st)) (wikiUrl (fullUrl st))
