{-# LANGUAGE DefaultSignatures, DeriveAnyClass #-}
{-DHUN| module to download the wiki source text form the wiki website DHUN-}
module Load where
import ImperativeState
import Tools
import Hex
import UrlAnalyse
import Control.Monad.State
import System.IO.Temp
import System.Directory
import System.FilePath.Posix
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim
import Codec.Binary.UTF8.String as C
import Data.String.HT
import Data.ByteString as B
       hiding (takeWhile, isInfixOf, intercalate, concat, map, sort)
import Data.List.Split
import Data.Map as Map hiding (map)
import Data.List hiding (lookup)
import MagicStrings
import SimpleContributors
import WikiHelper
import MediaWikiParseTree
import Network.URL
import Control.Monad.Except
import System.Process
import HtmlParser (parseHtml)
import Data.Serialize as S (encode, decode)



notendyet ::
          (String -> ImperativeMonad String) ->
            ParsecT String () ImperativeMonad String ->
              ParsecT String () ImperativeMonad String ->
                String -> ParsecT String () ImperativeMonad String
notendyet action sstart eend aku
  = try
      (do eof
          return aku)
      <|>
      try
        (do _ <- eend
            r <- startToEnd action sstart eend
            a <- lift (action aku)
            return (a ++ r))
      <|>
      do a <- anyChar
         notendyet action sstart eend (aku ++ [a])

beginning ::
          (String -> ImperativeMonad String) ->
            ParsecT String () ImperativeMonad String ->
              ParsecT String () ImperativeMonad String ->
                ParsecT String () ImperativeMonad [Char]
beginning action sstart eend
  = try
      (do eof
          return [])
      <|>
      do _ <- sstart
         ne <- notendyet action sstart eend []
         return (ne)

startToEnd ::
           (String -> ImperativeMonad String) ->
             ParsecT String () ImperativeMonad String ->
               ParsecT String () ImperativeMonad String ->
                 ParsecT String () ImperativeMonad String
startToEnd action sstart eend
  = try
      (do eof
          return [])
      <|> try (beginning action sstart eend)
      <|>
      do a <- anyChar
         s <- startToEnd action sstart eend
         return (a : s)

zeroAction :: (Monad m) => t -> t1 -> m [Char]
zeroAction _ _ = return ""

runAction ::
          String ->
            String ->
              (String -> ImperativeMonad String) ->
                String -> ImperativeMonad String
runAction sstart eend action text
  = do x <- (runParserT
               (startToEnd action (string sstart) (string eend))
               ()
               ""
               text)
       case x of
           Left _ -> return ""
           Right xs -> return xs

chapterAction :: WikiUrl -> String -> ImperativeMonad String
chapterAction wurl text
  = do pp <- liftIO (getpage d (wurl))
       case pp of
           Just p -> do _ <- addContributors d Nothing
                        noinclude wurl ("\n\ndhunparserurl " ++ d ++ "\n\n" ++ p)
           _ -> return ""
  where d = (trim (takeWhile (/= '|') text))

chapterAction2 :: WikiUrl->String -> String -> ImperativeMonad String
chapterAction2 wurl lema text
  = do pp <- liftIO (getpage d (wurl))
       case pp of
           Just p -> do _ <- addContributors d Nothing
                        noinclude wurl
                          ("\n\ndhunparserurl " ++ d ++ "\n\n" ++ "= " ++ e ++ " =\n" ++ p)
           _ -> return ""
  where e = (trim (takeWhile (/= '|') text))
        d = (removePrintVersion lema) ++ "/" ++ e

chapterAction3 :: WikiUrl -> String -> String -> ImperativeMonad String
chapterAction3 wurl text lema
  = do pp <- liftIO (getpage d (wurl))
       case pp of
           Just p -> do _ <- addContributors d Nothing
                        noinclude wurl
                          ("\n\ndhunparserurl " ++ d ++ "\n\n" ++ "= " ++ e ++ " =\n" ++ p)
           _ -> return ""
  where e = (trim (takeWhile (/= '|') text))
        d = (removePrintVersion lema) ++ "/ " ++ e

includeAction :: WikiUrl -> String -> ImperativeMonad String
includeAction = qIncludeAction

qIncludeAction :: WikiUrl -> String -> ImperativeMonad String
qIncludeAction wurl text
  = if isInfixOf "Vorlage" text then return ("{{" ++ text ++ "}}")
      else
      do pp <- (liftIO (print d)) >> liftIO (getpage d (wurl))
         case pp of
             Just p -> do _ <- addContributors d Nothing
                          noinclude wurl ("\n\ndhunparserurl " ++ d ++ "\n\n" ++ p)
             _ -> return ""
  where d = (trim (takeWhile (/= '|') text))


runqBookIncludeAction :: String -> ImperativeMonad ()
runqBookIncludeAction dir
  = do t <- liftIO $ B.readFile (dir </> "bookinput")
       let (cfg,wurl,text,acu,ad,fu)=(case S.decode t of {Right k->k::(FullConfig,WikiUrl,String,Either [FilePath] [Anything Char],[(Map String Contributor)],FullWikiUrl);_->undefined})
       oldst<-get 
       put oldst{loadacu=acu,fullUrl=fu}
       _ <- qBookIncludeActionbase cfg wurl text
       st<-get
       liftIO $ B.writeFile (dir </> "bookoutput") (S.encode (loadacu st,(audict st)++ad))


qBookIncludeActionbase :: FullConfig-> WikiUrl -> String ->  ImperativeMonad String
qBookIncludeActionbase cfg wurl text 
  =  if isInfixOf "Vorlage" text then return ("{{" ++ text ++ "}}") else if isInfixOf "Category:" text then return ""
                   else
                     do pp <- (liftIO (print d)) >> myfun
                        case pp of
                         Just p -> do _ <- addContributors d Nothing
                                      x <- noinclude wurl ("\n\ndhunparserurl " ++ d ++ "\n\n" ++ p)
                                      st <- get
                                      systempdir <- liftIO getTemporaryDirectory
                                      tempdir <- liftIO $
                                                  createTempDirectory systempdir "MediaWiki2LaTeXParser"
                                      liftIO $ Tools.writeFile (tempdir </> "input") x
                                      _ <- liftIO $
                                             system
                                               ("mediawiki2latex -x " ++
                                                  (Hex.hex (show (fullconfigbase{compile = Just tempdir, runMode= runMode cfg}))))
                                      case (loadacu st) of
                                          Right base -> do t <- liftIO $ B.readFile (tempdir </> "output")
                                                           put st{loadacu = Right ((case S.decode t of {Right k->k;_->[]})++ base :: [Anything Char])}
                                          Left base -> put st{loadacu = Left (tempdir: base)}
                                      return x
                         _ -> return  ""
  where d = (trim (takeWhile (/= '|') text))
        myfun = case (runMode cfg) of 
                  HTML _ -> liftIO  (getBookpage d (wurl))
                  (ExpandedTemplates _) ->  (loadMediaWiki d wurl) >>= (return . Just)
                  _ -> (loadPlain d wurl Nothing)  >>= (return . Just)

qBookIncludeAction :: FullConfig-> WikiUrl -> String ->  ImperativeMonad String
qBookIncludeAction cfg wurl text 
  = do sst <- get
       case (loadacu sst) of
        Right _ ->   if isInfixOf "Vorlage" text then return ("{{" ++ text ++ "}}") else if isInfixOf "Category:" text then return "" else
                        do pp <- (liftIO (print d)) >> myfun
                           case pp of
                            Just p -> do _ <- addContributors d Nothing
                                         x <- noinclude wurl ("\n\ndhunparserurl " ++ d ++ "\n\n" ++ p)
                                         st <- get
                                         systempdir <- liftIO getTemporaryDirectory
                                         tempdir <- liftIO $ createTempDirectory systempdir "MediaWiki2LaTeXParser"
                                         --liftIO ( Tools.writeFile (t2empdir </> "input") x)
                                         _ <- liftIO $ system ("mediawiki2latex -x " ++   (Hex.hex (show (fullconfigbase{compile = Just tempdir, runMode= runMode cfg}))))
                                         case (loadacu st) of
                                          Right base -> do t <- liftIO $ B.readFile (tempdir </> "output")
                                                           put st{loadacu = Right ((case S.decode t of {Right k->k;_->[]})++ base :: [Anything Char])}
                                          Left base -> put st{loadacu = Left (tempdir: base)}
                                         return x
                            _ -> return  ""
        Left _ ->    do systempdir <- liftIO getTemporaryDirectory
                        tempdir <- liftIO $  createTempDirectory systempdir "MediaWiki2LaTeXBook"
                        liftIO $ B.writeFile (tempdir </> "bookinput") (S.encode (cfg,wurl,text,loadacu sst,audict sst,fullUrl sst))
                        _ <- liftIO $ system ("mediawiki2latex -x " ++  (Hex.hex (show (fullconfigbase{convert = Just (NewLoad tempdir)}))))
                        t <- liftIO $ B.readFile (tempdir </> "bookoutput")
                        oldst<-get
                        let (acu,au)=(case S.decode t of {Right k->k::(Either [FilePath] [Anything Char],[(Map String Contributor)]);_->(Right [],[])}) 
                        put oldst{loadacu=acu,audict=au}
                        return []

                       
  where d = (trim (takeWhile (/= '|') text))
        myfun = case (runMode cfg) of 
                  HTML _ -> liftIO  (getBookpage d (wurl))
                  (ExpandedTemplates _) ->  (loadMediaWiki d wurl) >>= (return . Just)
                  _ -> (loadPlain d wurl Nothing)  >>= (return . Just)

makeUrl :: String -> String -> String -> [Char]
makeUrl lang theFam thePage
  = (unify . exportURL)
      (if isInfixOf "commons" lang then
         (URL{url_path = "~daniel/WikiSense/Contributors.php",
              url_params =
                [("wikifam", "commons.wikimedia.org"), ("page", thePage),
                 ("since", ""), ("until", ""), ("grouped", "on"),
                 ("hideanons", "on"), ("max", "100000"), ("format", "html")],
              url_type =
                Absolute
                  (Host{protocol = HTTP True, host = "toolserver.org",
                        port = Nothing})})
         else
         (URL{url_path = "~daniel/WikiSense/Contributors.php",
              url_params =
                [("wikilang", lang), ("wikifam", theFam), ("page", thePage),
                 ("since", ""), ("until", ""), ("grouped", "on"),
                 ("hideanons", "on"), ("max", "100000"), ("format", "html")],
              url_type =
                Absolute
                  (Host{protocol = HTTP True, host = "toolserver.org",
                        port = Nothing})}))

langau :: Map String String
langau
  = fromList
      [("hi", "\2354\2375\2326\2325"), ("ja", "\33879\32773"),
       ("pl", "Autorzy"),
       ("lo", "\3737\3761\3713\3739\3760\3742\3761\3737"),
       ("fi", "Tekij\228"), ("sv", "F\246rfattare"), ("pt", "Autores"),
       ("ru", "\1040\769\1074\1090\1086\1088\1099"),
       ("ko", "\51200\51088"), ("tr", "Yazar"), ("sk", "Avtor"),
       ("hy", "\1344\1381\1394\1387\1398\1377\1391"), ("lt", "Autorius"),
       ("ta", "\2986\2975\3016\2986\3021\2986\3006\2995\2992\3021"),
       ("en", "Contributors"), ("ro", "Autor"), ("it", "Autori"),
       ("hr", "\192utor"), ("vo", "Lautan"), ("eo", "Verkinto"),
       ("hu", "Szerz\337"), ("is", "H\246fundur"), ("gd", "\217ghdar"),
       ("de", "Autoren"), ("ca", "Autor"),
       ("el", "\931\965\947\947\961\945\966\941\945\962"),
       ("bg", "\1040\1074\1090\1086\1088"),
       ("ce", "\1071\1079\1076\1072\1088\1093\1086"), ("nl", "Auteurs"),
       ("es", "Autores"), ("eu", "Egile"), ("fr", "Auteurs"),
       ("cs", "Autor"), ("br", "Aozer")]

makeHeader :: FullWikiUrl -> Maybe String -> [Char]
makeHeader fullurl m
  = let mmm = m >>= (\ yy -> Map.lookup yy langau) in
      "\\chapter{" ++
        (case
           mmm `mplus`
             (case splitOn "." (hostname fullurl) of
                  (x : _) -> Map.lookup x langau
                  _ -> Nothing)
             `mplus` (Map.lookup "en" langau)
           of
             Just x -> x
             _ -> "Contributors")
          ++
          "}\n" ++
            "\\label{Contributors}\n" ++
              "\\begin{longtable}{rp{0.6\\linewidth}}\n" ++
                "\\textbf{Edits}&\\textbf{User}\\\\\n"

makeHeaderHTML :: FullWikiUrl -> Maybe String -> [Char]
makeHeaderHTML fullurl m
  = let mmm = m >>= (\ yy -> Map.lookup yy langau) in
      "<h2>" ++
        (case
           mmm `mplus`
             (case splitOn "." (hostname fullurl) of
                  (x : _) -> Map.lookup x langau
                  _ -> Nothing)
             `mplus` (Map.lookup "en" langau)
           of
             Just x -> x
             _ -> "Contributors")
          ++
          "</h2>\n" ++
            "<table rules=\"all\">" ++ "<tr><td>Edits</td><td>User</td></tr>"

makeBody :: (Ord t) => Map t Contributor -> URL -> [Char]
makeBody m u = concat (map go (sort (toList m)))
  where fun ('/' : xs) = xs
        fun xs = xs
        go (_, v)
          = (show (edits v)) ++
              "& \\myhref{" ++
                (concat
                   (map chartransforlink (exportURL (u{url_path = (fun (href v))}))))
                  ++ "}{" ++ (concat (map chartrans (name v))) ++ "}\\\\\n"

makeBodyHTML :: (Ord t) => Map t Contributor -> URL -> [Char]
makeBodyHTML m u = concat (map go (sort (toList m)))
  where fun ('/' : xs) = xs
        fun xs = xs
        go (_, v)
          = "<tr><td>" ++
              (show (edits v)) ++
                "</td><td><a href=\"" ++
                  (((exportURL (u{url_path = (fun (href v))})))) ++
                    "\">" ++ (concat (map chartrans (name v))) ++ "</a></td></tr>"

makeContributors :: Maybe URL -> ImperativeMonad (String, String)
makeContributors uu
  = do st <- get
       li <- liftIO (return (audict st))
       let myaudict = contribsum li
       let theUrl
             = case uu of
                   Just u -> exportURL u
                   _ -> makeUrl3 (lemma (fullUrl st)) (hostname (fullUrl st))
       yy <- liftIO $ geturl theUrl
       let lang
             = case (deepGet2 "html" (parseHtml yy)) of
                   ((Environment Tag (TagAttr _ m) _) : []) -> Map.lookup "lang" m
                   _ -> Nothing
       return
         (((makeHeader (fullUrl st) lang) ++
             (makeBody (myaudict) (url (fullUrl st))) ++
               "\\end{longtable}\n" ++ "\\pagebreak\n"),
          (makeHeaderHTML (fullUrl st) lang) ++
            (makeBodyHTML (myaudict) (url (fullUrl st))) ++ "</table>")

parseUrl :: String -> ImperativeMonad FullWikiUrl
parseUrl u
  = case analyseFull u of
        Just x -> return x
        _ -> throwError (WikiUrlParseError u)

getContributors ::
                [String] ->
                  ImperativeMonad (([(Map String Contributor)], [(Maybe String)]))
getContributors u
  = do st <- get
       stz <- liftIO imperativeStateZero
       put stz{counter = counter st}
       au <- mapM go u
       newState <- get
       put st
       rr <- liftIO (return (audict newState, au))
       return rr
  where go uu
          = do purl <- parseUrl uu
               sst <- get
               put sst{fullUrl = purl}
               addContributors (lemma purl) (Just (UrlAnalyse.url purl))

addContributors ::
                [Char] -> Maybe URL -> ImperativeMonad ((Maybe String))
addContributors theLemma uu
  = do sst <- get
       let st = fullUrl sst
       thetheLemma <- liftIO $ return theLemma
       thetheHostname <- liftIO $ return (hostname st)
       thetheUU <- liftIO $ return uu
       au <- (liftIO ((((fun sst)) thetheLemma thetheHostname thetheUU)))
               :: ImperativeMonad ((Map String Contributor, Maybe String))
       auau <- liftIO (((return . fst)) au)
       lic <- liftIO (((return . snd)) au)
       put sst{audict = auau : (audict sst)}
       return lic
  where fun ssst lem ho uuu
          = do xx <- simpleContributors lem ho uuu ssst
               return (Data.List.foldl runGo2 Map.empty xx, myvalue xx)
        runGo2 mymap (author, theHref, theEdits, _)
          = Map.alter (infun author theHref (fromIntegral theEdits)) author
              mymap
        myvalue yy
          = case yy of
                [(_, __, _, Just lic)] -> (Just lic)
                _ -> Nothing
        
        infun ::
              String ->
                String -> Integer -> Maybe Contributor -> Maybe Contributor
        infun a h e xx
          = case xx of
                Nothing -> Just Contributor{name = a, href = h, edits = e}
                Just old -> Just old{edits = (edits old) + e}

noinclude :: t -> String -> ImperativeMonad [Char]
noinclude wurl
  = runAction "<noinclude>" "</noinclude>" (zeroAction wurl)

runActions :: WikiUrl -> String -> String-> ImperativeMonad String
runActions wurl lema text
  = do x <- noinclude wurl text
       y <- runAction "{{Druckversion Kapitel|" "}}" (chapterAction3 wurl lema) x
       v <- runAction "{{Print entry|" "}}" (chapterAction2 wurl lema) y
       z <- runAction "{{print entry|" "}}" (chapterAction2 wurl lema) v
       a <- runAction "{{Print entry|" "}}" (chapterAction wurl) z
       b <- runAction "{{:" "}}" (includeAction wurl) a
       c <- runAction "{{:" "}}" (qIncludeAction wurl) b
       d <- runAction "{{:" "}}" (qIncludeAction wurl) c
       e <- runAction "{{:" "}}" (qIncludeAction wurl) d
       f <- runAction "{{:" "}}" (qIncludeAction wurl) e
       g <- runAction "{{:" "}}" (qIncludeAction wurl) f
       h <- runAction "{{:" "}}" (qIncludeAction wurl) g
       i <- runAction "{{:" "}}" (qIncludeAction wurl) g
       j <- runAction "{{:" "}}" (qIncludeAction wurl) h
       _ <- runAction "{{:" "}}" (qIncludeAction wurl) i
       runAction "{{:" "}}" (qIncludeAction wurl) j

runBookActions :: FullWikiUrl -> String -> FullConfig ->ImperativeMonad String
runBookActions fu text cfg
  = do x <- noinclude wurl text
       runAction "[[" "]]" (qBookIncludeAction cfg wurl) x
  where wurl = wikiUrl fu

replacements :: String -> String
replacements x
  = replace2
      (replace2
         (replace2
            (replace2 x "[[Image:Nuvola apps noatun.png|left|20px|Aufgabe]]"
               "{{Ubung}}")
            "[[Image:Yes_check.svg|12px]]"
            "{{TickYes}}")
         "{{col-break}}"
         "\n|")
      "{{Fortran:Vorlage: Table}}"
      "prettytable"

loadPlain :: String -> WikiUrl -> Maybe URL -> ImperativeMonad [Char]
loadPlain lema wurl uu
  =   do pp <- liftIO (getpage lema wurl)
         case pp of
             Just p -> do _ <- addContributors lema uu
                          runActions wurl lema p
             _ -> throwError (DownloadError lema (show wurl))

loadBook :: ImperativeState -> Maybe URL -> FullConfig ->ImperativeMonad [Char]
loadBook st uu cfg
  = let fu = fullUrl st in
      do pp <- liftIO (getpage (lemma fu) (wikiUrl fu))
         case pp of
             Just p -> do _ <- addContributors (lemma fu) uu
                          runBookActions fu p cfg
             _ -> throwError (DownloadError (lemma fu) (exportURL (url fu)))

loadHTML :: ImperativeState -> ImperativeMonad String
loadHTML st
  = let fu = fullUrl st in
      do midst <- get
         (res, newst) <- liftIO
                           (runStateT (runExceptT (loadPlain (lemma (fullUrl st)) (wikiUrl (fullUrl st)) (Just (url fu)))) midst)
         case res of
             Right _ -> put newst
             _ -> return ()
         x <- liftIO (geturl2 (exportURL (url fu)))
         return . C.decode . unpack $ x



loadHTMLnoQuote :: ImperativeState -> ImperativeMonad String
loadHTMLnoQuote st
  = let fu = fullUrl st in
      do x <- liftIO (geturl2 (exportURL (url fu)))
         return . C.decode . unpack $ x

loadBookHTML :: ImperativeState -> ImperativeMonad String
loadBookHTML st
  = let fu = fullUrl st in
      do midst <- get
         (res, newst) <- liftIO
                           (runStateT (runExceptT (loadPlain (lemma (fullUrl st)) (wikiUrl (fullUrl st)) (Just (url fu)))) midst)
         case res of
             Right _ -> put newst
             _ -> return ()
         x <- liftIO (geturl2 (exportURL (url fu)))
         return . C.decode . unpack $ x

loadMediaWiki :: String-> WikiUrl -> ImperativeMonad [Char]
loadMediaWiki lema  wurl
  =   do pp <- liftIO (getpage2 lema wurl)
         case pp of
             Just (ss, u) -> do _ <- addContributors lema Nothing
                                s <- liftIO
                                       (getExpandedPage lema
                                          (replace2
                                             (replace2
                                                (replace2 ss "<ref name=" "dhunnamedrefopendhun")
                                                "<ref"
                                                "dhunrefopendhun")
                                             "</ref>"
                                             "dhunrefclosedhun")
                                          u)
                                case s of
                                    Just sss -> return
                                                  (multireplace sss
                                                     [("&lt;", "<"), ("&gt;", ">"), ("&amp;", "&"),
                                                      ("dhunrefopendhun", "<ref"),
                                                      ("dhunnamedrefopendhun", "<ref name="),
                                                      ("dhunrefclosedhun", "</ref>")])
                                    _ -> do liftIO (print "Error")
                                            throwError
                                              (DownloadError lema (show wurl))
             _ -> throwError (DownloadError lema  (show wurl))

{-DHUN| main function to download images form the wiki. It takes the RunMode as only parameter. In case of HTML the html from the website is loaded. In all other cases the wiki source text is downloaded. In case of ExpandedTemplates the templates are also expanded by mediawiki running on the wiki website DHUN-}

load :: FullConfig->ImperativeMonad String
load cfg
  = do st <- get
       case (runMode cfg) of
           HTML Yes-> do if (outputType cfg) `Data.List.elem` [ZipArchive,PlainPDF] then  put (st{loadacu=Left []}) else return ()
                         loadBook st Nothing cfg

           UserTemplateFile Yes _ -> loadBook st Nothing cfg
           StandardTemplates Yes -> loadBook st Nothing cfg
           ExpandedTemplates Yes -> loadBook st Nothing cfg


           HTML No -> loadHTML st
           UserTemplateFile No _ -> loadPlain (lemma (fullUrl st)) (wikiUrl (fullUrl st)) Nothing
           StandardTemplates No -> loadPlain (lemma (fullUrl st)) (wikiUrl (fullUrl st)) Nothing
           ExpandedTemplates No -> loadMediaWiki (lemma (fullUrl st)) (wikiUrl (fullUrl st))














