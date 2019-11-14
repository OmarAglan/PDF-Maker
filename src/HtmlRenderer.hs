
module HtmlRenderer where
import MediaWikiParseTree
import MyState
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.Trans.State (State, state, runState, put, get)
import LatexRenderer
import WikiHelper
import Tools
import Data.Char
import Text.Printf
import Babel
import Control.Monad (guard)
import Data.String.HT (trim)
import Data.List.Split
import Text.Highlighting.Kate
import Text.Blaze.Html.Renderer.String
import Data.Tuple
import Data.Hashable
import Hex

type HtmlRenderer = State MyState

templateToHtml :: [Anything Char] -> String -> Renderer String
templateToHtml l s
  = state $
      \ st -> swap $ templateHtmlProcessor st (prepateTemplate l s)

templateHtmlProcessor ::
                      MyState ->
                        (String, Map String [Anything Char]) -> (MyState, String)
templateHtmlProcessor st
  ("Mathe f\252r Nicht-Freaks: Vorlage:Warnung", ll)
  = (st,
     "<b>Warnung</b><br/>" ++
       (treeToHtml (Map.findWithDefault [] "1" ll) st) ++ "")
templateHtmlProcessor st
  ("Mathe f\252r Nicht-Freaks: Vorlage:Hinweis", ll)
  = (st,
     "<b>Hinweis</b><br/>" ++
       (treeToHtml (Map.findWithDefault [] "1" ll) st) ++ "")
templateHtmlProcessor st
  ("Mathe f\252r Nicht-Freaks: Vorlage:Beispiel", ll)
  = (st,
     "<b>Beispiel</b><br/>" ++
       (treeToHtml (Map.findWithDefault [] "beispiel" ll) st) ++ "")
templateHtmlProcessor st
  ("Mathe f\252r Nicht-Freaks: Vorlage:Satz", ll)
  = (st,
     "<b>Satz</b><br/>" ++
       (treeToHtml (Map.findWithDefault [] "satz" ll) st) ++ "")
templateHtmlProcessor st
  ("Mathe f\252r Nicht-Freaks: Vorlage:L\246sungsweg", ll)
  = (st,
     "<b>Wie kommt man auf den Beweis?</b>" ++
       (treeToHtml (Map.findWithDefault [] "l\246sungsweg" ll) st) ++ "")
templateHtmlProcessor st
  ("Mathe f\252r Nicht-Freaks: Vorlage:Beweis", ll)
  = (st,
     "<b>Beweis: </b><br/>" ++
       (treeToHtml (Map.findWithDefault [] "beweis" ll) st) ++ "")
templateHtmlProcessor st
  ("Mathe f\252r Nicht-Freaks: Vorlage:Definition", ll)
  = (st,
     "<b>Definition: </b><i>(" ++
       (treeToHtml (Map.findWithDefault [] "titel" ll) st) ++
         ")</i><br/>" ++
           (treeToHtml (Map.findWithDefault [] "definition" ll) st))
templateHtmlProcessor st
  ("mathe f\252r Nicht-Freaks: Vorlage:Definition", ll)
  = (st,
     "<b>Definition: </b><i>(" ++
       (treeToHtml (Map.findWithDefault [] "titel" ll) st) ++
         ")</i><br/>" ++
           (treeToHtml (Map.findWithDefault [] "definition" ll) st))
templateHtmlProcessor st ("-", ll)
  = (tempProcAdapter $ mnfindent ll) st
templateHtmlProcessor st
  ("Mathe f\252r Nicht-Freaks: Vorlage:Klapptext", ll)
  = (tempProcAdapter $ mnfklapptext ll) st
templateHtmlProcessor st
  ("Aufgabensammlung: Vorlage:Klapptext", ll)
  = (tempProcAdapter $ mnfklapptext ll) st
templateHtmlProcessor st
  ("Aufgabensammlung: Vorlage:Vollst\228ndige Induktion", ll)
  = (tempProcAdapter $ mnfinduktion ll) st
templateHtmlProcessor st ("Formel", ll)
  = (st,
     "<dl><dd>" ++
       (treeToHtml (Map.findWithDefault [] "1" ll) st) ++ "</dd></dl>")
templateHtmlProcessor st
  ("Mathe f\252r Nicht-Freaks: Vorlage:Frage", ll)
  = (tempProcAdapter $ mnffrage ll) st
templateHtmlProcessor st ("Anker", _) = (st, "")
templateHtmlProcessor st ("Symbol", ll)
  = (st, (treeToHtml (Map.findWithDefault [] "1" ll) st))
templateHtmlProcessor st
  ("#invoke:Mathe f\252r Nicht-Freaks/Seite", _) = (st, "")
templateHtmlProcessor st ("Aufgabensammlung: Vorlage:Infobox", _)
  = (st, "")
templateHtmlProcessor st ("Aufgabensammlung: Vorlage:Symbol", _)
  = (st, "")
templateHtmlProcessor st ("Nicht l\246schen", _) = (st, "")
templateHtmlProcessor st ("#ifeq:{{{include", _) = (st, "")
templateHtmlProcessor st ("Druckversion Titelseite", _) = (st, "")
templateHtmlProcessor st ("PDF-Version Gliederung", _) = (st, "")
templateHtmlProcessor st ("#invoke:Liste", _) = (st, "")
templateHtmlProcessor st ("Smiley", _) = (st, "\9786")
templateHtmlProcessor st ("", _) = (st, "")
templateHtmlProcessor st (x, _)
  = (st, "UNKNOWN TEMPLATE " ++ x ++ " ")

wikiLinkCaptionHtml :: [Anything Char] -> MyState -> String
wikiLinkCaptionHtml l st = if isCaption x then rebuild x else ""
  where x = (treeToHtml (last (splitOn [C '|'] l)) st)
        rebuild (':' : xs) = xs
        rebuild b = b

wikiImageToHtml :: [Anything Char] -> Renderer String
wikiImageToHtml l
  = do st <- get
       mystr <- return
                  ((if not (micro st) then "<p>" else "") ++
                     "<" ++
                       (if ext == "webm" then "video controls" else "img") ++
                         " src=\"./images/" ++
                           (n st) ++
                             "." ++
                               ext ++
                                 "\" style=\"width: " ++
                                   (if (tb st) then "100.0" else (mysize st)) ++
                                     "%;\">" ++
                                       (if (not (micro st)) then
                                          "<br/> " ++
                                            (getfig st) ++ " " ++ (n st) ++ " " ++ (s st) ++ "</p>"
                                          else ""))
       put
         st{getImages = (getImages st) ++ [shallowFlatten l],
            getJ = ((getJ st) + 1)}
       return mystr
  where ext
          = normalizeExtensionHtml
              (map toLower
                 (fileNameToExtension (headSplitEq '|' (shallowFlatten l))))
        s st
          = if (trim (s1 st)) `elem` ["verweis=", "alt=", "link="] then ""
              else (s1 st)
        s2 st
          = case Map.lookup "alt" (snd (prepateTemplate l "x")) of
                Just xx -> wikiLinkCaptionHtml xx st
                Nothing -> wikiLinkCaptionHtml l st
        s1 st
          = if '|' `elem` (shallowFlatten l) then (s2 st) else
              (treeToHtml [] st{getJ = ((getJ st) + 1)})
        mysize st = printf "%0.5f" ((mysizefloat2 st) * 100.0)
        mysizefloat st = (min (getF st) (imageSize l))
        mysizefloat2 st = if (msb st) then 1.0 else (mysizefloat st)
        msb st = (mysizefloat st) == (getF st)
        micro st = ((mysizefloat st) < 0.17) || ((getInTab st) > 1)
        n st = show (getJ st)
        tb st = ((getInTab st) > 0)
        getfig st
          = head
              (splitOn "}"
                 (last
                    (splitOn "\\newcommand{\\myfigurebabel}{"
                       (makeBabel (langu st) "en"))))

galleryContentToHtml :: [[Anything Char]] -> Renderer String
galleryContentToHtml (x : xs)
  = do s <- galleryRowToHtml x
       ss <- galleryContentToHtml xs
       return $ s ++ "</tr><tr>" ++ ss
galleryContentToHtml [] = return []

{-DHUN| converts a part of a gallery (image gallery, gallery tag) from parse tree to latex. A part are as many elements as fit into a single row in the resulting latex table DHUN-}

galleryRowToHtml :: [Anything Char] -> Renderer String
galleryRowToHtml [] = return []
galleryRowToHtml (x : []) = treeToHtml2 [x]
galleryRowToHtml (x : xs)
  = do s <- treeToHtml2 [x]
       g <- galleryRowToHtml xs
       return $ s ++ "</td><td>" ++ g

{-DHUN| Converts are gallery (image gallery, gallery tag) from parse tree to latex. Also writes table header and footer. This is the function you should use for converting galleries to latex DHUN-}

galleryToHtml :: [Anything Char] -> Renderer String
galleryToHtml x
  = do st <- get
       put st{getF = (getF st) * galleryTableScale}
       s <- (galleryContentToHtml
               [z | z <- splitToTuples [y | y <- x, isWikiLink y],
                trim (treeToHtml z st) /= ""])
       st2 <- get
       put st2{getF = (getF st)}
       return ("<table><tr>" ++ s ++ "</tr></table>")

mnffrage :: Map String [Anything Char] -> Renderer String
mnffrage ll
  = do typ <- treeToHtml2 (Map.findWithDefault [] "typ" ll)
       frage <- treeToHtml2 (Map.findWithDefault [] "frage" ll)
       antwort <- treeToHtml2 (Map.findWithDefault [] "antwort" ll)
       return
         ("<dl><dd><b>" ++
            typ ++ ":</b> " ++ frage ++ "</dd><dd>" ++ antwort ++ "</dd></dl>")

mnfindent :: Map String [Anything Char] -> Renderer String
mnfindent ll
  = do one <- treeToHtml2 (Map.findWithDefault [] "1" ll)
       return ("<dl><dd>" ++ one ++ "</dd></dl>")

mnfklapptext :: Map String [Anything Char] -> Renderer String
mnfklapptext ll
  = do kopf <- treeToHtml2 (Map.findWithDefault [] "kopf" ll)
       inhalt <- treeToHtml2 (Map.findWithDefault [] "inhalt" ll)
       return ("<b>" ++ kopf ++ "</b><br/>" ++ inhalt)

mnfinduktion :: Map String [Anything Char] -> Renderer String
mnfinduktion ll
  = do erf <- treeToHtml2
                (Map.findWithDefault [] "erfuellungsmenge" ll)
       aus <- treeToHtml2 (Map.findWithDefault [] "aussageform" ll)
       anf <- treeToHtml2 (Map.findWithDefault [] "induktionsanfang" ll)
       vor <- treeToHtml2
                (Map.findWithDefault [] "induktionsvoraussetzung" ll)
       beh <- treeToHtml2
                (Map.findWithDefault [] "induktionsbehauptung" ll)
       sch <- treeToHtml2
                (Map.findWithDefault [] "beweis_induktionsschritt" ll)
       return
         ("<b>Aussageform, deren Allgemeing\252ltigkeit f\252r " ++
            erf ++
              " bewiesen werden soll:</b><br/>" ++
                aus ++
                  "<br/><b>1. Induktionsanfang</b><br/>" ++
                    anf ++
                      "<br/><b>2. Induktionsschritt</b><br><b>2a. Induktionsvoraussetzung</b><br/>"
                        ++
                        vor ++
                          "<br/><b>2b. Induktionsbehauptung</b><br/>" ++
                            beh ++
                              "<br/><b>2c. Beweis des Induktionsschritts</b><br/>" ++
                                sch ++ "<br/>")

writedict :: [(String, String)] -> String
writedict [] = []
writedict ((k, v) : xs)
  = k ++ "=\"" ++ v ++ "\" " ++ (writedict xs)

treeToHtml3 ::
            Map String Int ->
              Maybe String ->
                String -> [Anything Char] -> MyState -> (String, MyState)
treeToHtml3 formulas mylanguage title l st
  = let (a, b)
          = runState (treeToHtml2 l) st{langu = mylanguage, forms = formulas}
      in
      ("<html><head><meta charset=\"utf-8\"><title>" ++
         title ++
           "</title><style>table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode {  margin: 0; padding: 0; vertical-align: baseline; border: none; }\ntable.sourceCode { width: 100%; line-height: 100%; }\ntd.lineNumbers { text-align: right; padding-right: 4px; padding-left: 4px; }\ntd.sourceCode { padding-left: 5px; }\ncode > span.kw { font-weight: bold; }\ncode > span.dt { text-decoration: underline; }\ncode > span.co { font-style: italic; }\ncode > span.al { font-weight: bold; }\ncode > span.er { font-weight: bold; }\n</style></head><body>"
             ++ a,
       b)

treeToHtml :: [Anything Char] -> MyState -> String
treeToHtml l states = (fst $ runState (treeToHtml2 l) states)

treeToHtmlBak :: [Anything Char] -> MyState -> String
treeToHtmlBak _ _ = ""

treeToHtml2Bak :: [Anything Char] -> HtmlRenderer String
treeToHtml2Bak _ = return ""

treeToHtml2 :: [Anything Char] -> HtmlRenderer String
treeToHtml2 ll
  = do x <- allinfo
       return $ concat x
  where allinfo :: HtmlRenderer [String]
        allinfo = mapM nodeToHtml ll
        
        walk :: String -> [Anything Char] -> String -> HtmlRenderer String
        walk prefix l postfix
          = do d <- treeToHtml2 l
               return $ prefix ++ d ++ postfix
        
        nodeToHtml :: Anything Char -> HtmlRenderer String
        nodeToHtml (C c)
          = do st <- get
               x <- if (c == '\n') && ((lastChar st) == c) then return "</p><p>"
                      else return [c]
               put st{lastChar = c}
               return x
        nodeToHtml (Environment Wikilink _ l)
          = do st <- get
               if getInHeading st then return $ wikiLinkCaption l st else
                 if (isImage (shallowFlatten l)) then wikiImageToHtml l else
                   return $ wikiLinkCaption l st
        nodeToHtml (Environment Tag (TagAttr "br" _) _) = return "<br/>"
        nodeToHtml (Environment Tag (TagAttr "script" _) _) = return []
        nodeToHtml (Environment Source (TagAttr _ a) l)
          = do let g = case reverse l of
                           [] -> []
                           (x : xs) -> if x == (C '\n') then reverse xs else l
               let f = shallowFlatten (map renormalize (breakLines3 linewidth l))
               d <- treeToHtml2 (breakLines3 linewidth g)
               st <- get
               return $
                 case
                   do aa <- Map.lookup "lang" a
                      guard (not (getInFootnote st))
                      guard (not ((getInTab st) > 0))
                      return aa
                   of
                     Just j -> (renderHtml
                                  ((formatHtmlBlock defaultFormatOpts) (highlightAs j f)))
                     Nothing -> (rtrim d)
        nodeToHtml (Environment Template (Str s) l) = templateToHtml l s
        nodeToHtml (Environment Wikitable _ l)
          = walk "<table><tr>" l "</table></tr>"
        nodeToHtml (Environment TableRowSep _ _) = return "</tr><tr>"
        nodeToHtml (Environment TableColSep _ _) = return "</td><td>"
        nodeToHtml (Environment TableHeadColSep _ _) = return "</th><th>"
        nodeToHtml (Environment TableCap _ l)
          = walk "<caption>" l "</caption>"
        nodeToHtml (Environment Wikiheading (Str x) l)
          = let y = (show (length x)) in
              walk ("<h" ++ y ++ ">") l ("</h" ++ y ++ ">")
        nodeToHtml (Environment ItemEnv (Str _) [Item _]) = return []
        nodeToHtml (Environment ItemEnv (Str s) l)
          = do tag <- return
                        (case s of
                             "*" -> "ul"
                             _ -> "ol")
               walk ("<" ++ tag ++ ">") l ("</li></" ++ tag ++ ">")
        nodeToHtml (Item _) = return "</li><li>"
        nodeToHtml (Environment Tag (TagAttr "noscript" _) _) = return []
        nodeToHtml (Environment Tag (TagAttr "head" _) _) = return []
        nodeToHtml (Environment Tag (TagAttr "a" _) l) = walk "" l ""
        nodeToHtml (Environment Tag (TagAttr "body" _) l) = walk "" l ""
        nodeToHtml (Environment Tag (TagAttr "html" _) l) = walk "" l ""
        nodeToHtml (Environment Tag (TagAttr "div" a) l)
          = if (Map.member "class" a) then
              if
                ((Map.findWithDefault [] "class" a) `elem`
                   ["noprint", "latitude", "longitude", "elevation"])
                  || ((Map.findWithDefault [] "id" a) `elem` ["coordinates"])
                then return "" else walk "" l ""
              else walk "" l ""
        nodeToHtml (Environment Tag (TagAttr "img" m) _)
          | (Map.lookup "class" m) == (Just "mwe-math-fallback-image-inline")
            = return []
        nodeToHtml (Environment Comment _ _) = return []
        nodeToHtml (Environment Preformat (TagAttr "pre" _) l)
          = walk "<pre>" l "</pre>"
        nodeToHtml (Environment Math (TagAttr "math" _) l)
          = do st <- get
               return
                 ("<img src=\"./formulas/" ++
                    (hex (show (hash (mathTransform l)))) ++
                      ".png\" style=\" width:" ++
                        (show
                           ((((forms st) Map.!
                                ((hex (show (hash (mathTransform l)))) ++ ".png"))
                               * 2)
                              `div` 3))
                          ++ "px;\" />")
        nodeToHtml (Environment Math _ l)
          = do st <- get
               return
                 ("<img src=\"./formulas/" ++
                    (hex (show (hash (mathTransform l)))) ++
                      ".png\" style=\" width:" ++
                        (show
                           ((((forms st) Map.!
                                ((hex (show (hash (mathTransform l)))) ++ ".png"))
                               * 2)
                              `div` 3))
                          ++ "px;\" />")
        nodeToHtml (Environment Tag (TagAttr "table" m) l)
          = do st <- get
               put $ st{getInTab = (getInTab st) + 1}
               d <- walk ("<table " ++ (writedict (Map.toList m)) ++ ">") l
                      ("</table>")
               st2 <- get
               put $ st2{getInTab = (getInTab st)}
               return d
        nodeToHtml (Environment Gallery _ l)
          = do st <- get
               put st{getInGallery = True}
               d <- galleryToHtml l
               st2 <- get
               put $ (newst st2){getInGallery = (getInGallery st)}
               return d
          where midst i = i{getInGallery = False}
                gins i = generateGalleryImageNumbers i (midst i)
                newst i
                  = (midst i){getGalleryNumbers =
                                (getGalleryNumbers (midst i)) ++ (map toInteger (gins i))}
        nodeToHtml (Environment Tag (TagAttr "span" _) l) = walk "" l ""
        nodeToHtml (Environment Tag (TagAttr x m) l)
          = walk ("<" ++ x ++ " " ++ (writedict (Map.toList m)) ++ ">") l
              ("</" ++ x ++ ">")
        nodeToHtml (Environment _ _ l) = walk "" l ""
        nodeToHtml _ = return []
