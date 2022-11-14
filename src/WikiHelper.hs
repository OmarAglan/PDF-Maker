{-DHUN| general helper function for converting from the parsetree to latex DHUN-}
module WikiHelper where
import Tools
import MediaWikiParseTree
import Data.Char
import Data.List.Split
import MagicStrings
import qualified Data.Map as Map
import MyState
import Data.List
import Data.String.HT (trim)

{-DHUN| add latex label elements to the parse tree. chapter section subsection and so on will be referenced by these labels. It takes the initial UrlState as second parameter. And the parse tree to be processed as first parameter. It returns a tuple. The first element is them modified urlstate and the second is the modified parse tree with the labels added. The field sUrlState of UrlState contains the page name of the downloaded page currently being processed by the algorithm. the filed mUrlState of UrlState is the number of the currenty label. And ,UrlState is a mapping from combined page and chapter, section etc. names (like urls) to their label numbers.    DHUN-}

makeLables ::
           [Anything Char] -> UrlState -> (UrlState, [Anything Char])
makeLables ll states
  = let (f, s) = mapAccumL makeLablesForNode states ll in
      (f, concat s)
  where makeLablesForNode ::
                          UrlState -> Anything Char -> (UrlState, [Anything Char])
        makeLablesForNode st (Environment DhunUrl ss l)
          = (st{iUrlState = (iUrlState st) + 1, sUrlState = yy,
                mUrlState = Map.insert yy lab (mUrlState st)},
             [Environment DhunUrl ss l] ++ [Environment Label (Str lab) []])
          where yy = (replace2 (shallowFlatten l) " " "_")
                lab = (show . iUrlState $ st)
        makeLablesForNode st (Environment Wikiheading (Str ss) l)
          = (st{iUrlState = (iUrlState st) + 1,
                mUrlState =
                  Map.insert ((sUrlState st) ++ "#" ++ yy) lab (mUrlState st)},
             [Environment Wikiheading (Str ss) l] ++
               [Environment Label (Str lab) []])
          where lab = (show . iUrlState $ st)
                yy = (replace2 (trim (shallowFlatten l)) " " "_")
        makeLablesForNode st (Environment e s l)
          = (fst zz, [Environment e s (snd $ zz)])
          where zz = makeLables l st
        makeLablesForNode st x = (st, [x])

{-DHUN| remove superfluous br html tags from the parse tree. Always run before converting the parse tree to latex DHUN-}

removeBr :: [Anything Char] -> [Anything Char]
removeBr ((C '\n') : ((Environment Tag (TagAttr "br" _) _) : xs))
  = (C '\n') : removeBr xs
removeBr
  ((Environment Tag (TagAttr "br" _) _) :
     ((Environment Tag (TagAttr "br" a2) l2) : xs))
  = removeBr ((Environment Tag (TagAttr "br" a2) l2) : xs)
removeBr
  ((Environment Wikilink s1 l1) :
     ((Environment Tag (TagAttr "br" a2) l2) : xs))
  = if isImage (shallowFlatten l1) then
      removeBr ((Environment Wikilink s1 l1) : xs) else
      (Environment Wikilink s1 l1) :
        (removeBr ((Environment Tag (TagAttr "br" a2) l2) : xs))
removeBr ((Environment SpaceIndent x l) : xs)
  = (Environment SpaceIndent x (removeBr l)) : removeBr xs
removeBr (x : xs) = x : removeBr xs
removeBr [] = []

{-DHUN| checks if a given string is an image inclusion in wiki notation. In wiki notation an image is included by [[Image:FooBar.png]], but image may be replace by localized versions like Bild in German, so this function checks for those and return true if it seems to be an image DHUN-}

isImage :: String -> Bool
isImage x
  = ([z | z <- map (++ ":") imgtags, z `isPrefixOf` (map toLower x)]
       /= [])

{-DHUN| flattens a parse tree shallowly. that is take all characters on the surface level of the parse tree and combines them into a single string. It does not decent into substructures of the parse and so neglects all characters there and does not return those with the exception of character in SpaceIndent environments directly attached to the surface level DHUN-}

shallowFlatten :: [Anything Char] -> String
shallowFlatten ((C a) : xs) = a : (shallowFlatten xs)
shallowFlatten ((Environment HtmlChar (Str "quot") _) : xs)
  = '"' : (shallowFlatten xs)
shallowFlatten ((Environment HtmlChar (Str "amp") _) : xs)
  = '&' : (shallowFlatten xs)
shallowFlatten ((Environment HtmlChar (Str "lt") _) : xs)
  = '<' : (shallowFlatten xs)
shallowFlatten ((Environment HtmlChar (Str "gt") _) : xs)
  = '>' : (shallowFlatten xs)
shallowFlatten ((Environment NumHtml (Str s) _):xs)
   = let h= (case reads s of
                  [] -> case
                          do z <- case s of
                                      ('x' : xxs) -> Just xxs
                                      ('X' : xxs) -> Just xxs
                                      _ -> Nothing
                             g <- unhex z
                             return g
                          of
                            Just x -> chr . fromIntegral $ x
                            Nothing -> '?'
                  (x : _) -> chr . fst $ x) in h: (shallowFlatten xs) 
shallowFlatten ((Environment SpaceIndent _ l) : xs)
  = '\n' : ((shallowFlatten l) ++ (shallowFlatten xs))
shallowFlatten (_ : xs) = shallowFlatten xs
shallowFlatten [] = []



{-DHUN| A link in wiki notation is given by [foorbar.com a caption]. This function returns the location the link points to so foobar.com as String. It takes the parse tree representation of the  link as input parameter DHUN-}

linkLocation :: [Anything Char] -> String
linkLocation l
  = case yy of
        [] -> ""
        (z : _) -> z
  where xx = (splitOn " " (shallowFlatten l))
        yy
          = splitOn "|"
              (case xx of
                   [] -> ""
                   (g : _) -> g)

normalizeExtensionHtml :: String -> String
normalizeExtensionHtml ('s' : ('v' : ('g' : _))) = "svg"
normalizeExtensionHtml ('j' : ('p' : ('e' : ('g' : _)))) = "jpg"
normalizeExtensionHtml ('j' : ('p' : ('g' : _))) = "jpg"
normalizeExtensionHtml ('g' : ('i' : ('f' : _))) = "gif"
normalizeExtensionHtml ('p' : ('n' : ('g' : _))) = "png"
normalizeExtensionHtml ('t' : ('i' : ('f' : ('f' : _)))) = "png"
normalizeExtensionHtml ('t' : ('i' : ('f' : _))) = "png"
normalizeExtensionHtml  ('s' : ('t' : ('l' : _))) = "png"
normalizeExtensionHtml  ('x' : ('c' : ('f' : _))) = "png"
normalizeExtensionHtml  ('d' : ('j' : ('v' :('u': _)))) = "png"
normalizeExtensionHtml  ('w' : ('e' : ('b' :('p': _)))) = "png"
normalizeExtensionHtml (' ' : xs) = normalizeExtension xs
normalizeExtensionHtml (x : xs) = x : (normalizeExtension xs)
normalizeExtensionHtml [] = []

{-DHUN| changes the extension for a filename given in the wiki source to the extension to be used in the latex document. For example gif documents are converted to png documents. So this function converts the string 'gif' to the string 'png' DHUN-}

normalizeExtension :: String -> String
normalizeExtension ('s' : ('v' : ('g' : _))) = "\\SVGExtension"
normalizeExtension ('j' : ('p' : ('e' : ('g' : _)))) = "jpg"
normalizeExtension ('j' : ('p' : ('g' : _))) = "jpg"
normalizeExtension ('g' : ('i' : ('f' : _))) = "png"
normalizeExtension ('p' : ('n' : ('g' : _))) = "png"
normalizeExtension ('t' : ('i' : ('f' : ('f' : _)))) = "png"
normalizeExtension ('t' : ('i' : ('f' : _))) = "png"
normalizeExtension ('s' : ('t' : ('l' : _))) = "png"
normalizeExtension ('x' : ('c' : ('f' : _))) = "png"
normalizeExtension ('d' : ('j' : ('v' :('u': _)))) = "png"
normalizeExtension  ('w' : ('e' : ('b' :('p': _)))) = "png"
normalizeExtension (' ' : xs) = normalizeExtension xs
normalizeExtension (x : xs) = x : (normalizeExtension xs)
normalizeExtension [] = []

{-DHUN| changes the extension for a filename given in the wiki source to the extension to be used in as filename when storing the image in the latex tree. DHUN-}

normalizeExtension2 :: String -> String
normalizeExtension2 ('s' : ('v' : ('g' : _))) = "svg"
normalizeExtension2 ('j' : ('p' : ('e' : ('g' : _)))) = "jpg"
normalizeExtension2 ('j' : ('p' : ('g' : _))) = "jpg"
normalizeExtension2 ('g' : ('i' : ('f' : _))) = "gif"
normalizeExtension2 ('p' : ('n' : ('g' : _))) = "png"
normalizeExtension2 ('t' : ('i' : ('f' : ('f' : _)))) = "tif"
normalizeExtension2 ('t' : ('i' : ('f' : _))) = "tif"
normalizeExtension2 ('s' : ('t' : ('l' : _))) = "stl"
normalizeExtension2 ('x' : ('c' : ('f' : _))) = "xcf"
normalizeExtension2 ('d' : ('j' : ('v' :('u': _)))) = "djvu"
normalizeExtension2  ('w' : ('e' : ('b' :('p': _)))) = "webp"
normalizeExtension2 (' ' : xs) = normalizeExtension xs
normalizeExtension2 (x : xs) = x : (normalizeExtension xs)
normalizeExtension2 [] = []

{-DHUN| returns the extension of a filename. DHUN-}

fileNameToExtension :: String -> String
fileNameToExtension s = last (splitOn "." (map toLower s))

{-DHUN| a predicate that can be run on an element of a parse tree that returns true if the element is a wikilink. A wikilink is denoted as [[Foobar]] in the wiki notation, an links to an other mediawiki page on the same or a different wiki DHUN-}

isWikiLink :: (Anything Char) -> Bool
isWikiLink (Environment Wikilink _ []) = False
isWikiLink (Environment Wikilink _ _) = True
isWikiLink _ = False

{-DHUN| changes Math elements on the surface level of a parse tree to bigmath elements. Those will be rendered as equation environments. Normal math is usually only display be the dollar math environment in latex.  DHUN-}

shallowEnlargeMath :: [Anything Char] -> [Anything Char]
shallowEnlargeMath ((Environment Math s l) : xs)
  = (Environment BigMath s l) : shallowEnlargeMath xs
shallowEnlargeMath (x : xs) = x : shallowEnlargeMath xs
shallowEnlargeMath [] = []

{-DHUN| this function modified chapter headings. It is used when converting a chapter heading from the parse tree to latex. In the wiki often the page name is used as chapter heading and thus only the parts after the slashes and colons are to be taken into account. Also underscores have to be replaced by spaces DHUN-}

chapterTransform :: String -> String
chapterTransform s
  = replace '_' ' ' (last (splitOn ":" (last (splitOn "/" s))))

{-DHUN| returns the separator to separate items in enumeration, itemizations and so on. Currently this is always \\item{} but this may change depending on which latex package is used to display enumerations and so on. Takes the char for this type of enumeration etc. in wiki notation. That is a hash for enumeration and a asterisk for itemization and so on DHUN-}

itemSeperator :: Char -> String
itemSeperator c = itemSeperator2 [c]

{-DHUN| see documentation on itemSeperator. The only difference is that this function takes a string containing a single character instead of the single character itself DHUN-}

itemSeperator2 :: String -> String
itemSeperator2 "#" = "\\item{}"
itemSeperator2 ":" = "\\item{}"
itemSeperator2 ";" = "\\item{}"
itemSeperator2 "*" = "\\item{}"
itemSeperator2 _ = "\\item{}"

{-DHUN| returns the name of a latex environment for an itemization enumeration etc.. The first parameter is a string in wiki notation and declare which type environment should be used. The second parameter is a float giving the width of the current cell in units of the line width when inside a table and is 1.0 if currently not inside any table. DHUN-}

itemEnvironmentName :: String -> Float -> String
itemEnvironmentName "#" _ = "myenumerate"
itemEnvironmentName ":" _ = "myquote"
itemEnvironmentName ";" _ = "mydescription"
itemEnvironmentName "*" _ = "myitemize"
itemEnvironmentName _ _ = "list"

{-DHUN| returns additional parameter for the opening of a latex environment for an itemization enumeration etc. The second parameter is a float giving the width of the current cell in units of the line width when inside a table and is 1.0 if currently not inside any table. DHUN-}

itemEnvironmentParameters :: String -> Float -> String
itemEnvironmentParameters "#" _ = ""
itemEnvironmentParameters ":" _ = ""
itemEnvironmentParameters ";" _ = ""
itemEnvironmentParameters "*" _ = ""
itemEnvironmentParameters _ _ = "{\\labelitemi}{\\leftmargin=1em}"

{-DHUN| do multple replacements in a row. The first argument is haystack. The second one a list of pair of a needle and a corresponding nail. The haystack with each needle replaced by a nail is returned DHUN-}

multireplace :: (Eq a) => [a] -> [([a], [a])] -> [a]
multireplace haystack ((needle, nail) : xs)
  = multireplace (replace2 haystack needle nail) xs
multireplace haystack [] = haystack

{-DHUN| converts a mathematical expression (that is something within a math tag in the wiki) to an latex expression. Essentially the wiki is using latex. But it allows for some extra features that are take care of in this transformation DHUN-}

mathTransform :: [Anything Char] -> String
mathTransform x
  = multireplace (replace '\n' ' ' (shallowFlatten x)) replist

{-DHUN| list of replacements to be applied to contents of math tags in wiki notation for use in the latex equation environment DHUN-}

replist :: [([Char], [Char])]
replist
  = [("\\or", "\\vee{}"), ("%", "\\%"), ("\\and", "\\wedge{}"),
     ("\\begin{align}", "\\begin{aligned}"),
     ("\\end{align}", "\\end{aligned}"), ("\\\\%", "\\%"),
     ("\\part ", "\\partial "), ("\\part{", "\\partial{"), ("\\;", ""),
     ("\\|", "\\Vert"), ("\\!", ""), ("\\part\\", "\\partial\\"),  ("&#10;", "{\\newline}"),
     ("&#39;", "'")]

{-DHUN| helper function for line breaking in source code and preformatted block. Not to be called from outside this module. Converts character to parse tree entities, to be process by breakLinesHelper3 DHUN-}

breakLinesHelper4 :: [Anything Char] -> [Anything Char]
breakLinesHelper4 ((C '\n') : xs)
  = (Environment Tag (TagAttr "br" Map.empty) []) :
      breakLinesHelper4 xs
breakLinesHelper4 ((C '\t') : xs) = Tab : breakLinesHelper4 xs
breakLinesHelper4 ((C ' ') : xs) = Quad : breakLinesHelper4 xs
breakLinesHelper4 (x : xs) = x : breakLinesHelper4 xs
breakLinesHelper4 [] = []

{-DHUN| the width of a tab character in spaces DHUN-}

tabwidth :: Int
tabwidth = 4

{-DHUN| helper function for line breaking in source code and preformatted block. Not to be called from outside this module. Inserts br tags where line breaks are needed, the first parameter is an integer which represents the current column in the text, it should be zero when this function is called externally. The second parameter is an integer an represents the maximum length of the line in characters. The third input parameter is the code block to which the line breaks should be added in parse tree notation. The function returns the code block with added br tags for the line breaks in parse tree notation DHUN-}

breakLinesHelper3 ::
                  Int -> Int -> [Anything Char] -> [Anything Char]
breakLinesHelper3 _ m ((Environment Tag (TagAttr "br" y) []) : xs)
  | y == Map.empty =
    (Environment Tag (TagAttr "br" Map.empty) []) :
      breakLinesHelper3 0 m xs
breakLinesHelper3 i m (Tab : xs)
  = if i + wl >= m then
      (Environment Tag (TagAttr "br" Map.empty) []) :
        Tab : breakLinesHelper3 0 m xs
      else Tab : breakLinesHelper3 (i + tabwidth) m xs
  where wlb = length (takeWhile fun xs)
        wl = if (wlb < m) then wlb else 0
        fun x
          = (x /= Quad) &&
              (x /= (Environment Tag (TagAttr "br" Map.empty) []))
breakLinesHelper3 i m (x : xs)
  = if i + wl >= m then
      (Environment Tag (TagAttr "br" Map.empty) []) :
        x : breakLinesHelper3 0 m xs
      else x : breakLinesHelper3 (i + 1) m xs
  where wlb = length (takeWhile (fun) xs)
        wl = if (wlb < m) then wlb else 0
        fun xx
          = (xx /= Quad) &&
              (xx /= (Environment Tag (TagAttr "br" Map.empty) []))
breakLinesHelper3 _ _ [] = []

{-DHUN| Breaks lines in source code and preformatted block. Inserts br tags where line breaks are needed. The first parameter is an integer an represents the maximum length of the line in characters. The second input parameter is the code block to which the line breaks should be added in parse tree notation. The function returns the code block with added br tags for the line breaks in parse tree notation DHUN-}

breakLines3 :: Int -> [Anything Char] -> [Anything Char]
breakLines3 m s
  = rebreak (breakLinesHelper3 0 m (breakLinesHelper4 s))

{-DHUN| Adds quads in between double br line breaks, needed since double \\newline is not allowed in latex DHUN-}

rebreak :: [Anything Char] -> [Anything Char]
rebreak
  ((Environment Tag (TagAttr "br" a) l) :
     ((Environment Tag (TagAttr "br" a2) l2) : xs))
  = (Environment Tag (TagAttr "br" a) l) :
      Quad : (rebreak ((Environment Tag (TagAttr "br" a2) l2) : xs))
rebreak (x : xs) = x : (rebreak xs)
rebreak [] = []

{-DHUN| Replaces several parse tree item representations of white space characters with the corresponding whitespace characters themselves in parse tree notation. DHUN-}

renormalize :: Anything Char -> Anything Char
renormalize (Environment Tag (TagAttr "br" _) []) = C '\n'
renormalize Quad = C ' '
renormalize Tab = C '\t'
renormalize x = x
