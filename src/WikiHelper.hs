-- | General helper function for converting from the parsetree to latex
module WikiHelper where

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.String.HT (trim)
import MagicStrings
import MediaWikiParseTree
import MyState
import Tools

-- | Add latex label elements to the parse tree. chapter section subsection and
-- so on will be referenced by these labels. It takes the initial UrlState as
-- second parameter. And the parse tree to be processed as first parameter. It
-- returns a tuple. The first element is the modified urlstate and the second is
-- the modified parse tree with the labels added. The field sUrlState of
-- UrlState contains the page name of the downloaded page currently being
-- processed by the algorithm. the field iUrlState of UrlState is the number of
-- the current label. An mUrlState is a mapping from combined page and chapter,
-- section etc. names (like urls) to their label numbers.
makeLables ::
  -- | The input parse tree without the labels
  [Anything Char] ->
  -- | The initial UrlState
  UrlState ->
  -- | A pair. The first element is the modified UrlState. The second element is
  -- the modified parse tree with the labels added.
  (UrlState, [Anything Char])
makeLables ll states =
  let (f, s) = mapAccumL makeLablesForNode states ll
   in (f, concat s)
  where
    makeLablesForNode ::
      UrlState -> Anything Char -> (UrlState, [Anything Char])
    makeLablesForNode st (Environment DhunUrl ss l) =
      ( st
          { iUrlState = (iUrlState st) + 1,
            sUrlState = yy,
            mUrlState = Map.insert yy lab (mUrlState st)
          },
        [Environment DhunUrl ss l] ++ [Environment Label (Str lab) []]
      )
      where
        yy = (replace2 (shallowFlatten l) " " "_")
        lab = (show . iUrlState $ st)
    makeLablesForNode st (Environment Wikiheading (Str ss) l) =
      ( st
          { iUrlState = (iUrlState st) + 1,
            mUrlState =
              Map.insert ((sUrlState st) ++ "#" ++ yy) lab (mUrlState st)
          },
        [Environment Wikiheading (Str ss) l]
          ++ [Environment Label (Str lab) []]
      )
      where
        lab = (show . iUrlState $ st)
        yy = (replace2 (trim (shallowFlatten l)) " " "_")
    makeLablesForNode st (Environment e s l) =
      (fst zz, [Environment e s (snd $ zz)])
      where
        zz = makeLables l st
    makeLablesForNode st x = (st, [x])

-- | Remove superfluous br html tags from the parse tree. Always run before
-- converting the parse tree to latex
removeBr ::
  -- | The input parse tree with possibly superfluous br HTML tags.
  [Anything Char] ->
  -- | The output parse tree with the superfluous br HTML tags removed.
  [Anything Char]
removeBr ((C '\n') : ((Environment Tag (TagAttr "br" _) _) : xs)) =
  (C '\n') : removeBr xs
removeBr
  ( (Environment Tag (TagAttr "br" _) _)
      : ((Environment Tag (TagAttr "br" a2) l2) : xs)
    ) =
    removeBr ((Environment Tag (TagAttr "br" a2) l2) : xs)
removeBr
  ( (Environment Wikilink s1 l1)
      : ((Environment Tag (TagAttr "br" a2) l2) : xs)
    ) =
    if isImage (shallowFlatten l1)
      then removeBr ((Environment Wikilink s1 l1) : xs)
      else
        (Environment Wikilink s1 l1)
          : (removeBr ((Environment Tag (TagAttr "br" a2) l2) : xs))
removeBr ((Environment SpaceIndent x l) : xs) =
  (Environment SpaceIndent x (removeBr l)) : removeBr xs
removeBr (x : xs) = x : removeBr xs
removeBr [] = []

-- | Checks if a given string is an image inclusion in wiki notation. In wiki
-- notation an image is included by [[Image:FooBar.png]], but image may be
-- replace by localized versions like Bild in German, so this function checks
-- for those and return true if it seems to be an image
isImage ::
  -- | The wiki link to be checked
  String ->
  -- | True if the wiki link is an image inclusion, False otherwise
  Bool
isImage x =
  ( [z | z <- map (++ ":") imgtags, z `isPrefixOf` (map toLower x)]
      /= []
  )

-- | Flattens a parse tree shallowly. that is take all characters on the surface
-- level of the parse tree and combines them into a single string. It does not
-- decent into substructures of the parse and so neglects all characters there
-- and does not return those with the exception of character in SpaceIndent
-- environments directly attached to the surface level
shallowFlatten ::
  -- | The parse tree to be flattened.
  [Anything Char] ->
  -- | The flattened version of the parse tree given in the first parameter.
  String
shallowFlatten ((C a) : xs) = a : (shallowFlatten xs)
shallowFlatten ((Environment HtmlChar (Str "quot") _) : xs) =
  '"' : (shallowFlatten xs)
shallowFlatten ((Environment HtmlChar (Str "amp") _) : xs) =
  '&' : (shallowFlatten xs)
shallowFlatten ((Environment HtmlChar (Str "lt") _) : xs) =
  '<' : (shallowFlatten xs)
shallowFlatten ((Environment HtmlChar (Str "gt") _) : xs) =
  '>' : (shallowFlatten xs)
shallowFlatten ((Environment NumHtml (Str s) _) : xs) =
  let h =
        ( case reads s of
            [] -> case do
              z <- case s of
                ('x' : xxs) -> Just xxs
                ('X' : xxs) -> Just xxs
                _ -> Nothing
              g <- unhex z
              return g of
              Just x -> chr . fromIntegral $ x
              Nothing -> '?'
            (x : _) -> chr . fst $ x
        )
   in h : (shallowFlatten xs)
shallowFlatten ((Environment SpaceIndent _ l) : xs) =
  '\n' : ((shallowFlatten l) ++ (shallowFlatten xs))
shallowFlatten (_ : xs) = shallowFlatten xs
shallowFlatten [] = []

-- | A link in wiki notation is given by [foorbar.com a caption]. This function
-- returns the location the link points to so foobar.com as String. It takes the
-- parse tree representation of the link as input parameter
linkLocation ::
  -- | The parse tree representation of the contents of a link.
  [Anything Char] ->
  -- | The URL the link points to,
  String
linkLocation l =
  case yy of
    [] -> ""
    (z : _) -> z
  where
    xx = (splitOn " " (shallowFlatten l))
    yy =
      splitOn
        "|"
        ( case xx of
            [] -> ""
            (g : _) -> g
        )

-- | Changes the extension for a filename given in the wiki source to the
-- extension to be used in the HTML document. For example tif documents are
-- converted to png documents. So this function converts the string 'tif' to the
-- string 'png'
normalizeExtensionHtml ::
  -- | The extension of a filename as found in the wiki.
  String ->
  -- | The extension of the filename to be used when generating HTML code.
  String
normalizeExtensionHtml ('s' : ('v' : ('g' : _))) = "svg"
normalizeExtensionHtml ('j' : ('p' : ('e' : ('g' : _)))) = "jpg"
normalizeExtensionHtml ('j' : ('p' : ('g' : _))) = "jpg"
normalizeExtensionHtml ('g' : ('i' : ('f' : _))) = "gif"
normalizeExtensionHtml ('p' : ('n' : ('g' : _))) = "png"
normalizeExtensionHtml ('t' : ('i' : ('f' : ('f' : _)))) = "png"
normalizeExtensionHtml ('t' : ('i' : ('f' : _))) = "png"
normalizeExtensionHtml ('s' : ('t' : ('l' : _))) = "png"
normalizeExtensionHtml ('x' : ('c' : ('f' : _))) = "png"
normalizeExtensionHtml ('d' : ('j' : ('v' : ('u' : _)))) = "png"
normalizeExtensionHtml ('w' : ('e' : ('b' : ('p' : _)))) = "png"
normalizeExtensionHtml (' ' : xs) = normalizeExtension xs
normalizeExtensionHtml (x : xs) = x : (normalizeExtension xs)
normalizeExtensionHtml [] = []

-- | Changes the extension for a filename given in the wiki source to the
-- extension to be used in the LaTeX document. For example gif documents are
-- converted to png documents. So this function converts the string 'gif' to the
-- string 'png'
normalizeExtension ::
  -- | The extension of a filename as found in the wiki.
  String ->
  -- | The extension of the filename to be used when generating LaTeX code.
  String
normalizeExtension ('s' : ('v' : ('g' : _))) = "\\SVGExtension"
normalizeExtension ('j' : ('p' : ('e' : ('g' : _)))) = "jpg"
normalizeExtension ('j' : ('p' : ('g' : _))) = "jpg"
normalizeExtension ('g' : ('i' : ('f' : _))) = "png"
normalizeExtension ('p' : ('n' : ('g' : _))) = "png"
normalizeExtension ('t' : ('i' : ('f' : ('f' : _)))) = "png"
normalizeExtension ('t' : ('i' : ('f' : _))) = "png"
normalizeExtension ('s' : ('t' : ('l' : _))) = "png"
normalizeExtension ('x' : ('c' : ('f' : _))) = "png"
normalizeExtension ('d' : ('j' : ('v' : ('u' : _)))) = "png"
normalizeExtension ('w' : ('e' : ('b' : ('p' : _)))) = "png"
normalizeExtension (' ' : xs) = normalizeExtension xs
normalizeExtension (x : xs) = x : (normalizeExtension xs)
normalizeExtension [] = []

-- | Changes the extension for a filename given in the wiki source to the
-- extension to be used in as filename when storing the image in the latex tree.
-- For example 'jpeg' gets converted to 'jpg'
normalizeExtension2 ::
  -- | The extension of a filename as found in the wiki.
  String ->
  -- | The extension of the filename when writing the LaTeX tree for the image
  -- conversion procedure.
  String
normalizeExtension2 ('s' : ('v' : ('g' : _))) = "svg"
normalizeExtension2 ('j' : ('p' : ('e' : ('g' : _)))) = "jpg"
normalizeExtension2 ('j' : ('p' : ('g' : _))) = "jpg"
normalizeExtension2 ('g' : ('i' : ('f' : _))) = "gif"
normalizeExtension2 ('p' : ('n' : ('g' : _))) = "png"
normalizeExtension2 ('t' : ('i' : ('f' : ('f' : _)))) = "tif"
normalizeExtension2 ('t' : ('i' : ('f' : _))) = "tif"
normalizeExtension2 ('s' : ('t' : ('l' : _))) = "stl"
normalizeExtension2 ('x' : ('c' : ('f' : _))) = "xcf"
normalizeExtension2 ('d' : ('j' : ('v' : ('u' : _)))) = "djvu"
normalizeExtension2 ('w' : ('e' : ('b' : ('p' : _)))) = "webp"
normalizeExtension2 (' ' : xs) = normalizeExtension xs
normalizeExtension2 (x : xs) = x : (normalizeExtension xs)
normalizeExtension2 [] = []

-- | Returns the extension of a filename.
fileNameToExtension ::
  -- | The filename for which the extension is requested
  String ->
  -- | The extension of the given filename
  String
fileNameToExtension s = last (splitOn "." (map toLower s))

-- | A predicate that can be run on an element of a parse tree that returns true
-- if the element is a wikilink and not the empty wikilink. A wikilink is
-- denoted as [[Foobar]] in the wiki notation, an links to an other MediaWiki
-- page on the same or a different wiki
isWikiLink ::
  -- | The parse tree element to be checked
  (Anything Char) ->
  -- | True if the element is a non empty wikilink, False otherwise.
  Bool
isWikiLink (Environment Wikilink _ []) = False
isWikiLink (Environment Wikilink _ _) = True
isWikiLink _ = False

-- | Changes math elements on the surface level of a parse tree to bigmath
-- elements. Those will be rendered as equation environments. Normal math is
-- usually only display be the dollar math environment in latex.
shallowEnlargeMath ::
  -- | The parse tree section in which to put equation in the LaTeX equation
  -- environment instead of the LaTeX dollar environment.
  [Anything Char] ->
  -- | The input parse tree which all math elements at surface level are
  -- replaced by the corresponding bigmath elements.
  [Anything Char]
shallowEnlargeMath ((Environment Math s l) : xs) =
  (Environment BigMath s l) : shallowEnlargeMath xs
shallowEnlargeMath (x : xs) = x : shallowEnlargeMath xs
shallowEnlargeMath [] = []

-- | Returns the separator to separate items in enumeration, itemization and so
-- on. Currently this is always \\item{} but this may change depending on which
-- latex package is used to display enumerations and so on. Takes the char for
-- this type of enumeration etc. in wiki notation. That is a hash for
-- enumeration and a asterisk for itemization and so on.
itemSeperator ::
  -- | The item separator in MediaWiki notation.
  Char ->
  -- | The item separator in LaTeX notation.
  String
itemSeperator c = itemSeperator2 [c]

-- | See documentation on `itemSeperator`. The only difference is that this
-- function takes a string containing a single character instead of the single
-- character itself.
itemSeperator2 ::
  -- | The item separator in MediaWiki notation.
  String ->
  -- | The item separator in LaTeX notation.
  String
itemSeperator2 "#" = "\\item{}"
itemSeperator2 ":" = "\\item{}"
itemSeperator2 ";" = "\\item{}"
itemSeperator2 "*" = "\\item{}"
itemSeperator2 _ = "\\item{}"

-- | Returns the name of a latex environment for an itemization, enumeration
-- etc.. The first parameter is a string in wiki notation and declare which type
-- environment should be used. The second parameter is a float giving the width
-- of the current cell in units of the line width when inside a table and is 1.0
-- if currently not inside any table.
itemEnvironmentName ::
  -- | The item separator in LaTeX notation.
  String ->
  -- | A float that is equal to 1.0 when outside a table and smaller than 1.0
  -- when inside a table.
  Float ->
  -- | The name of the environment in LaTeX notation.
  String
itemEnvironmentName "#" _ = "myenumerate"
itemEnvironmentName ":" _ = "myquote"
itemEnvironmentName ";" _ = "mydescription"
itemEnvironmentName "*" _ = "myitemize"
itemEnvironmentName _ _ = "list"

-- | Returns additional parameter for the opening of a latex environment for an
-- itemization enumeration etc. The second parameter is a float giving the width
-- of the current cell in units of the line width when inside a table and is 1.0
-- if currently not inside any table.
itemEnvironmentParameters ::
  -- | The item separator in LaTeX notation.
  String ->
  -- | A float that is equal to 1.0 when outside a table and smaller than 1.0
  -- when inside a table.
  Float ->
  -- | Additional parameter for the opening of a LaTeX environment.
  String
itemEnvironmentParameters "#" _ = ""
itemEnvironmentParameters ":" _ = ""
itemEnvironmentParameters ";" _ = ""
itemEnvironmentParameters "*" _ = ""
itemEnvironmentParameters _ _ = "{\\labelitemi}{\\leftmargin=1em}"

-- | Do multople replacements in a row. The first argument is haystack. The
-- second one a list of pair of a needle and a corresponding nail. The haystack
-- with each needle replaced by a nail is returned. See also documentation of
-- `replace2`
multireplace ::
  (Eq a) =>
  -- | The haystack
  [a] ->
  -- | A list of pairs. The first element of each pair is a needle. The second
  -- element of each pair is a nail.
  [([a], [a])] ->
  -- | The haystack with all needles replaced by nails.
  [a]
multireplace haystack ((needle, nail) : xs) =
  multireplace (replace2 haystack needle nail) xs
multireplace haystack [] = haystack

-- | list of replacements to be applied to contents of math tags in wiki
-- notation for use in the latex equation environment
replist ::
  -- | A list of pairs. The first element is the code found in math environments
  -- in the wiki which should be replaced. The second element of each is its
  -- replacement in LaTeX code.
  [([Char], [Char])]
replist =
  [ ("\\or", "\\vee{}"),
    ("%", "\\%"),
    ("\\and", "\\wedge{}"),
    ("\\begin{align}", "\\begin{aligned}"),
    ("\\end{align}", "\\end{aligned}"),
    ("\\\\%", "\\%"),
    ("\\part ", "\\partial "),
    ("\\part{", "\\partial{"),
    ("\\;", ""),
    ("\\|", "\\Vert"),
    ("\\!", ""),
    ("\\part\\", "\\partial\\"),
    ("&#10;", "{\\newline}"),
    ("&#39;", "'")
  ]

-- | Helper function for line breaking in source code and preformatted blocks.
-- Not to be called from outside this module. Converts character to parse tree
-- entities, to be processed by `breakLinesHelper3`
breakLinesHelper4 ::
  -- | The parse tree section of a source code or preformatted block as parsed
  -- from the wiki
  [Anything Char] ->
  -- | The parse tree section given as input with the necessary replacement to
  -- make it ready for processing with the `breakLinesHelper3` function in the
  -- module.
  [Anything Char]
breakLinesHelper4 ((C '\n') : xs) =
  (Environment Tag (TagAttr "br" Map.empty) [])
    : breakLinesHelper4 xs
breakLinesHelper4 ((C '\t') : xs) = Tab : breakLinesHelper4 xs
breakLinesHelper4 ((C ' ') : xs) = Quad : breakLinesHelper4 xs
breakLinesHelper4 (x : xs) = x : breakLinesHelper4 xs
breakLinesHelper4 [] = []

-- | the width of a tab character in spaces
tabwidth ::
  -- | the width of the tab character in spaces
  Int
tabwidth = 4

-- | Helper function for line breaking in source code and preformatted block.
-- Not to be called from outside this module. Inserts br tags where line breaks
-- are needed.
breakLinesHelper3 ::
  -- | The current column in the text.
  Int ->
  -- | The maximum length of the line in characters. Tt should be zero when this
  -- function is called externally.
  Int ->
  -- | The code block in parse tree notation to which line breaks shall be
  -- added.
  [Anything Char] ->
  -- | The code block given as third parameter with the necessary br HTML tags
  -- added to ensure line breaks occur as often as needed to satisfy the maximum
  -- characters per line limit given in the second parameter.
  [Anything Char]
breakLinesHelper3 _ m ((Environment Tag (TagAttr "br" y) []) : xs)
  | y == Map.empty =
      (Environment Tag (TagAttr "br" Map.empty) [])
        : breakLinesHelper3 0 m xs
breakLinesHelper3 i m (Tab : xs) =
  if i + wl >= m
    then
      (Environment Tag (TagAttr "br" Map.empty) [])
        : Tab
        : breakLinesHelper3 0 m xs
    else Tab : breakLinesHelper3 (i + tabwidth) m xs
  where
    wlb = length (takeWhile fun xs)
    wl = if (wlb < m) then wlb else 0
    fun x =
      (x /= Quad)
        && (x /= (Environment Tag (TagAttr "br" Map.empty) []))
breakLinesHelper3 i m (x : xs) =
  if i + wl >= m
    then
      (Environment Tag (TagAttr "br" Map.empty) [])
        : x
        : breakLinesHelper3 0 m xs
    else x : breakLinesHelper3 (i + 1) m xs
  where
    wlb = length (takeWhile (fun) xs)
    wl = if (wlb < m) then wlb else 0
    fun xx =
      (xx /= Quad)
        && (xx /= (Environment Tag (TagAttr "br" Map.empty) []))
breakLinesHelper3 _ _ [] = []

-- | Breaks lines in source code and preformatted block. Inserts br tags where
-- line breaks are needed.
breakLines3 ::
  -- | An integer that represents the maximum length of the line in characters.
  Int ->
  -- | the code block to which the line breaks should be added in parse tree
  -- notation
  [Anything Char] ->
  -- | the code block with added br tags for the line breaks in parse tree
  -- notation
  [Anything Char]
breakLines3 m s =
  rebreak (breakLinesHelper3 0 m (breakLinesHelper4 s))

-- | Adds quads in between double br line breaks, needed since double \\newline
-- is not allowed in latex
rebreak ::
  -- | The parse tree representation of the wiki source text with possible
  -- double line breaks.
  [Anything Char] ->
  -- | The parse tree representation of the wiki source text with quad symbols
  -- added between double line breaks where needed.
  [Anything Char]
rebreak
  ( (Environment Tag (TagAttr "br" a) l)
      : ((Environment Tag (TagAttr "br" a2) l2) : xs)
    ) =
    (Environment Tag (TagAttr "br" a) l)
      : Quad
      : (rebreak ((Environment Tag (TagAttr "br" a2) l2) : xs))
rebreak (x : xs) = x : (rebreak xs)
rebreak [] = []

-- | Replaces several parse tree item representations of white space characters
-- with the corresponding white space characters themselves in parse tree
-- notation.
renormalize ::
  -- | The parse tree representation of the wiki text possibly containing
  -- special symbols for white space characters.
  Anything Char ->
  -- | The parse tree representation of the wiki text with all special symbols
  -- for white space characters by the respective characters.
  Anything Char
renormalize (Environment Tag (TagAttr "br" _) []) = C '\n'
renormalize Quad = C ' '
renormalize Tab = C '\t'
renormalize x = x
