{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | A module providing all necessary types of a parse tree for the
-- representation of source written in the MediaWiki markup language
module MediaWikiParseTree where

import Control.DeepSeq
import Data.Map.Strict (Map)
import Data.Serialize
import GHC.Generics

-- | Lists the different environment possible in the MediaWiki markup language
-- and example of an environment is an HTML tag with everything included between
-- the its opening and closing tags. Here this is called Tag.
data EnvType
  = -- | A wiki link, so a inside a wiki or to an article on another MediaWiki instance. In wiki notation this is denoted by the link target in double square brackets.
    Wikilink
  | -- | An "include only" tag. So contents inside this tag shall only be visible when the page get included by an other wiki page. Within mediawiki2latex the content is just passed through, just like only the contents between the opening an closing tags but not the tags themselves would be there.
    IncludeOnly
  | -- | An HTML image map, so an image with some clickable areas that link to other HTML pages. Within mediawiki2latex only the image itself is processed.
    ImageMap
  | -- | A table in the wiki. In wiki notation this is denoted by an opening curly bracket followed by a vertical bar character as starting marker and a vertical bar character followed by a closing curly bracket as end marker.
    Wikitable
  | -- | The root element of a Wiki page. It is automatically added to any wiki page and surround the wiki page as a whole.
    Root
  | -- | A heading in the wiki text. In wiki notation. A heading in wiki notation is denoted by two or more equal signs surrounding the heading text.
    Wikiheading
  | -- | A group of items. This can be an numbered list or a bulleted list or the like. See documentation on `MediaWikiParser.evaluateItemgroup` for more details
    Itemgroup
  | -- | a line inside an item group. See documentation on `MediaWikiParser.evaluateItemgroup` for more details. Its a line starting with a combination of the characters *#:; . So this is close to the wiki notation of an itemization
    ItemLine
  | -- | An itemized environment. So a bulleted or numbered list of the like. This is close to the LaTeX representation of an itemization. It carries one of the characters *#:; which are mapped to the respective itemization commands in LaTeX.
    ItemEnv
  | -- | A text that shall be printed in italics. This is denoted by double apostrophes surrounding the italic text.
    Italic
  | -- | A text that shall be printed in bold font. This is denoted by triple apostrophes surrounding the bold text.
    Bold
  | -- | A table caption line.
    TableCap
  | -- | A table row separator.
    TableRowSep
  | -- | A table column separator.
    TableColSep
  | -- | A table headings columns separator.
    TableHeadColSep
  | -- | A HTML tag.
    Tag
  | -- | A HTML tag for a table.
    TableTag
  | -- | A MediaWiki source tag. Used for displaying source code on the wiki.
    Source
  | -- | A keyword of the MediaWiki syntax.
    Reserved
  | -- | A HTML comment.
    Comment
  | -- | A template in MediaWiki notation.
    Template
  | -- | The contents inside a template.
    TemplateInside
  | -- | The contents inside a template whose inner structure shall not be parsed. Needed for templates containing source code, which shall be passed to latex verbatim.
    TemplateInsideVerbatim
  | -- | A gallery. So a group of images that belong together.
    Gallery
  | -- | content inside a nowiki tag in wiki code.
    NoWiki
  | -- | Parser for the length unit mu (math unit) inside a math formula. Only use when the contents of math tags is parsed for the second time in the course of making the formula ready for use in a LaTeX document.
    MuInsideMath
  | -- | A horizontal dividing line. This replace by nothing when generating the LaTeX code.
    HDevLine
  | -- | A noinclude tag in the wiki source code. It is replaced by nothing when generating the LaTeX code. So content between the opening and closing noinclude tags is removed.
    NoInclude
  | -- | A page break.
    PageBreak
  | -- | A formula written in between opening closing math tags in the wiki source code. This is usually rendered between dollar signs in the LaTeX document. So it becomes a normal formula. If the formula is indented or centered it will be converted to `BigMath` by `WikiHelper.shallowEnlargeMath` function.
    Math
  | -- | A parser for a link in the wiki source code. Surrounding square brackets are required. May also be nested.
    Link
  | -- | A parser for a link in the wiki source code. Surrounding square brackets are not possible. Must not be nested.
    Link2
  | -- | A formula written in between opening and closing math tag in the wiki source code, which needs to be surrounded by either center tags or indented by a colon in the beginning of the line in the wiki source code. This formula is rendered in a LaTeX document by surrounding it with a equation environment, so it looks centered and bigger in the resulting PDF file.
    BigMath
  | -- | A Greek letter or similar HTML entity, that can be converted to LaTeX by removing the starting ampersand and tailing semicolon and add a backslash in front.
    Greek
  | -- | A character that shall have a hat (circumflex) above itself. In HTML (and wiki code) this is denoted by putting a hash x 302 entity behind the character which shall get the hat above itself.
    P302
  | -- | A HTML character entity starting with an ampersand character and ending with a semicolon character. It is mapped to its LaTeX representation by the `MagicStrings.htmlchars` mapping.
    HtmlChar
  | -- | An attribute of a table header column separator or a table column separator or a table heading. Similar to and attribute of an HTML tag. Use only for tables processing.
    Attribute
  | -- | A preformatted section that is denoted by lines starting with the space character in wiki notation. Some markup commands are still usable in such a section. This is in contrast to `Preformat`
    SpaceIndent
  | -- | A preformatted section enclosed by pre HTML tags. No markup commands are possible inside this section.
    Preformat
  | -- | Denotes the start of a new lemma inside the wiki to download. This is needed to for cross references inside to different lemma within a single LaTeX document. When downloading the lemmas, a line starting with dhunparserurl is inserted at the beginning of each lemma.
    DhunUrl
  | -- | Text that is displayed subscript.
    Sub
  | -- | Text that is displayed superscript.
    Sup
  | -- | A label for a reference to a section inside the wiki code of the current lemma. See `WikiHelper.makeLables` for details.
    Label
  | -- | A parameter in the definition of a template in the wiki code. So something that is enclosed in triple curly brackets.
    Parameter
  | -- | A HTML character entity written in numeric encoding. So an ampersand followed by a hash followed by the numeric code in hex or decimal number followed by a semicolon.
    NumHtml
  deriving (Show, Eq, Serialize, Generic, NFData, Read)

-- | A type representing a node in a the parse tree. Open and Close represent
-- opening and closing bracket. They will be replace by environments (look at
-- 'Environment' in this data structure) before the parse tree is processed
-- further. The C represents a single character. Tab is a special elements used
-- like the tabulator character for line breaking purposes. Quad is similar to
-- that. The Item... data construction are for processing itemization
-- enumerations and so on and well be replace be environments before further
-- processing
data Anything a
  = -- | An environment that was successfully parsed. The first parameter is the type of the environment (see `EnvType` for details). The second parameter is the data that was parsed at during the parse of the beginning of the environment (see `StartData` for details). The third parameter is anything that is contained inside the parsed environment.
    Environment EnvType StartData [Anything a]
  | -- | An opening bracket of an environment. Used only internally during the parse process. The first parameter is the depth of the stack of environments at the time this opening bracket was parsed. So it shows how deeply the environment is nested in outer environments. Note this has nothing to do with the nestingdepth parameter in the module `MediaWikiParser`, which is just a unique identifier for each environment ever parsed. The second parameter is the type of the environment (see `EnvType` for details). The third parameter is the data parsed during the parsing of the beginning of the environment (see `StartData` for details). The forth parameter is the number of the parser that is currently matching at this specific opening bracket. So there is a list of parsers use for the whole parsing process, where each parser is responsible for a certain environment. This list gets enumerated before the actual parsing starts and the third parameter here is just the number in this list.
    Open Int EnvType StartData Int
  | -- | The closing part of a parsed environment. The first parameter is the depth of the stack of environments. So it show how deep the environment just about to close is nested in sub environments.
    Close Int EnvType
  | -- | A character.
    C a
  | -- | An item separator in an itemization. This first parameter defines the type of the itemization and is written in wiki notation (for example * stands for a bulleted list)
    Item Char
  | -- | The beginning of an itemization only used internally during the parsing of item groups. (see `MediaWikiParser.evaluateItemgroup` for details)
    ItemStop Char
  | -- | The end of an itemization only used internally during the parsing of item groups. (see `MediaWikiParser.evaluateItemgroup` for details)
    ItemStart Char
  | -- | A space character only used during line breaking of preformatted text sections (like for example source codes)
    Quad
  | -- | A tab character only used during line breaking of preformatted text sections (like for example source codes) four times wide as the `Quad` symbol.
    Tab
  deriving (Show, Eq, Serialize, Generic, NFData, Read)

-- | represents the result of a parser for the begin of an environment. A parser
-- for an opening HTML tag is an example. TagAttr means tag with attributes. And
-- is thus a string for the element and a map from string to string for it
-- attributes. Str is a String. And Attr is key value pair and used for
-- attribute in tables.
data StartData
  = -- | A string
    Str [Char]
  | -- | A HTML tag. The first parameter is a string representing the tag. The second parameter is a map that maps the tags attributes as keys to their respective values.
    TagAttr String (Map String String)
  | -- | An tupel. The first element is and attribute. The second one is its value.
    Attr (String, String)
  deriving (Show, Eq, Serialize, Generic, NFData, Read)

-- | The information about the author(s) an the license(s) about an image on the
-- wiki.
data ImageCredits = ImageCredits
  { -- | the author of the image, represented as parse tree. This calculated by looking at the author entry of the information template in the HTML image description page on the wiki. If no author could be determined this way the HTML history page of the HTML image description page is used to get the authors. See `SimpleContributors.getAuthor` for details.
    theAuthor :: [Anything Char],
    -- | the license of the image, represented as parse tree. This is just a
    -- String abbreviation of the license as a list of characters in Anything
    -- Char. See `Licenses.licenses` for a list of licenses processable by
    -- mediawiki2latex. See `SimpleContributors.getLicense` for how the license
    -- is calculated.
    theLicense :: [Anything Char],
    -- | The URL on the wiki where the image description page of the image is
    -- located. This used to implement that you can click on the image number in
    -- the list of figures in the resulting PDF file in order to open the image
    -- description page of the image in your browser.
    theDescUrl :: String,
    -- | The file name of the image on the wiki.
    wikiFilename :: String,
    -- | the number of the image. That is the position at which it occurs in the
    -- input as well as in the output in the list of figures.
    imageNumber :: Int,
    -- | the alternative authors of the image file. This is derived from the
    -- full history list of the version history on the wiki. This is also use in
    -- the field `theAuthor` of this record if no author could be determined
    -- from the information template in the HTML image description page on the
    -- wiki.
    theAltAuthors :: String
  }
  deriving (Show, Eq, Serialize, Generic, NFData, Read)
