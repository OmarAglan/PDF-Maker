{-# LANGUAGE DefaultSignatures, DeriveAnyClass, DeriveGeneric #-}
{-DHUN| A module providing all necessary types of a parse tree for the representation of source written in the MediaWiki markup language DHUN-}
module MediaWikiParseTree where
import Data.Map.Strict (Map)
import Data.Serialize
import GHC.Generics
import Control.DeepSeq
{-DHUN| Lists the different environment possible in the mediawiki markup language and example of an environment is an HTML tag with everything included between the its opening and closing tags. Her is is called Tag DHUN-}

data EnvType = Wikilink
             | IncludeOnly
             | ImageMap
             | Wikitable
             | Root
             | Wikiheading
             | Itemgroup
             | ItemLine
             | ItemEnv
             | Italic
             | Bold
             | TableCap
             | TableRowSep
             | TableColSep
             | TableHeadColSep
             | Tag
             | TableTag
             | Source
             | Reserved
             | Comment
             | Template
             | TemplateInside
             | TemplateInsideVerbatim
             | Chapter
             | Gallery
             | NoWiki
             | HDevLine
             | NoInclude
             | PageBreak
             | Math
             | Link
             | Link2
             | BigMath
             | Greek
             | P302
             | HtmlChar
             | Attribute
             | SpaceIndent
             | ForbiddenTag
             | Preformat
             | DhunUrl
             | Sub
             | Sup
             | Label
             | Parameter
             | NumHtml
             deriving (Show, Eq, Read, Serialize, Generic, NFData)

{-DHUN| A type representing a node in a the parse tree. Open and Close represent opening and closing bracket. They will be replace by environments (look at 'Environment' in this data structure) before the parse tree is processed further. The C represents a single character. S stands for a String. Tab is a special elements used like the tabulator character for line breaking purposes. Quad is similar to that. The Item... data construction are for processing itemization enumerations and so on and well be replace be environments before further processing DHUN-}

data Anything a = Environment EnvType StartData [Anything a]
                | Open Int EnvType StartData Int
                | Close Int EnvType
                | C a
                | Item Char
                | ItemStop Char
                | ItemStart Char
                | Quad
                | Tab
                deriving (Show, Eq, Read, Serialize, Generic, NFData)

{-DHUN| represents the result of a parser for the begin of an environment. A parser for an opening HTML tag is an example. TagAttr means tag with attributes. And is thus a string for the element and a map from string to string for it attributes. Str is a String. And Attr is key value pair and used for attribute in tables.  DHUN-}

data StartData = Str [Char]
               | TagAttr String (Map String String)
               | Attr (String, String)
               deriving (Show, Eq, Read, Serialize, Generic, NFData)
