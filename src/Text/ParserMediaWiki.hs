module Text.ParserMediaWiki where

import MediaWikiParseTree
import MediaWikiParser

parseMediaWiki :: String -> [Anything Char]
parseMediaWiki text = parseit parsers text

parseMediaWikiGeneratedHTML :: String -> [Anything Char]
parseMediaWikiGeneratedHTML text = printPrepareTree (parseit minparsers text)
