module Text.ParserMediaWiki where
import MediaWikiParser
import MediaWikiParseTree

parseMediaWiki::String->[Anything Char]
parseMediaWiki text = parseit parsers text

parseMediaWikiGeneratedHTML::String->[Anything Char]
parseMediaWikiGeneratedHTML text = printPrepareTree (parseit minparsers text)
