-- | Automated self test of mediawiki2latex.
module Test where

import Data.ByteString
import Data.ByteString.UTF8
import Data.List
import Data.List.Split
import LatexRenderer
import MediaWikiParser
import Static
import Test.HUnit
import Tools

-- | Compile wiki code to LaTeX code for automated test of the software.
compile ::
  -- | The wiki code to be compiled to LaTeX .
  String ->
  -- | The result of the compilation of the wiki code give in the first
  -- parameter to LaTeX.
  String
compile s =
  fst
    ( treeToLaTeX3
        (snd newtree)
        initialState
          { templateMap =
              getUserTemplateMap
                (read userTemplates :: [[String]]),
            urld = analyseNetloc "de.wikibooks.org/wiki/Benutzer:Dirk_H%C3%BCnniger/play",
            urls = mUrlState . fst $ newtree
          }
    )
  where
    newtree = makeLables (parseit parsers s) initialUrlState

-- | remove font switching commands from a LaTeX file.
killfont ::
  -- | The input LaTeX code from which the font switching command shall be
  -- removed.
  String ->
  -- | The output LaTeX code which is equal the the input LaTeX code with the
  -- font switching commands. removed
  String
killfont s = replace2 (replace2 (replace2 (replace2 (replace2 (replace2 (replace2 (replace2 s "\\allowbreak{}\\setmainfont{cmunrm.ttf}[RawFeature={fallback=myfallback},Path=/usr/share/fonts/truetype/cmu/,UprightFont=cmunrm,BoldFont=cmunbx,ItalicFont=cmunti,BoldItalicFont=cmunbi]\\setmonofont{cmunrm.ttf}[RawFeature={fallback=myfallback},Path=/usr/share/fonts/truetype/cmu/,UprightFont=cmuntt,BoldFont=cmuntb,ItalicFont=cmunit,BoldItalicFont=cmuntx]" "") "\\allowbreak{}\\setmainfont{cmuntt.ttf}[RawFeature={fallback=myfallback},Path=/usr/share/fonts/truetype/cmu/,UprightFont=cmunrm,BoldFont=cmunbx,ItalicFont=cmunti,BoldItalicFont=cmunbi]\\setmonofont{cmuntt.ttf}[RawFeature={fallback=myfallback},Path=/usr/share/fonts/truetype/cmu/,UprightFont=cmuntt,BoldFont=cmuntb,ItalicFont=cmunit,BoldItalicFont=cmuntx]" "") "\\allowbreak{}\\setmainfont{cmunti.ttf}[RawFeature={fallback=myfallback},Path=/usr/share/fonts/truetype/cmu/,UprightFont=cmunrm,BoldFont=cmunbx,ItalicFont=cmunti,BoldItalicFont=cmunbi]\\setmonofont{cmunti.ttf}[RawFeature={fallback=myfallback},Path=/usr/share/fonts/truetype/cmu/,UprightFont=cmuntt,BoldFont=cmuntb,ItalicFont=cmunit,BoldItalicFont=cmuntx]" "") "\\allowbreak{}\\setmainfont{cmunbx.ttf}[RawFeature={fallback=myfallback},Path=/usr/share/fonts/truetype/cmu/,UprightFont=cmunrm,BoldFont=cmunbx,ItalicFont=cmunti,BoldItalicFont=cmunbi]\\setmonofont{cmunbx.ttf}[RawFeature={fallback=myfallback},Path=/usr/share/fonts/truetype/cmu/,UprightFont=cmuntt,BoldFont=cmuntb,ItalicFont=cmunit,BoldItalicFont=cmuntx]" "") "\\allowbreak{}\\setmainfont{cmunbi.ttf}[RawFeature={fallback=myfallback},Path=/usr/share/fonts/truetype/cmu/,UprightFont=cmunrm,BoldFont=cmunbx,ItalicFont=cmunti,BoldItalicFont=cmunbi]\\setmonofont{cmunbi.ttf}[RawFeature={fallback=myfallback},Path=/usr/share/fonts/truetype/cmu/,UprightFont=cmuntt,BoldFont=cmuntb,ItalicFont=cmunit,BoldItalicFont=cmuntx]" "") "\\allowbreak{}\\setmainfont{cmuntx.ttf}[RawFeature={fallback=myfallback},Path=/usr/share/fonts/truetype/cmu/,UprightFont=cmunrm,BoldFont=cmunbx,ItalicFont=cmunti,BoldItalicFont=cmunbi]\\setmonofont{cmuntx.ttf}[RawFeature={fallback=myfallback},Path=/usr/share/fonts/truetype/cmu/,UprightFont=cmuntt,BoldFont=cmuntb,ItalicFont=cmunit,BoldItalicFont=cmuntx]" "") "\\allowbreak{}\\setmainfont{cmunit.ttf}[RawFeature={fallback=myfallback},Path=/usr/share/fonts/truetype/cmu/,UprightFont=cmunrm,BoldFont=cmunbx,ItalicFont=cmunti,BoldItalicFont=cmunbi]\\setmonofont{cmunit.ttf}[RawFeature={fallback=myfallback},Path=/usr/share/fonts/truetype/cmu/,UprightFont=cmuntt,BoldFont=cmuntb,ItalicFont=cmunit,BoldItalicFont=cmuntx]" "") "\\allowbreak{}\\setmainfont{cmuntb.ttf}[RawFeature={fallback=myfallback},Path=/usr/share/fonts/truetype/cmu/,UprightFont=cmunrm,BoldFont=cmunbx,ItalicFont=cmunti,BoldItalicFont=cmunbi]\\setmonofont{cmuntb.ttf}[RawFeature={fallback=myfallback},Path=/usr/share/fonts/truetype/cmu/,UprightFont=cmuntt,BoldFont=cmuntb,ItalicFont=cmunit,BoldItalicFont=cmuntx]" ""

-- | Extract the files from the "test" directory structure embedded in the
-- executable for a given name of a test
readFiles ::
  -- | The embedded directory form the "test" directory.
  [(String, ByteString)] ->
  -- | The name of the test to be extracted.
  String ->
  -- | The files for the test case with the given name as a tuple containing the
  -- MediaWiki code as first item and the LaTeX code it should compiled to as
  -- second parameter. The tuple is wrapped into a Just value of the Maybe monad
  -- if the files were found. A Nothing value of the Maybe monad is returned
  -- otherwise.
  Maybe (String, String)
readFiles files f = do
  w <- lookup (f ++ ".wiki") files
  t <- lookup (f ++ ".tex") files
  return (toString w, toString t)

-- | Prepare a compilation self test with the name of the test given in the
-- second parameter.
filetest ::
  -- | The embedded directory structure of the "test" directory.
  [(String, ByteString)] ->
  -- | The name of the test to run
  String ->
  -- | The HUnit test case for the test with the name given in the second
  -- parameter.
  Test
filetest files f =
  TestLabel
    f
    ( TestCase
        ( case (readFiles files f) of
            Just (wiki, tex) -> assertEqual f (killfont (strip "\n" tex)) (killfont (strip "\n" (compile wiki)))
            Nothing -> assertFailure "Test case not found"
        )
    )

-- | Return test name of a test from an entry the embedded directory structure
-- of the test directory. The name of the test is returned as list containing
-- the name of the test as a single item if it was found. Otherwise the empty
-- list is returned.
wikipred ::
  -- |  the entry of the embedded directory structure of test directory.
  (String, ByteString) ->
  -- | A list containing the name of test as single entry if a name of the test
  -- was found. Otherwise the empty list.
  [String]
wikipred s = case Data.List.reverse (splitOn "." (fst s)) of
  (("wiki") : xs) -> [Data.List.intercalate "." (Data.List.reverse xs)]
  _ -> []

-- | Run all test of mediawiki2latex
runTest ::
  -- | The IO action to run all test of mediawiki2latex
  IO ()
runTest = do
  _ <- runTestTT (TestList ((Data.List.map (filetest testFiles) (testFiles >>= wikipred))))
  return ()
