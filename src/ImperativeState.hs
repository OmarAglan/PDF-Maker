{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | module defining the datatypes needed for the outer imperative flow control
-- of the program. All configuration information needed for single run of the
-- program is stored here.
module ImperativeState where

import Control.Monad.Except
import Control.Monad.State
import Data.List
import Data.Map.Strict
import Data.Serialize
import GHC.Generics
import MediaWikiParseTree
import UrlAnalyse

-- | A type to for errors that might be thrown during the imperative
-- calculation.
data MyError
  = -- | Error downloading the lemma (first parameter) from the URL (second parameter).
    DownloadError String String
  | -- | Generic error with error message.
    OtherError String
  | -- | Error parsing an URL.
    WikiUrlParseError String
  | -- | Error: A command line option that has to be given exactly one was given more than once of not at all.
    NotExcatlyOneError String
  | -- | Error: The command line option given in the only parameter did have a parameter that could not be parsed to an integer.
    NotIntegerError String
  | -- | Error: An option that is allowed to be present at most once was present two or more times
    NotAtMostOneError String
  | -- | Error: more than one of the options --internal --templates --mediawiki --html were given
    ToManyOptionsError
  | -- | Error: more than one of the options --zip --epub --odt were given
    ToManyOutputOptionsError
  | -- | Error: An other paper format than A4, A5, B5, letter, legal, executive was given.
    PaperError
  | -- | Error: More than one of the options --tableschromium or ---tableslatex were given
    ToManyTableOptionsError
  | -- | Error: The option was given as well as an other option.
    ToManyTestOptionsError

-- | A monad for dealing with errors
type MyErrorMonad = Either MyError

-- | printable error messages
instance Show MyError where
  show (DownloadError theLemma theUrl) =
    "Error downloading the lemma \""
      ++ theLemma
      ++ "\" form the url \""
      ++ theUrl
      ++ "\""
  show (WikiUrlParseError theUrl) =
    "Error: The supplied url " ++ theUrl ++ " could not be parsed"
  show PaperError =
    "Error: The option paper may only be one of A4, A5, B5, letter, legal, executive"
  show ToManyTableOptionsError =
    "Error: at most one of the options --tableschromium or ---tableslatex  may be given"
  show ToManyOptionsError =
    "Error: at most one of the options --internal --templates --mediawiki --html may be given"
  show ToManyOutputOptionsError =
    "Error: at most one of the options --zip --epub --odt may be given"
  show ToManyTestOptionsError =
    "Error: if the option --test is given no other option may be given"
  show (NotExcatlyOneError msg) =
    "Error: The option --"
      ++ msg
      ++ " has to be present exactly once in the command line"
  show (NotAtMostOneError msg) =
    "Error: The option --"
      ++ msg
      ++ " can only be present at most once in the command line"
  show (NotIntegerError msg) =
    "Error: The option --"
      ++ msg
      ++ " could not be parsed as an integer."
  show (OtherError msg) = msg

-- | A type to capture a contributor form the list of contributors needed for
-- license reasons.
data Contributor = Contributor
  { -- | The Username of the author.
    name :: String,
    -- | The number of edits on the given article done by the user.
    edits :: Integer,
    -- | The link to the users homepage on the wiki.
    href :: String
  }
  deriving (Eq, Ord, Show, Read, Serialize, Generic)

-- | The sum of two contributors is defined as the new contributor with the same
-- name and homepage and the edits summed up. Of course this makes only sense
-- when summing up edits for the same contributor, which is not checked but has
-- to be ensured by hand
myplus ::
  -- | The first addend.
  Contributor ->
  -- | The second addend.
  Contributor ->
  -- | The sum of the addends.
  Contributor
myplus x y = x {edits = (edits x) + (edits y)}

-- | Build a map of contributors summing up the edits per contributor. The map
-- takes the name of the contributor as key and the `Contributor` records given
-- above as value. The first parameter is a list of such maps the results is
-- also such a map representing the sum of the maps in the list
contribsum ::
  -- | List of maps from contributor names to `Contributor` records.
  [Map String Contributor] ->
  -- | The sum of the list given in the first parameter
  Map String Contributor
contribsum x =
  Data.List.foldl (unionWith myplus) Data.Map.Strict.empty x

-- | a default version of the record `ImperativeState`
imperativeStateZero ::
  -- | IO action returning the default version of `ImperativeState`
  IO ImperativeState
imperativeStateZero =
  do
    return
      ImperativeState
        { audict = [],
          fullUrl = fullWikiUrlZero,
          loadacu = Right [],
          vectorr = False,
          finishedLemmas = []
        }

-- | The datatype storing the state information for the part of the program
-- using imperative flow of control
data ImperativeState = ImperativeState
  { -- | The list of maps from contributor names (as keys) to `Contributor` records (as values). Each map in the list represents a list of contributors which a read from the list of contributors for a single wiki article on the web.
    audict ::
      [(Map String Contributor)],
    -- | The parsed version of the main URL mediawiki2latex is processing. See
    -- `UrlAnalyse.analyseFull` for details.
    fullUrl :: FullWikiUrl,
    -- | The loading accumulator for accumulating the input to be processed with
    -- mediawiki2latex. In book mode (Left case) with outputs PDF or LaTeX zip
    -- it stores the names of temporary directories where the data to be
    -- processed is stored. In any other case (Right case) it stores the parsed
    -- version of the information downloaded from the wiki.
    loadacu :: Either [FilePath] [Anything Char],
    -- | True if vector graphics should be converted to PDF as vector graphics
    -- and should not be converted to raster graphics. False otherwise.
    vectorr :: Bool,
    -- | The list of lemmas that already have been download an shall not be
    -- downloaded again to avoid infinite recursive downloads.
    finishedLemmas :: [String]
  }

-- | Datatype for the results of a compilation process. It is the process of
-- compiling MediaWiki or HTML source code to LaTeX source code. So this data
-- type contains the resulting LaTeX code together with auxiliary information
data CompileResult = CompileResult
  { -- | Contains the strings enclosed in double square brackets in the wiki used for image inclusion.
    images :: [String],
    -- | Contains the body of the latex document compiled form the wiki
    -- documents
    body :: String,
    -- | Contains a list of lists of  bodies of latex document containing the
    -- latex source for a single column.  Those can be compiled with the proper
    -- headers and footers on arbitrary huge paper to determine the maximum
    -- reasonable width for a each column in each table of the document which is
    -- need for automatic calculation of column widths for the document.
    tablelist :: [[String]],
    -- | Contains the image numbers of images included in the wiki source inside
    -- of galleries. These got a smaller dimension in cm in the final PDF
    -- document and can thus be dithered to a small width in pixels.
    galleryNumbers :: [Integer],
    -- | Contains the title of the document if a template defining the title was
    -- part of the parse wiki source
    title :: String,
    -- | HTML intermediate code if output mode EPUB or ODT was selected.
    html :: String,
    -- | The HTML code for the tables to be compiled with chromium if table
    -- rendering with chromium is selected
    theHtmlTabs :: [String],
    -- |  The OpenStreetMap maps included in the wiki article as HTML code.
    -- Works only when run from inside wikimedias server farm, so only on the
    -- web server provides the wikimedia foundation.
    theHtmlMaps :: [String],
    -- | The LaTeX Codes for the chess board diagrams in the Wiki currently
    -- processes. Currently only used for the German Wikibook on chess.
    theChessBoards :: [String]
  }
  deriving (Show, Serialize, Generic, Read)

-- | The configuration for a run of the LaTeX typesetting system.
data LatexConfig = LatexConfig
  { -- | Information about the authors of the images
    figures :: [ImageCredits],
    -- | The information for the title page of the PDF to be generated
    ltitle :: String,
    -- | the full configuration for single run of mediawiki2latex (e.g parsed
    -- from the command line interface)
    fullConfig :: FullConfig,
    -- | the content of the main.tex LaTeX file to be processed by the LaTeX
    -- typesetting system
    content :: String,
    -- | the hostname of the host the wiki information is downloaded from
    lhostname :: String,
    -- | The result of the source code HTML or Wiki to LaTeX source compilation.
    theResult :: CompileResult,
    -- | True if the intended LaTeX run is only a table columns precompilation
    -- run and not a run creating a PDF document for the user. If True the
    -- unnecessary parts of compilation process are left out.
    onlyTables :: Bool,
    -- The language code for the document to be processed wrapped into a Just
    -- value of the Maybe monad if it could be determined, otherwise the Nothing
    -- value of the Maybe monad.
    lang :: Maybe String,
    -- The temporary directory where to store the information and run the LaTeX
    -- typesetting system in.
    theTempDir :: String,
    -- The formulas for use in HTML intermediate output (e.g. for EPUB or ODT
    -- generation) as a list of pairs. The first element of each pair is the
    -- name code for the PNG file the formula was compiled to. The second
    -- element of each pair is the size of the PNG file in x direction (so its
    -- width).
    formulas :: [(String, Int)],
    -- The tailing part of the HTML intermediate code containing the list of
    -- contributors as well as the list of figures (including the respective
    -- authors) as well as the closing tags for the root and body tags of the
    -- HTML documents. This is only needed when HTML intermediate code is use to
    -- create ODT or EPUB documents.
    figHTML :: String,
    -- | The HTML code for the tables to be compiled with chromium if table
    -- rendering with chromium is selected
    theHtmlTables :: [String],
    -- Works only when run from inside wikimedias server farm, so only on the
    -- web server provides the wikimedia foundation.
    theHtmlMapes :: [String],
    -- | The LaTeX Codes for the chess board diagrams in the Wiki currently
    -- processes. Currently only used for the German wikibook on chess.
    theChessBoardes :: [String]
  }
  deriving (Show, Serialize, Read, Generic)

-- | The monad for the part of mediawiki2latex written in imperative style.
type ImperativeMonad = ExceptT MyError (StateT ImperativeState IO)

-- | Selection for the book mode. That means processing of multiple articles
-- (given as list wiki links) to a single document
data BookMode
  = -- | Book mode active
    Yes
  | -- | Book mode not active
    No
  deriving (Show, Read, Eq, Serialize, Generic)

-- | The mode of processing mediawiki2latex is running. See also `BookMode`.
data RunMode
  = -- | Process HTML generated by MediaWiki downloaded from the MediaWiki server (default mode)
    HTML BookMode
  | -- | process MediaWiki source code downloaded from the MediaWiki server with templates expanded by the MediaWiki server.
    ExpandedTemplates BookMode
  | -- | Process MediaWiki source code downloaded from the MediaWiki server. Load the plain wiki source including possible calls to templates, don't let the MediaWiki server expand the templates. Rather use the own template processing mechanisms included in mediawiki2latex.
    StandardTemplates BookMode
  | -- | Same as `StandardTemplates` but use a used defined template mapping. The file name of the mapping file is given in the second parameter
    UserTemplateFile BookMode String
  deriving (Show, Read, Eq, Serialize, Generic)

-- | The output format to be generated.
data OutputType
  = -- | Output a PDF file.
    PlainPDF
  | -- | Output a Zip archive of the LaTeX code that can be compiled to a PDF file.
    ZipArchive
  | -- | Output an EPUB file. Here a HTML file is generated at first an the processed into an EPUB file by calibre.
    EPubFile
  | -- | Output a ODT file. Here a HTML file is generated at first an the processed into an ODT file by libreoffice.
    OdtFile
  deriving (Show, Read, Eq, Serialize, Generic)

-- | For recursive call of the mediawiki2latex command line application. In
-- order to save memory  (RAM) when processing to LaTeX ZIP or PDF in bookmode
-- mediawiki2latex run different steps of the compilation processes in sub
-- processes of itself which free their memory when terminated.
data ConvertState
  = -- | Add labels to the parse tree and return the new parse tree as well as the new renderer state, including the labels for internal references in the PDF or LaTeX document. The String parameter is the name of the directory where the processing shall take place.
    NewTree String
  | -- | Convert parse tree to LaTeX representation. The first parameter is the name of the directory where to read the initial renderer state form. The second parameter is the directory where the processing shall take place and where the output shall be written to.
    TreeToLaTeX String String
  | -- | Load an article form the server, parse the result and store the resulting parse tree. Also download an store the contributor information the contributors of the article. The first parameter is the name of the directory where processing shall take place.
    NewLoad String
  deriving (Show, Read, Eq, Serialize, Generic)

-- | The full configuration for a single run of mediawiki2latex
data FullConfig = FullConfig
  { -- | If a user supplied headers directory for the LaTeX compilation process is provided in the command line, its name is given as a Just value of the Maybe monad. Otherwise the Nothing value of the Maybe monad.
    headers :: Maybe String,
    -- | The maximum resolution in (dots per inch) the images shall be dithered
    -- to, in order to make the PDF not too large in terms of diskspace.
    resolution :: Integer,
    -- | The output filename where the resulting file shall be written to.
    outputFilename :: String,
    -- | The input URL to the article on the wiki server where to download the
    -- content from.
    inputUrl :: String,
    -- | The run mode of mediawiki2latex. See `RunMode`.
    runMode :: RunMode,
    -- | The paper format to use for the output file.
    paper :: String,
    -- | If true SVG vector graphics are included in the output file as vector
    -- graphics. Otherwise they are converted to raster graphics.
    vector :: Bool,
    -- | If the output tree directory containing the LaTeX of HTML intermediate
    -- code shall be copied to a given directory before the final compilation
    -- step takes place a Just value of the Maybe monad containing the name of
    -- the directory where the data shall be copied. Otherwise the Nothing value
    -- of the Maybe monad.
    copy :: Maybe String,
    -- | The working directory where mediawiki2latex is running in.
    mainPath :: String,
    -- | Open the mediawiki2latex web server on the port given as first
    -- parameter
    server :: Maybe Int,
    -- | The output format to be generated. See `OutputType`
    outputType :: OutputType,
    -- | Used only for recursive calls of mediawiki2latex to itself to save
    -- memory (see also `ConvertState`). The only parameter is the directory
    -- where the processing shall happen wrapped in a Just value of the Maybe
    -- monad. Reads the given input from the wiki notation into a parse tree
    -- structure and stores the result. On a non recursive (normal) call of
    -- mediawiki2latex a Nothing value of the Maybe monad is used and nothing is
    -- done here.
    compile :: Maybe String,
    -- |  Used only for recursive calls of mediawiki2latex to itself to save
    -- memory (see also `ConvertState`). On a recursive call the requested
    -- `ConvertState` wrapped into a Maybe monad is used here. Otherwise the
    -- Nothing value of the Maybe monad is used and nothing is done here.
    convert :: Maybe ConvertState,
    -- | True if only children of the current URL shall be used in the
    -- processing. False if any URL shall be processed
    noparent :: Bool,
    -- | Run LaTeX compiler in a sub process, this is needed for table column
    -- precompilation, since it can happen in parallel processes. The only
    -- parameter is the name of the directory where the processing shall take
    -- place. This is only used in recursive call of mediawiki2latex on itself.
    ltxproc :: Maybe String,
    -- | Read the HTML image description page and determine the author and
    -- license of the image if possible. Also determine the link to the version
    -- history of the image. Write the resulting information to disk. The first
    -- element of the parameter tuple is the filename where to read the HTML of
    -- the image description page from. The second one is the host name of the
    -- server the data has been received from. This only used in recursive calls
    -- of the mediawiki2latex command line application to itself, in which case
    -- a Just value of the Maybe monad is used. In normal processing the Nothing
    -- value of the Maybe monad is used.
    imgctrburl :: Maybe (String, String),
    -- | Get the image contributors from a version history page of the MediaWiki
    -- server stored on the local disc. Store the result to disk again. This is
    -- parsing the HTML of the version history page. In order so save memory
    -- (RAM), this is done in a separate sub process freeing its memory when
    -- terminated. This only used in recursive calls of the mediawiki2latex
    -- command line application to itself, in which case a Just value of the
    -- Maybe monad is used. In normal processing the Nothing value of the Maybe
    -- monad is used.
    ctrb :: Maybe String,
    -- | Typeset all table with LaTeX if True. Typeset complex tables with
    -- chromium and simple tables with LaTeX if false.
    latexTables :: Bool,
    -- | True if internal test shall be run an no further processing shall
    -- occur.
    testMode :: Bool
  }
  deriving (Show, Read, Serialize, Generic)

-- | The standard instance of the `FullConfig` data type.
fullconfigbase :: FullConfig
fullconfigbase =
  FullConfig
    { headers = Nothing,
      resolution = 0,
      outputFilename = "",
      inputUrl = "",
      runMode = HTML No,
      paper = "A4",
      vector = False,
      copy = Nothing,
      mainPath = "",
      server = Nothing,
      outputType = PlainPDF,
      compile = Nothing,
      convert = Nothing,
      noparent = False,
      imgctrburl = Nothing,
      ctrb = Nothing,
      latexTables = False,
      ltxproc = Nothing,
      testMode = False
    }
