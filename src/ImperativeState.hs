{-# LANGUAGE DefaultSignatures, DeriveAnyClass, DeriveGeneric, StandaloneDeriving #-}
{-DHUN| module defining the datatypes needed for the outer imperative flow control of the program. All configurtion information needed for single run of the program is stored here. DHUN-}
module ImperativeState where
import Data.Map.Strict
import Control.Monad.State
import Control.Monad.Except
import MediaWikiParseTree
import UrlAnalyse
import Control.Concurrent.MVar
import Data.List
{-DHUN| A type to for errors that might be thrown during the imperative calculation DHUN-}
import Data.Serialize
import GHC.Generics
import qualified Data.Map as Map
data MyError = DownloadError String String
             | OtherError String
             | WikiUrlParseError String
             | NotImplementedError
             | NotExcatlyOneError String
             | NotIntegerError String
             | NotIntegerPairError String
             | NotAtMostOneError String
             | ToManyOptionsError
             | ToManyOutputOptionsError
             | PaperError
             | ToManyTableOptionsError




{-DHUN| A monad for dealing with errors DHUN-}

type MyErrorMonad = Either MyError

{-DHUN| printable error messages DHUN-}

instance Show MyError where
        show (DownloadError theLemma theUrl)
          = "Error downloading the lemma \"" ++
              theLemma ++ "\" form the url \"" ++ theUrl ++ "\""
        show (WikiUrlParseError theUrl)
          = "Error: The supplied url " ++ theUrl ++ " could not be parsed"
        show NotImplementedError
          = "Error: The requested feature is not implemented yet"
        show (NotIntegerPairError msg)
          = "Error: The option --" ++
              msg ++ "could not be parsed to a pair of integers (like -f 23:42)"
        show PaperError
          = "Error: The option paper may only be one of A4,A5,B5,letter,legal,executive"
        show ToManyTableOptionsError
          = "Error: at most one of the options --tableschromium or ---tableslatex  may be given"
        show ToManyOptionsError
          = "Error: at most one of the options --internal --templates --mediawiki --html may be given"
        show ToManyOutputOptionsError
          = "Error: at most one of the options --zip --epub --odt may be given"
        show (NotExcatlyOneError msg)
          = "Error: The option --" ++
              msg ++ " has to be present exactly once in the command line"
        show (NotAtMostOneError msg)
          = "Error: The option --" ++
              msg ++ " can only be present at most once in the command line"
        show (NotIntegerError msg)
          = "Error: The option --" ++
              msg ++ " could not be parsed as an integer."
        show (OtherError msg) = msg


{-DHUN| A type to capture a contributor form the list of contributors needed for license reasons. The element name is username of the author, the element edits is the number of edits done by the user, href is a link to the users homepage on the wiki DHUN-}

data Contributor = Contributor{name :: String, edits :: Integer,
                               href :: String}
                 deriving (Eq, Ord, Show, Read, Serialize, Generic)

{-DHUN| The sum of two contribors is defined as the new contribor with the same name and homepage and the edits summed up. Of course this makes only sense when summing up edits for the same contributor, which is not checked but has to be ensured by hand DHUN-}

myplus :: Contributor -> Contributor -> Contributor
myplus x y = x{edits = (edits x) + (edits y)}

{-DHUN| Build a map of contributors summing up the edits per contributor. The map takes the name of the contributor as key and the contributor records given above as value. The first parameter is a list of such maps the results is also such a map representing the sum of the maps in the list DHUN-}

contribsum :: [Map String Contributor] -> Map String Contributor
contribsum x
  = Data.List.foldl (unionWith myplus) Data.Map.Strict.empty x

{-DHUN| a defaults version of the record Imperative State DHUN-}


imperativeStateZero :: IO ImperativeState
imperativeStateZero
  = do v <- newMVar (0 :: Int)
       return
         ImperativeState{audict = [], fullUrl = fullWikiUrlZero,
                         tmpPath = "", counter = v, loadacu = Right [], vectorr=False, noparentis=False,latexTable = False,finishedLemmas=[]}

data ImperativeState = ImperativeState{audict ::
                                       [(Map String Contributor)],
                                       fullUrl :: FullWikiUrl, tmpPath :: String,
                                       counter :: MVar Int, loadacu :: Either [FilePath] [Anything Char], vectorr::Bool, noparentis::Bool,latexTable :: Bool, finishedLemmas::[String]}

{-DHUN| datatype for the results of a compulation process. The field images contains the strings enlclose in double square brackes in the wiki used for image inclusion. The field body contains the body of the latex document compiled form the wiki documents. The field tablelist contains a list of lists of  bodies of latex document containing the latex source for a single column. Those can be compiled with the propper headers and footers on arbitrary huge paper to determine the maximum reasonable width for a each column in each table of the document which is need for automatic calculation of column widths for the document. The field gallery numbers contain the image numbers of images include in the wiki source inside of galleries. These got a smaller dimension in cm in the final pdf document and can thus be dithers to a small width in pixels. The field title contains the title of the document if a template defining the title was part of the parse wiki source |DHUN-}

data CompileResult = CompileResult{images :: [String],
                                   body :: String, tablelist :: [[String]],
                                   galleryNumbers :: [Integer], title :: String, html :: String, theHtmlTabs::[String], theHtmlMaps::[String], theHtmlColors::Map.Map String String}
                deriving (Show, Serialize, Generic, Read)


data LatexConfig = LatexConfig{figures :: [ImageCredits],
                               ltitle :: String, fullConfig :: FullConfig, content :: String,
                               lhostname :: String, theResult :: CompileResult, onlyTables :: Bool,
                               lang :: Maybe String, theTempDir :: String,
                               formulas :: [(String, Int)], figHTML :: String, theHtmlTables :: [String], theHtmlMapes::[String],theHtmlColours::Map String String}
                deriving (Show, Serialize , Read, Generic)

type ImperativeMonad = ExceptT MyError (StateT ImperativeState IO)

data BookMode = Yes | No
             deriving (Show, Read, Eq, Serialize, Generic)

data RunMode = HTML BookMode
             | ExpandedTemplates BookMode 
             | StandardTemplates BookMode
             | UserTemplateFile BookMode String
             deriving (Show, Read, Eq, Serialize, Generic)

data SourceMode = Included
                | Excluded
                deriving (Show, Read, Serialize, Generic)

data OutputType = PlainPDF
                | ZipArchive
                | EPubFile
                | OdtFile
                deriving (Show, Read, Eq, Serialize, Generic)

data ConvertState = NewTree String | TreeToLaTeX String String | NewLoad String
                deriving (Show, Read, Eq,  Serialize, Generic)

data FullConfig = FullConfig{headers :: Maybe String,
                             resolution :: Integer, outputFilename :: String,
                             inputUrl :: String, runMode :: RunMode, paper :: String,
                             vector :: Bool, copy :: Maybe String, mainPath :: String,
                             server :: Maybe Int, outputType :: OutputType,
                             selfTest :: Maybe (Integer, Integer), compile :: Maybe String,
                             convert:: Maybe (ConvertState), noparent::Bool, ltxproc :: Maybe String, imgctrburl :: Maybe (String,String),ctrb :: Maybe String, latexTables :: Bool}
                deriving (Show, Read, Serialize, Generic)

fullconfigbase :: FullConfig
fullconfigbase
  = FullConfig{headers = Nothing, resolution = 0,
               outputFilename = "", inputUrl = "", runMode = HTML No, paper = "A4",
               vector = False, copy = Nothing, mainPath = "", server = Nothing,
               outputType = PlainPDF, selfTest = Nothing, compile = Nothing,
               convert =Nothing, noparent=False, imgctrburl=Nothing,ctrb=Nothing, latexTables = False, ltxproc =Nothing}
