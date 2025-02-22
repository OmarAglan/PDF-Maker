{-# LANGUAGE DefaultSignatures, DeriveAnyClass, DeriveGeneric #-}
{-DHUN| A module for mutable states used in the programm DHUN-}
module MyState where
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Control.Monad.Trans.State (State)
import MediaWikiParseTree
import BaseFont
import Data.Serialize
import GHC.Generics
{-DHUN| a type used as mutable state while processing a table. See documentation of the TableHelper module DHUN-}

data TableState = TableState{rowCounter :: Int,
                             inputLastRowOfHeader :: Int, outputLastRowOfHeader :: Int,
                             outputTableHasHeaderRows :: Bool,
                             lastRowHadEmptyMultiRowMap :: Bool, isFirstRow :: Bool,
                             lastCellWasHeaderCell :: Bool, stillInTableHeader :: Bool,
                             currentColumn :: Int, multiRowMap :: Map Int (Int, Int),
                             numberOfColumnsInTable :: Int, lastCellWasMultiRow :: Bool,
                             seperatingLinesRequestedForTable :: Bool,
                             currentRowIsHeaderRow :: Bool,
                             lastCellWasNotFirstCellOfRow :: Bool, columnsWidthList :: [Float],
                             lastCellWasMultiColumn :: Bool, activeColumn :: Maybe Int}

{-DHUN| see documentation of the makeLables function in WikiHelper module DHUN-}

data UrlState = UrlState{iUrlState :: Int, sUrlState :: String,
                         mUrlState :: Map String String}
              deriving (Show, Eq, Read, Serialize, Generic)

{-DHUN| see initial value of the type UrlState DHUN-}

initialUrlState :: UrlState
initialUrlState
  = UrlState{iUrlState = 0, sUrlState = "", mUrlState = Map.empty}

{-DHUN| a type used as mutable state during the course of the LaTeXRederer DHUN-}

data MyState = MyState{getImages :: [String], getJ :: Int,
                       getF :: Float, getC :: Int, getInTab :: Int, getInGallery :: Bool,
                       getInFootnote :: Bool, getInHeading :: Bool, getInCenter :: Bool,
                       getInCode :: Bool, getTitle :: String,
                       templateMap :: Map String [String], urls :: Map String String,
                       urld :: WikiUrlData, getGalleryNumbers :: [Integer],
                       currentUrl :: String, fndict :: Map String [Anything Char],
                       tablist :: [[String]], tabmap :: Map Int (Map Int Double),
                       fontStack :: [FontStyle], font :: Font, langu :: Maybe String,
                       forms :: Map String Int, lastChar :: Char, lastFontChanged :: Bool, getCaptionLevel :: Integer, vector:: Bool, htmlTables :: [String], latexTabs :: Bool, htmlMaps::[String], htmlColors::Map String String, isSpaceIndent :: Bool, footnoteMap::Map Integer String, footnoteNumber::Integer}
             deriving (Show, Eq, Serialize, Generic)


getInCaption :: MyState->Bool
getInCaption st = (getCaptionLevel st)>0

{-DHUN| Renderer is the State monad using MyState as mutable state DHUN-}

type Renderer = State MyState

{-DHUN| the initial value for MyState DHUN-}

initialState :: MyState
initialState
  = MyState{getImages = [], getJ = 1, getF = 1, getC = 1,
            getInTab = 0, getInGallery = False, getInFootnote = False,
            getInHeading = False, getInCenter = False, getInCode = False,
            getTitle = "", templateMap = Map.fromList [], urls = Map.empty,
            urld = BaseUrl (WikiBaseUrl ""), getGalleryNumbers = [],
            currentUrl = "", fndict = Map.empty, tablist = [],
            tabmap = Map.empty,
            fontStack =
              [FontStyle{stylebase = Normal, bold = False, italic = False}],
            font = ComputerModernRoman, langu = Nothing, forms = Map.empty,
            lastChar = ' ', lastFontChanged = False, getCaptionLevel=0, vector=False,htmlTables=[], latexTabs=False,htmlMaps=[],htmlColors=Map.empty,isSpaceIndent=False, footnoteMap=Map.empty, footnoteNumber=1}

{-DHUN| represents an URL to a wiki (not to a page thereof), which is not a sister project of wikipedia, so not wikibooks wikisource, etc. DHUN-}

data WikiBaseUrl = WikiBaseUrl{baseUrl :: String}
                 deriving (Show, Eq, Serialize, Generic)

{-DHUN| represents an URL to a wiki (not to a page thereof), which is a sister project of wikipedia, so wikibooks wikisource, etc. DHUN-}

data WikiUrlInfo = WikiUrlInfo{language :: String,
                               wikitype :: String}
                 deriving (Show, Eq, Serialize, Generic)

{-DHUN| represents an URL to a wiki (not to a page thereof), which is either a sister project of wikipedia, so wikibooks wikisource, etc. or isn't a sister project of wikipedia DHUN-}

data WikiUrlData = BaseUrl WikiBaseUrl
                 | UrlInfo WikiUrlInfo
                 deriving (Show, Eq, Serialize, Generic)

{-DHUN| represents an URL to a page on a wiki DHUN-}

data WikiLinkInfo = WikiLinkInfo{urldata :: WikiUrlData,
                                 page :: String}
                  deriving (Show, Eq, Serialize, Generic)
                  

