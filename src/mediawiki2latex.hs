{-DHUN| This is the main entry point of mediawiki2latex it parses the command line and delegatesto the requested submodules DHUN-}
module Main where
import All
import Control.Monad.State
import Control.Monad.Except
import System.Directory
import System.Environment
import System.Console.GetOpt
import System.Exit
import ImperativeState
import Data.Maybe
import Hex
import Server
import System.FilePath.Posix
import System.Info
import Compiler (runCompile, runTreeToLaTeX, runNewTree)
import Tools (replace2)
import Load
import GetImages
import Data.Serialize as S (decode)
import qualified Data.ByteString as B
{-DHUN| Data structure to repesent a single option on the command line. DHUN-}

data Flag = Verbose
          | Vector
          | Version
          | Templates String
          | Resolution String
          | Paper String
          | Copy String
          | Headers String
          | Input String
          | Output String
          | LibDir String
          | Featured String
          | MediaWiki
          | BookMode
          | HTML
          | InternalTemplates
          | Hex String
          | Zip
          | EPub
          | Odt
          | NoParent
          | LaTeXTables
          | ChromiumTables
          | Server String
          deriving (Show, Eq)

{-DHUN| String constant on for the version command line option. DHUN-}

versionOption :: String
versionOption = "version"

{-DHUN| String constant on for the featured command line option. DHUN-}

featuredOption :: String
featuredOption = "featured"

{-DHUN| String constant on for the resolution command line option. DHUN-}

resolutionOption :: String
resolutionOption = "resolution"

{-DHUN| String constant on for the output command line option. DHUN-}

output :: String
output = "output"

{-DHUN| String constant on for the zip command line option. DHUN-}

zip :: String
zip = "zip"

{-DHUN| String constant on for the hex command line option. DHUN-}

hexen :: String
hexen = "hex"

{-DHUN| String constant on for the templates command line option. DHUN-}

templates :: String
templates = "templates"

{-DHUN| String constant on for the headers command line option. DHUN-}

headers :: String
headers = "headers"

{-DHUN| String constant on for the url command line option. DHUN-}

url :: String
url = "url"

{-DHUN| String constant on for the medaiwiki command line option. DHUN-}

mediawiki :: String
mediawiki = "mediawiki"

{-DHUN| String constant on for the book-namespace command line option. DHUN-}

bookmode :: String
bookmode = "bookmode"

{-DHUN| String constant on for the html command line option. DHUN-}

html :: String
html = "html"

{-DHUN| String constant on for the paper command line option. DHUN-}

paperOption :: String
paperOption = "paper"

{-DHUN| String constant on for the internal command line option. DHUN-}

internal :: String
internal = "internal"

{-DHUN| String constant on for the vector command line option. DHUN-}

vectorOption :: String
vectorOption = "vector"

{-DHUN| String constant on for the copy command line option. DHUN-}

copyOption :: String
copyOption = "copy"

{-DHUN| String constant for the noparent command line option. DHUN-}

noparentOption :: String
noparentOption = "noparent"


{-DHUN| String constant on for the server command line option. DHUN-}

serverOption :: String
serverOption = "server"

{-DHUN| String constant on for the epub command line option. DHUN-}

epubOption :: String
epubOption = "epub"

{-DHUN| String constant on for the odt command line option. DHUN-}

odtOption :: String
odtOption = "odt"


{-DHUN| String constant on for the tableslatex command line option. DHUN-}

tableslatex :: String
tableslatex = "tableslatex"

{-DHUN| String constant on for the tableslatex command line option. DHUN-}

tableschromium :: String
tableschromium = "tableschromium"

{-DHUN| Datastructure describing all possible command line options DHUN-}

options :: [OptDescr Flag]
options
  = [Option ['V', '?', 'v'] [versionOption, "help"] (NoArg Version)
       "show version number",
     Option ['o'] [output] (ReqArg Output "FILE")
       "output FILE (REQUIRED)",
     Option ['f'] [featuredOption] (ReqArg Featured "START:END")
       "run selftest on featured article numbers from START to END",
     Option ['x'] [hexen] (ReqArg Hex "CONFIG")
       "hex encoded full configuration for run",
     Option ['s'] [serverOption] (ReqArg Server "PORT")
       "run in server mode listen on the given port",
     Option ['t'] [templates] (ReqArg Templates "FILE")
       "user template map FILE",
     Option ['r'] [resolutionOption] (ReqArg Resolution "INTEGER")
       "maximum image resolution in dpi INTEGER",
     Option ['u'] [url] (ReqArg Input "URL") "input URL (REQUIRED)",
     Option ['p'] [paperOption] (ReqArg Paper "PAPER")
       "paper size, on of A4,A5,B5,letter,legal,executive",
     Option ['m'] [mediawiki] (NoArg MediaWiki)
       "use MediaWiki to expand templates",
     Option ['h'] [Main.html] (NoArg Main.HTML)
       "use MediaWiki generated html as input (default)",
     Option ['e'] [tableslatex] (NoArg Main.LaTeXTables)
       "use LaTeX to generate tables",
     Option ['a'] [tableschromium] (NoArg Main.ChromiumTables)
       "use Chromium to generate tables",
     Option ['n'] [noparentOption] (NoArg Main.NoParent)
       "only include urls which a children of start url",
     Option ['k'] [bookmode] (NoArg Main.BookMode)
       "use book-namespace mode for expansion",
     Option ['z'] [Main.zip] (NoArg Main.Zip)
       "output zip archive of latex source",
     Option ['b'] [epubOption] (NoArg Main.EPub) "output epub file",
     Option ['d'] [odtOption] (NoArg Main.Odt) "output odt file",
     Option ['g'] [vectorOption] (NoArg Main.Vector)
       "keep vector graphics in vector form",
     Option ['i'] [internal] (NoArg Main.InternalTemplates)
       "use internal template definitions",
     Option ['l'] [Main.headers] (ReqArg Main.Headers "DIRECTORY")
       "use user supplied latex headers",
     Option ['c'] [copyOption] (ReqArg Main.Copy "DIRECTORY")
       "copy LaTeX tree to DIRECTORY"]

{-DHUN| parsed the options given on the command line via the getopt library DHUN-}

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv
  = case getOpt Permute options argv of
        (o, n, []) -> return (o, n)
        (_, _, errs) -> ioError
                          (userError (concat errs ++ usageInfo header options))

{-DHUN| header string for the usage help DHUN-}

header :: String
header = "Usage: mediawiki2latex [OPTION...]"

{-DHUN| header string giving the current version string of mediawiki2latex DHUN-}

versionHeader :: String
versionHeader
  = "mediawiki2latex version 8.26\n" ++ (usageInfo header options)

{-DHUN| print the version string of mediawiki2latex. Takes the output of the compilerOpts function as input. Prints the version string if no options were given or the version command was given does noting otherwise DHUN-}

printVersion :: (Eq a) => ([Flag], [a]) -> IO ()
printVersion o
  = if (Version `elem` (fst o)) || o == ([], []) then
      putStrLn versionHeader >> exitSuccess else return ()

{-DHUN| checks whether the given option exists exactly once in the given (getopt parsed) command line. Takes a predicate (returning a Maybe type) as first input parameter. Takes the decription string for the option under consideration as second parameter. Takes the  (getopt parsed) command line as third input parameter. Return an either monad giving Right together with the value of the option in cases the option exists exacatly once in the command line, gives Left with an Error values otherwise DHUN-}

exactlyOne :: (a -> Maybe b) -> String -> [a] -> Either MyError b
exactlyOne predicate s o
  = case filter isJust (map predicate o) of
        ((Just x) : []) -> Right x
        _ -> Left (NotExcatlyOneError s)

{-DHUN| checks whether the given option exists at most once in the given (getopt parsed) command line. Takes a predicate (returning a Maybe type) as first input parameter. Takes the decription string for the option under consideration as second parameter. Takes the  (getopt parsed) command line as third input parameter. Return an either monad giving Right together with the value of the option in cases the option exists at most once once in the command line, gives Left with an Error values otherwise DHUN-}

atMostOne ::
          (a1 -> Maybe a) -> String -> [a1] -> Either MyError (Maybe a)
atMostOne predicate s o
  = case filter isJust (map predicate o) of
        (x : []) -> Right x
        ([]) -> Right Nothing
        _ -> Left (NotAtMostOneError s)

{-DHUN| predicate for the resolution option. see atMostOne and exactlyOne functions for details DHUN-}

resolutionPredicate :: Flag -> Maybe String
resolutionPredicate (Resolution x) = Just x
resolutionPredicate _ = Nothing

{-DHUN| predicate for the copy option. see atMostOne and exactlyOne functions for details DHUN-}

copyPredicate :: Flag -> Maybe String
copyPredicate (Copy x) = Just x
copyPredicate _ = Nothing

{-DHUN| predicate for the output option. see atMostOne and exactlyOne functions for details DHUN-}

outputPredicate :: Flag -> Maybe String
outputPredicate (Output x) = Just x
outputPredicate _ = Nothing

{-DHUN| predicate for the input option. see atMostOne and exactlyOne functions for details DHUN-}

inputPredicate :: Flag -> Maybe String
inputPredicate (Input x) = Just x
inputPredicate _ = Nothing

{-DHUN| predicate for the templates option. see atMostOne and exactlyOne functions for details DHUN-}

templatesPredicate :: Flag -> Maybe String
templatesPredicate (Templates x) = Just x
templatesPredicate _ = Nothing

{-DHUN| predicate for the headers option. see atMostOne and exactlyOne functions for details DHUN-}

headersPredicate :: Flag -> Maybe String
headersPredicate (Headers x) = Just x
headersPredicate _ = Nothing

{-DHUN| predicate for the hex option. see atMostOne and exactlyOne functions for details DHUN-}

hexPredicate :: Flag -> Maybe String
hexPredicate (Hex x) = Just x
hexPredicate _ = Nothing

{-DHUN| predicate for the server option. see atMostOne and exactlyOne functions for details DHUN-}

serverPredicate :: Flag -> Maybe String
serverPredicate (Server x) = Just x
serverPredicate _ = Nothing

featuredPredicate :: Flag -> Maybe String
featuredPredicate (Featured x) = Just x
featuredPredicate _ = Nothing

{-DHUN| predicate for the paper option. see atMostOne and exactlyOne functions for details DHUN-}

paperPredicate :: Flag -> Maybe String
paperPredicate (Paper x) = Just x
paperPredicate _ = Nothing

{-DHUN| default images resolution. All images with a right resolution will be dithered to this resolution unless is is overridden with the resolution  command line option DHUN-}

defaultResolution :: Integer
defaultResolution = 300

{-DHUN| the default paper format DHUN-}

defaultPaper :: String
defaultPaper = "A4"

{-DHUN| function to count the number of given command lines options which are part of a certain set of possible command line options. Takes a Maybe value representing whether the command line option is present and returns 1 on Just and 0 otherwise DHUN-}

maybeToInt :: (Num a) => Maybe t -> a
maybeToInt (Just _) = 1
maybeToInt _ = 0

{-DHUN| function to count the number of given command lines option which are part of a certain set of possible command line options. Takes a Bool values representing whether the command line option is present and returns 1 on True and 0 otherwise DHUN-}

boolToInt :: (Num a) => Bool -> a
boolToInt True = 1
boolToInt _ = 0

{-DHUN| Caculates a configuration information for the run of program from the options given on the command line. It takes the pathname of the current working directory as first input parameter. It takes the (getopt parsed) command line as second input parameter.It returns an Either Monad. In case the command line made sence the Right type containg the configuation is returned otherwise the Left values with a Error Values desribing the problem is returned DHUN-}

checkOpts :: FilePath -> [Flag] -> Either MyError FullConfig
checkOpts cwd o
  = do serverVal <- atMostOne serverPredicate serverOption o
       featuredVal <- atMostOne featuredPredicate featuredOption o
       case featuredVal of
           Just x -> case
                       (reads ("(" ++ (replace2 x ":" ",") ++ ")")) ::
                         [((Integer, Integer), String)]
                       of
                         [((s, e), _)] | (s <= e) ->
                                         return
                                           FullConfig{ImperativeState.headers = Nothing,
                                                      resolution = 300, outputFilename = "",
                                                      inputUrl = "", runMode = ImperativeState.HTML ImperativeState.No,ltxproc=Nothing,
                                                      paper = "A4", vector = False,
                                                      ImperativeState.copy = Nothing, mainPath = "",
                                                      server = Nothing, selfTest = Just (s, e),
                                                      outputType = PlainPDF, compile = Nothing,
                                                      convert=Nothing, noparent=False, imgctrburl=Nothing,ctrb=Nothing,latexTables=False}
                         _ -> Left (NotIntegerPairError featuredOption)
           _ -> case serverVal of
                    Just x -> case reads x of
                                  [(z, _)] -> return
                                                FullConfig{ImperativeState.headers = Nothing,
                                                           resolution = 300, outputFilename = "",
                                                           inputUrl = "",
                                                           runMode = ImperativeState.HTML ImperativeState.No,
                                                           paper = "A4", vector = False,
                                                           ImperativeState.copy = Nothing,
                                                           mainPath = "", server = Just z,
                                                           outputType = PlainPDF,
                                                           selfTest = Nothing, compile = Nothing,
                                                           convert=Nothing, noparent=False, imgctrburl=Nothing,ctrb=Nothing,latexTables=False, ltxproc=Nothing}
                                  _ -> Left (NotIntegerError serverOption)
                    _ -> do hexVal <- atMostOne hexPredicate hexen o
                            case hexVal of
                                Just x -> do return ((read . unhex) x)
                                _ -> do resolutionOpt <- atMostOne resolutionPredicate
                                                           resolutionOption
                                                           o
                                        resolutionVal <- case resolutionOpt of
                                                             (Just x) -> case reads x of
                                                                             [(z, _)] -> Right z
                                                                             _ -> Left
                                                                                    (NotIntegerError
                                                                                       resolutionOption)
                                                             _ -> Right defaultResolution
                                        outputVal <- exactlyOne outputPredicate output o
                                        inputVal <- exactlyOne inputPredicate url o
                                        templatesVal <- atMostOne templatesPredicate templates o
                                        headersVal <- atMostOne headersPredicate templates o
                                        copyVal <- atMostOne copyPredicate copyOption o
                                        paperOpt <- atMostOne paperPredicate paperOption o
                                        paperVal <- case paperOpt of
                                                        Just x -> if
                                                                    x `elem`
                                                                      ["A4", "A5", "B5", "letter",
                                                                       "legal", "executive"]
                                                                    then Right x else
                                                                    Left PaperError
                                                        _ -> Right defaultPaper
                                        let mediaWikiVal = (MediaWiki `elem` o)
                                        let latexTablesVal = (LaTeXTables `elem` o)
                                        let chromiumTablesVal = (ChromiumTables `elem` o)
                                        let bookModeVal = if (Main.BookMode `elem` o) then ImperativeState.Yes else ImperativeState.No 
                                        let htmlVal = (Main.HTML `elem` o)
                                        let zipVal = (Main.Zip `elem` o)
                                        let epubVal = (Main.EPub `elem` o)
                                        let noparentVal = (Main.NoParent `elem` o)
                                        let odtVal = (Main.Odt `elem` o)
                                        let temVal = (Main.InternalTemplates `elem` o)
                                        let vectorVal = (Main.Vector `elem` o)
                                        let mysum
                                              = (boolToInt temVal) + (boolToInt mediaWikiVal) +
                                                  (boolToInt htmlVal)
                                                  + (maybeToInt templatesVal)
                                        if mysum > 1 then Left ToManyOptionsError else Right ()
                                        let mytablesum
                                              = ((boolToInt latexTablesVal) + (boolToInt chromiumTablesVal))::Integer 
                                        if mytablesum > 1 then Left ToManyTableOptionsError else Right ()
                                        
                                        if
                                          ((boolToInt zipVal) + (boolToInt epubVal) +
                                             (boolToInt odtVal))
                                            > (1 :: Integer)
                                          then Left ToManyOutputOptionsError else Right ()
                                        runModeVal <- if mysum == (1 :: Integer) then
                                                        case templatesVal of
                                                            Just xx -> Right (UserTemplateFile bookModeVal xx)
                                                            _ -> if mediaWikiVal then
                                                                   Right (ExpandedTemplates bookModeVal) else
                                                                   if htmlVal then
                                                                     Right (ImperativeState.HTML bookModeVal) else
                                                                     Right (StandardTemplates bookModeVal)
                                                        else Right (ImperativeState.HTML bookModeVal)
                                        return
                                          (FullConfig{ImperativeState.headers =
                                                        headersVal >>= (return . (cwd </>)),
                                                      resolution = resolutionVal,
                                                      selfTest = Nothing,
                                                      outputFilename = outputVal,
                                                      inputUrl = inputVal, runMode = runModeVal,
                                                      paper = paperVal, vector = vectorVal,
                                                      copy = copyVal >>= (return . (cwd </>)),
                                                      mainPath =
                                                        cwd ++ (if os == "linux" then "" else "\\"),
                                                      server = Nothing,
                                                      outputType =
                                                        if zipVal then ZipArchive else
                                                          if epubVal then EPubFile else
                                                            if odtVal then OdtFile else PlainPDF,
                                                      compile = Nothing, ltxproc=Nothing ,convert=Nothing, noparent=noparentVal, imgctrburl=Nothing,ctrb=Nothing, latexTables=if latexTablesVal then True else (if chromiumTablesVal then False else True)})

{-DHUN| main entry point of mediawiki2latex DHUN-}

main :: IO ()
main
  = do a <- getArgs
       o <- compilerOpts a
       printVersion o
       stz <- imperativeStateZero
       cwd <- getCurrentDirectory
       case (checkOpts cwd (fst o)) of
           Right x -> case (convert x) of 
                       Just stx -> case stx of 
                         NewTree fn -> do _ <- (runStateT (runExceptT (runNewTree fn)) stz)
                                          return ()
                         TreeToLaTeX instfn fn -> do _ <- (runStateT (runExceptT (runTreeToLaTeX instfn fn)) stz)
                                                     return ()
                         NewLoad fn -> do _ <- (runStateT (runExceptT (runqBookIncludeAction fn)) stz)
                                          return ()
                       _-> case (compile x) of
                            Just dir -> do _ <- (runStateT (runExceptT (runCompile dir x)) stz{latexTable=(latexTables x)})
                                           return ()
                            _ -> case (ctrb x) of
                                    Just fn -> do _<-(runStateT (runExceptT (getContribCallBack fn)) stz)
                                                  return ()
                                    _ -> case (server x) of
                                            Just zz -> serve zz
                                            _ ->  case (imgctrburl x) of
                                                   Just (cufile,host) -> do _<-(runStateT (runExceptT (runCtrbUrl cufile host)) stz)
                                                                            return ()
                                                   
                                                   _-> case (ltxproc x) of 
                                                         Just ltxpath -> do t <- liftIO $ B.readFile (ltxpath </> "config")
                                                                            ltxconf<-case S.decode t of 
                                                                                      Right ltxcfg->return (ltxcfg ::LatexConfig)
                                                                                      _-> error "could not read state ltxconf"
                                                                            _<-runLaTeXCallback ltxconf ltxpath
                                                                            return ()
                                                         _-> do print x
                                                                (xx, _) <- (runStateT (runExceptT (All.all x))
                                                                            stz {vectorr=(vector x)})
                                                                case xx of
                                                                  Left n -> print n
                                                                  _ -> return ()
           Left y -> print y
       return ()
