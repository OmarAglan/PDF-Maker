-- | This is the main entry point of mediawiki2latex it parses the command line
-- and delegatesto the requested submodules
module Main where

import All
import Compiler (runCompile, runNewTree, runTreeToLaTeX)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString as B
import Data.Maybe
import Data.Serialize as S (decode)
import GetImages
import Hex
import ImperativeState
import Load
import Server
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.Info
import Test (runTest)

-- | Data structure to represent a single option on the command line.
data Flag
  = Verbose
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
  | Test
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

-- | String constant on for the version command line option.
versionOption :: String
versionOption = "version"

-- | String constant on for the resolution command line option.
resolutionOption :: String
resolutionOption = "resolution"

-- | String constant on for the output command line option.
output :: String
output = "output"

-- | String constant on for the zip command line option.
zip :: String
zip = "zip"

-- | String constant on for the hex command line option.
hexen :: String
hexen = "hex"

-- | String constant on for the templates command line option.
templates :: String
templates = "templates"

-- | String constant on for the headers command line option.
headers :: String
headers = "headers"

-- | String constant on for the url command line option.
url :: String
url = "url"

-- | String constant on for the medaiwiki command line option.
mediawiki :: String
mediawiki = "mediawiki"

-- | String constant on for the book-namespace command line option.
bookmode :: String
bookmode = "bookmode"

-- | String constant on for the html command line option.
html :: String
html = "html"

-- | String constant on for the paper command line option.
paperOption :: String
paperOption = "paper"

-- | String constant on for the internal command line option.
internal :: String
internal = "internal"

-- | String constant on for the self test command line option.
test :: String
test = "test"

-- | String constant on for the vector command line option.
vectorOption :: String
vectorOption = "vector"

-- | String constant on for the copy command line option.
copyOption :: String
copyOption = "copy"

-- | String constant for the noparent command line option.
noparentOption :: String
noparentOption = "noparent"

-- | String constant on for the server command line option.
serverOption :: String
serverOption = "server"

-- | String constant on for the epub command line option.
epubOption :: String
epubOption = "epub"

-- | String constant on for the odt command line option.
odtOption :: String
odtOption = "odt"

-- | String constant on for the tableslatex command line option.
tableslatex :: String
tableslatex = "tableslatex"

-- | String constant on for the tableslatex command line option.
tableschromium :: String
tableschromium = "tableschromium"

-- | Datastructure describing all possible command line options
options :: [OptDescr Flag]
options =
  [ Option
      ['V', '?', 'v']
      [versionOption, "help"]
      (NoArg Version)
      "show version number",
    Option
      ['o']
      [output]
      (ReqArg Output "FILE")
      "output FILE (REQUIRED)",
    Option
      ['x']
      [hexen]
      (ReqArg Hex "CONFIG")
      "hex encoded full configuration for run",
    Option
      ['s']
      [serverOption]
      (ReqArg Server "PORT")
      "run in server mode listen on the given port",
    Option
      ['t']
      [templates]
      (ReqArg Templates "FILE")
      "user template map FILE",
    Option
      ['r']
      [resolutionOption]
      (ReqArg Resolution "INTEGER")
      "maximum image resolution in dpi INTEGER",
    Option ['u'] [url] (ReqArg Input "URL") "input URL (REQUIRED)",
    Option
      ['p']
      [paperOption]
      (ReqArg Paper "PAPER")
      "paper size, on of A4,A5,B5,letter,legal,executive",
    Option
      ['m']
      [mediawiki]
      (NoArg MediaWiki)
      "use MediaWiki to expand templates",
    Option
      ['h']
      [Main.html]
      (NoArg Main.HTML)
      "use MediaWiki generated html as input (default)",
    Option
      ['e']
      [tableslatex]
      (NoArg Main.LaTeXTables)
      "use LaTeX to generate tables",
    Option
      ['a']
      [tableschromium]
      (NoArg Main.ChromiumTables)
      "use Chromium to generate tables",
    Option
      ['n']
      [noparentOption]
      (NoArg Main.NoParent)
      "only include urls which a children of start url",
    Option
      ['k']
      [bookmode]
      (NoArg Main.BookMode)
      "use book-namespace mode for expansion",
    Option
      ['z']
      [Main.zip]
      (NoArg Main.Zip)
      "output zip archive of latex source",
    Option ['b'] [epubOption] (NoArg Main.EPub) "output epub file",
    Option ['d'] [odtOption] (NoArg Main.Odt) "output odt file",
    Option
      ['g']
      [vectorOption]
      (NoArg Main.Vector)
      "keep vector graphics in vector form",
    Option
      ['i']
      [internal]
      (NoArg Main.InternalTemplates)
      "use internal template definitions",
    Option
      ['f']
      [test]
      (NoArg Main.Test)
      "run self test",
    Option
      ['l']
      [Main.headers]
      (ReqArg Main.Headers "DIRECTORY")
      "use user supplied latex headers",
    Option
      ['c']
      [copyOption]
      (ReqArg Main.Copy "DIRECTORY")
      "copy LaTeX tree to DIRECTORY"
  ]

-- | parsed the options given on the command line via the getopt library
compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (o, n)
    (_, _, errs) ->
      ioError
        (userError (concat errs ++ usageInfo header options))

-- | header string for the usage help
header :: String
header = "Usage: mediawiki2latex [OPTION...]"

-- | header string giving the current version string of mediawiki2latex
versionHeader :: String
versionHeader =
  "mediawiki2latex version 8.32\n" ++ (usageInfo header options)

-- | print the version string of mediawiki2latex. Takes the output of the
-- compilerOpts function as input. Prints the version string if no options were
-- given or the version command was given does noting otherwise
printVersion :: (Eq a) => ([Flag], [a]) -> IO ()
printVersion o =
  if (Version `elem` (fst o)) || o == ([], [])
    then putStrLn versionHeader >> exitSuccess
    else return ()

-- | checks whether the given option exists exactly once in the given (getopt
-- parsed) command line. Takes a predicate (returning a Maybe type) as first
-- input parameter. Takes the decription string for the option under
-- consideration as second parameter. Takes the  (getopt parsed) command line as
-- third input parameter. Return an either monad giving Right together with the
-- value of the option in cases the option exists exacatly once in the command
-- line, gives Left with an Error values otherwise
exactlyOne :: (a -> Maybe b) -> String -> [a] -> Either MyError b
exactlyOne predicate s o =
  case filter isJust (map predicate o) of
    ((Just x) : []) -> Right x
    _ -> Left (NotExcatlyOneError s)

-- | checks whether the given option exists at most once in the given (getopt
-- parsed) command line. Takes a predicate (returning a Maybe type) as first
-- input parameter. Takes the decription string for the option under
-- consideration as second parameter. Takes the  (getopt parsed) command line as
-- third input parameter. Return an either monad giving Right together with the
-- value of the option in cases the option exists at most once once in the
-- command line, gives Left with an Error values otherwise
atMostOne ::
  (a1 -> Maybe a) -> String -> [a1] -> Either MyError (Maybe a)
atMostOne predicate s o =
  case filter isJust (map predicate o) of
    (x : []) -> Right x
    ([]) -> Right Nothing
    _ -> Left (NotAtMostOneError s)

-- | predicate for the resolution option. see atMostOne and exactlyOne functions
-- for details
resolutionPredicate :: Flag -> Maybe String
resolutionPredicate (Resolution x) = Just x
resolutionPredicate _ = Nothing

-- | predicate for the copy option. see atMostOne and exactlyOne functions for
-- details
copyPredicate :: Flag -> Maybe String
copyPredicate (Copy x) = Just x
copyPredicate _ = Nothing

-- | predicate for the output option. see atMostOne and exactlyOne functions for
-- details
outputPredicate :: Flag -> Maybe String
outputPredicate (Output x) = Just x
outputPredicate _ = Nothing

-- | predicate for the input option. see atMostOne and exactlyOne functions for
-- details
inputPredicate :: Flag -> Maybe String
inputPredicate (Input x) = Just x
inputPredicate _ = Nothing

-- | predicate for the templates option. see atMostOne and exactlyOne functions
-- for details
templatesPredicate :: Flag -> Maybe String
templatesPredicate (Templates x) = Just x
templatesPredicate _ = Nothing

-- | predicate for the headers option. see atMostOne and exactlyOne functions
-- for details
headersPredicate :: Flag -> Maybe String
headersPredicate (Headers x) = Just x
headersPredicate _ = Nothing

-- | predicate for the hex option. see atMostOne and exactlyOne functions for
-- details
hexPredicate :: Flag -> Maybe String
hexPredicate (Hex x) = Just x
hexPredicate _ = Nothing

-- | predicate for the server option. see atMostOne and exactlyOne functions for
-- details
serverPredicate :: Flag -> Maybe String
serverPredicate (Server x) = Just x
serverPredicate _ = Nothing

-- | predicate for the paper option. see atMostOne and exactlyOne functions for
-- details
paperPredicate :: Flag -> Maybe String
paperPredicate (Paper x) = Just x
paperPredicate _ = Nothing

-- | default images resolution. All images with a right resolution will be
-- dithered to this resolution unless is is overridden with the resolution
-- command line option
defaultResolution :: Integer
defaultResolution = 300

-- | the default paper format
defaultPaper :: String
defaultPaper = "A4"

-- | function to count the number of given command lines options which are part
-- of a certain set of possible command line options. Takes a Maybe value
-- representing whether the command line option is present and returns 1 on Just
-- and 0 otherwise
maybeToInt :: (Num a) => Maybe t -> a
maybeToInt (Just _) = 1
maybeToInt _ = 0

-- | function to count the number of given command lines option which are part
-- of a certain set of possible command line options. Takes a Bool values
-- representing whether the command line option is present and returns 1 on True
-- and 0 otherwise
boolToInt :: (Num a) => Bool -> a
boolToInt True = 1
boolToInt _ = 0

-- | Calculates a configuration information for the run of program from the
-- options given on the command line. It takes the pathname of the current
-- working directory as first input parameter. It takes the (getopt parsed)
-- command line as second input parameter.It returns an Either Monad. In case
-- the command line made sense the Right type containing the configuration is
-- returned otherwise the Left values with a Error Values describing the problem
-- is returned
checkOpts :: FilePath -> [Flag] -> Either MyError FullConfig
checkOpts cwd o =
  do
    serverVal <- atMostOne serverPredicate serverOption o
    case serverVal of
      Just x -> case reads x of
        [(z, _)] ->
          return
            FullConfig
              { ImperativeState.headers = Nothing,
                resolution = 300,
                outputFilename = "",
                inputUrl = "",
                runMode = ImperativeState.HTML ImperativeState.No,
                paper = "A4",
                vector = False,
                ImperativeState.copy = Nothing,
                mainPath = "",
                server = Just z,
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
        _ -> Left (NotIntegerError serverOption)
      _ -> do
        hexVal <- atMostOne hexPredicate hexen o
        case hexVal of
          Just x -> do return ((read . unhex) x)
          _ -> do
            resolutionOpt <-
              atMostOne
                resolutionPredicate
                resolutionOption
                o
            resolutionVal <- case resolutionOpt of
              (Just x) -> case reads x of
                [(z, _)] -> Right z
                _ ->
                  Left
                    ( NotIntegerError
                        resolutionOption
                    )
              _ -> Right defaultResolution
            let testVal = (Main.Test `elem` o)
            outputVal <- if testVal then return "" else exactlyOne outputPredicate output o
            inputVal <- if testVal then return "" else exactlyOne inputPredicate url o
            templatesVal <- atMostOne templatesPredicate templates o
            headersVal <- atMostOne headersPredicate templates o
            copyVal <- atMostOne copyPredicate copyOption o
            paperOpt <- atMostOne paperPredicate paperOption o
            paperVal <- case paperOpt of
              Just x ->
                if x
                  `elem` [ "A4",
                           "A5",
                           "B5",
                           "letter",
                           "legal",
                           "executive"
                         ]
                  then Right x
                  else Left PaperError
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
            let mysum =
                  (boolToInt temVal)
                    + (boolToInt mediaWikiVal)
                    + (boolToInt htmlVal)
                    + (maybeToInt templatesVal)
            if mysum > 1 then Left ToManyOptionsError else Right ()
            if testVal && ((length o) > 1) then Left ToManyTestOptionsError else Right ()
            let mytablesum =
                  ((boolToInt latexTablesVal) + (boolToInt chromiumTablesVal)) :: Integer
            if mytablesum > 1 then Left ToManyTableOptionsError else Right ()

            if ( (boolToInt zipVal)
                   + (boolToInt epubVal)
                   + (boolToInt odtVal)
               )
              > (1 :: Integer)
              then Left ToManyOutputOptionsError
              else Right ()
            runModeVal <-
              if mysum == (1 :: Integer)
                then case templatesVal of
                  Just xx -> Right (UserTemplateFile bookModeVal xx)
                  _ ->
                    if mediaWikiVal
                      then Right (ExpandedTemplates bookModeVal)
                      else
                        if htmlVal
                          then Right (ImperativeState.HTML bookModeVal)
                          else Right (StandardTemplates bookModeVal)
                else Right (ImperativeState.HTML bookModeVal)
            return
              ( FullConfig
                  { ImperativeState.headers =
                      headersVal >>= (return . (cwd </>)),
                    resolution = resolutionVal,
                    outputFilename = outputVal,
                    inputUrl = inputVal,
                    runMode = runModeVal,
                    paper = paperVal,
                    vector = vectorVal,
                    copy = copyVal >>= (return . (cwd </>)),
                    mainPath =
                      cwd ++ (if os == "linux" then "" else "\\"),
                    server = Nothing,
                    outputType =
                      if zipVal
                        then ZipArchive
                        else
                          if epubVal
                            then EPubFile
                            else if odtVal then OdtFile else PlainPDF,
                    compile = Nothing,
                    ltxproc = Nothing,
                    convert = Nothing,
                    noparent = noparentVal,
                    imgctrburl = Nothing,
                    ctrb = Nothing,
                    latexTables = if latexTablesVal then True else (if chromiumTablesVal then False else True),
                    testMode = testVal
                  }
              )

-- | main entry point of mediawiki2latex
main :: IO ()
main =
  do
    a <- getArgs
    o <- compilerOpts a
    printVersion o
    stz <- imperativeStateZero
    cwd <- getCurrentDirectory
    case (checkOpts cwd (fst o)) of
      Right x ->
        if testMode x
          then runTest
          else case (convert x) of
            Just stx -> case stx of
              NewTree fn -> do
                _ <- (runStateT (runExceptT (runNewTree fn)) stz)
                return ()
              TreeToLaTeX instfn fn -> do
                _ <- (runStateT (runExceptT (runTreeToLaTeX instfn fn)) stz)
                return ()
              NewLoad fn -> do
                _ <- (runStateT (runExceptT (runqBookIncludeAction fn)) stz)
                return ()
            _ -> case (compile x) of
              Just dir -> do
                _ <- (runStateT (runExceptT (runCompile dir x)) stz)
                return ()
              _ -> case (ctrb x) of
                Just fn -> do
                  _ <- (runStateT (runExceptT (getContribCallBack fn)) stz)
                  return ()
                _ -> case (server x) of
                  Just zz -> serve zz
                  _ -> case (imgctrburl x) of
                    Just (cufile, host) -> do
                      _ <- (runStateT (runExceptT (runCtrbUrl cufile host)) stz)
                      return ()
                    _ -> case (ltxproc x) of
                      Just ltxpath -> do
                        t <- liftIO $ B.readFile (ltxpath </> "config")
                        ltxconf <- case S.decode t of
                          Right ltxcfg -> return (ltxcfg :: LatexConfig)
                          _ -> error "could not read state ltxconf"
                        _ <- runLaTeXCallback ltxconf ltxpath
                        return ()
                      _ -> do
                        print x
                        (xx, _) <-
                          ( runStateT
                              (runExceptT (All.all x))
                              stz {vectorr = (vector x)}
                            )
                        case xx of
                          Left n -> print n
                          _ -> return ()
      Left y -> print y
    return ()
