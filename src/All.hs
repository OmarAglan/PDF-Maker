-- | imperative part of the program. The outer flow of the program is controlled
-- here
module All where

import Babel
import Codec.Archive.Zip
import Codec.Binary.UTF8.String
import Compiler
import Control.Concurrent
import Control.Concurrent.MVar ()
import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.ByteString hiding
  ( drop,
    dropWhile,
    elem,
    intercalate,
    map,
    reverse,
    take,
    takeWhile,
    zip,
  )
import Data.ByteString.Char8 (any, singleton)
import qualified Data.ByteString.Lazy as LZ (toStrict)
import Data.ByteString.UTF8 (toString)
import Data.Char
import Data.Hashable
import Data.List
import Data.List.Split
import Data.Map.Strict hiding (drop, map, take)
import qualified Data.Map.Strict as Map hiding (drop, take)
import qualified Data.Serialize as S (encode)
import Data.String.HT
import Data.Word8 (isSpace)
import GHC.Conc
import GetImages
import Hex
import HtmlParser (parseHtml)
import ImperativeState
import LatexRenderer
import Load
import MagicStrings
import MediaWikiParseTree
import MediaWikiParser
import MyState (footnoteMap, getInTab)
import Network.URI
import Network.URL
import SimpleContributors
import Static
import System.Directory
import System.Directory.Tree
import System.FilePath
import System.IO.Temp
import System.Info
import System.Process hiding (cwd)
import Text.Read (readMaybe)
import Tools
import UrlAnalyse
import Wiki2Fen
import WikiHelper

-- | takes a filename as input parameter and returns the so called normalized
-- extension for it. This is and extension classifying the type of the file
-- while used in this program. Its not necessarily the real extension of the
-- filename. For example .jpeg images will be converted to jpg and so on.
getExtension :: String -> String
getExtension s =
  normalizeExtension2
    (map toLower (reverse . (takeWhile (/= '.')) . reverse $ s))

-- | Get the extension of a filename.
getExtensionBase ::
  -- | The filename of which the extension shall be determined.
  String ->
  -- | The extension of the filename given in the first parameter.
  String
getExtensionBase s =
  (map toLower (reverse . (takeWhile (/= '.')) . reverse $ s))

-- | returns that name of image magick convert command on the current operating
-- system. It takes the path from which mediawiki2latex was started as input
-- parameter. On Windows convert.exe has to reside in a path relative to it for
-- this to work. On linux it does not matter
getConvert :: FilePath -> String
getConvert p =
  if os == "linux" then convertexe else (getPathPrefix p) ++ convertexe
  where
    convertexe =
      if os == "linux" then "convert " else "convert.exe "

-- | Generates the LaTeX version information for the title page.  If title
-- information is found in the CompileResult it is used, otherwise it is
-- generated from the FullWikiUrl
makeTitle ::
  -- | The result of the wiki text compilation (CompileResult).
  CompileResult ->
  -- | The FullWikiUrl to the article.
  FullWikiUrl ->
  -- | The LaTeX version of the title page information.
  [Char]
makeTitle result fu = theTitle
  where
    theTitle =
      if (title result) == ""
        then pub ++ tit
        else pub ++ (title result)
    pub =
      "\\publishers{"
        ++ (Data.List.concat (map chartrans (UrlAnalyse.hostname fu)))
        ++ "}\n"
    tit =
      "\\title{"
        ++ (Data.List.concat (map (chartrans) ((removePrintVersion (lemma fu)))))
        ++ "}\n"

-- | Generates the HTML version information for the title page.  If title
-- information is found in the CompileResult it is used, otherwise it is
-- generated from the FullWikiUrl
makeTitle2 ::
  -- | The result of the wiki text compilation (CompileResult).
  CompileResult ->
  -- | The FullWikiUrl to the article.
  FullWikiUrl ->
  -- | The HTML version of the title page information.
  [Char]
makeTitle2 result fu = theTitle
  where
    theTitle =
      if (title result) == ""
        then tit
        else (title result)
    tit = (Data.List.concat (map (chartrans) ((removePrintVersion (lemma fu)))))

-- | returns the prefix of the path where addional needed software resides
-- depending on the operation system
getPathPrefix :: FilePath -> String
getPathPrefix p = if os == "linux" then "" else (p ++ "..\\lib\\")

-- | applied the necessary image processing to a file so that it can be included
-- in a latex document and does not take too much diskspace. It takes the path
-- form which mediawiki2latex was started as first input parameter. It takes
-- filename with its extension striped as second input parameter. It takes the
-- normalized extension of the file as third parameter (see also function
-- getExtension in this module). It takes the maximum resolution that images
-- should have as fourth parameter. It takes the imagenumbers of the images
-- residing in image galleries in the wiki source text as fifth parameter (those
-- images are given a lower absolute with in cm in the pdf document and thus can
-- be dithered to a loweder absolute with in pixels). It takes the image number
-- of the current image as sixth parameter.
runFileMods ::
  String ->
  FilePath ->
  String ->
  String ->
  Integer ->
  [Integer] ->
  Integer ->
  String ->
  IO ()
runFileMods
  extbase
  p
  filenamebase
  extension
  theResolution
  gals
  imgNumber
  pathname =
    case extension of
      ('s' : ('v' : ('g' : _))) -> do
        _ <-
          system
            ( (getPathPrefix p)
                ++ "rsvg-convert -u -o "
                ++ pngfilename
                ++ " -a -w 1250 "
                ++ filename
            )
        postprocpng pngfilename
        _ <-
          system
            ( (getPathPrefix p)
                ++ "rsvg-convert -u --format=pdf -o "
                ++ (filenamebase ++ "." ++ "pdf")
                ++ " -a -w 1250 "
                ++ filename
            )
        return ()
      ('g' : ('i' : ('f' : _))) -> do
        stdfun
        b <- doesFileExist firstpngfilename
        if b
          then copyFile firstpngfilename newfilename
          else return ()
        postprocpng newfilename
      ('t' : ('i' : ('f' : _))) -> do
        stdfun
        postprocpng newfilename
      ('x' : ('c' : ('f' : _))) -> do
        stdfun
        postprocpng newfilename
      ('j' : ('p' : ('g' : _))) -> postprocjpg filename
      ('p' : ('n' : ('g' : _))) -> postprocpng filename
      ('d' : ('j' : ('v' : ('u' : _)))) -> do
        let hx = case (dropWhile (/= '?') extbase) of
              ('?' : 'p' : 'a' : 'g' : 'e' : '=' : ys) -> (readMaybe ys :: (Maybe Integer))
              _ -> Nothing
        case hx of
          Just n -> djvupngp n
          _ -> djvupng
        b <- doesFileExist firstpngfilename
        if b then copyFile firstpngfilename newfilename else return ()
      ('w' : ('e' : ('b' : ('p' : _)))) -> do
        stdfun
        postprocpng newfilename
      ('p' : 'd' : 'f' : _) -> do
        _ <- system ("pdftk " ++ filename ++ " output " ++ filename ++ ".tmp")
        copyFile (filename ++ ".tmp") filename
      _ -> return ()
    where
      firstpngfilename =
        (reverse . (drop 4) . reverse $ newfilename) ++ "-0.png"
      newfilename = filenamebase ++ "." ++ (normalizeExtension extension)
      filename = filenamebase ++ "." ++ extension
      stdfun =
        do
          _ <-
            system
              ( (getConvert p)
                  ++ "\""
                  ++ filename
                  ++ "\" \""
                  ++ newfilename
                  ++ "\""
              )
          return ()
      djvupngp :: Integer -> IO ()
      djvupngp n = do
        _ <-
          system
            ( "djvused \""
                ++ filename
                ++ "\"  -e \"select "
                ++ (show n)
                ++ "; save-page-with "
                ++ filenamebase
                ++ "b.djvu\""
            )
        _ <-
          system
            ( "ddjvu -format=pdf "
                ++ "\""
                ++ filenamebase
                ++ "b.djvu"
                ++ "\"  \""
                ++ filenamebase
                ++ ".pdf"
                ++ "\""
            )
        _ <-
          system
            ( (getConvert p)
                ++ "\""
                ++ filenamebase
                ++ ".pdf"
                ++ "\" \""
                ++ newfilename
                ++ "\""
            )
        return ()
      djvupng = do
        _ <- system ("ddjvu -format=pdf " ++ "\"" ++ filename ++ "\"  \"" ++ filenamebase ++ ".pdf" ++ "\"")
        _ <-
          system
            ( (getConvert p)
                ++ "\""
                ++ filenamebase
                ++ ".pdf"
                ++ "\" \""
                ++ newfilename
                ++ "\""
            )
        return ()
      postprocpng fn =
        do
          _ <- background fn
          dither fn
          return ()
      postprocjpg fn =
        do
          _ <- system ((getConvert p) ++ "-verbose " ++ fn ++ " " ++ fn)
          _ <- system ("exiftool -all=  -overwrite_original " ++ fn)
          dither fn
          return ()
      background fn =
        system
          ((getConvert p) ++ fn ++ " -background white -flatten " ++ fn)
      pngfilename = filenamebase ++ "." ++ "png"

      dither :: String -> IO ()
      dither fn =
        do
          _ <-
            system
              ( (getConvert p)
                  ++ "-verbose "
                  ++ fn
                  ++ " -format '%w'  "
                  ++ pathname
                  ++ "nullfile.bmp"
                  ++ " > "
                  ++ pathname
                  ++ "dump2 2> "
                  ++ pathname
                  ++ "dump"
              )
          dump <- Tools.readFile (pathname ++ "dump")
          case reverse
            (Prelude.filter (\x -> (trim x) /= "") (splitOn "\n" dump)) of
            (x : _) -> case splitOn " " x of
              (_ : (_ : (y : _))) -> case splitOn "x" y of
                (z : _) -> case reads z of
                  [ ( ii,
                      _
                      )
                    ] -> do
                      runDither
                        fn
                        ( if imgNumber
                            `elem` gals
                            then galleryWidth
                            else imageWidth
                        )
                        ii
                  _ -> return ()
                _ -> return ()
              _ -> return ()
            _ -> return ()

      runDither :: String -> Integer -> Integer -> IO ()
      runDither fn newSize oldSize =
        if newSize < oldSize
          then
            system
              ( (getConvert p)
                  ++ fn
                  ++ " -resize "
                  ++ (show newSize)
                  ++ " "
                  ++ fn
              )
              >> return ()
          else return ()
      textWidth = 10.5
      galleryImageWidth = 10.5

      centimetersPerInch :: Double
      centimetersPerInch = 2.54

      galleryWidth :: Integer
      galleryWidth =
        round
          ( (fromIntegral theResolution)
              * galleryImageWidth
              / centimetersPerInch
          )

      imageWidth :: Integer
      imageWidth =
        round
          ((fromIntegral theResolution) * textWidth / centimetersPerInch)

-- | function to write the image files into the temporary directory and modify
-- the files for use in a LaTeX document. It takes the pathname of temporary
-- image download directory as first input parameter. Please note that the
-- temporary image download directory is always different from the temporary
-- directory. It takes the pathname of the directory in which mediawiki2latex
-- was stated as second input parameter. It takes the name of the temporary
-- directory as third input parameter. It takes the result of the image
-- downloading process as fourth input parameter. This structure has a Maybe
-- monad as outer type since the image download may fail. Inside is a 3 element
-- tuple. The first one is the filename of the image on the wiki. The second one
-- is the number under which the image was stored in the temporary image
-- download directory. The third one is a list of possible urls where the image
-- may be found on the wiki. It takes maximum resolution that images should have
-- as  fifth parameter. It takes the list of images number of images residing in
-- galleries as sixth input parameter.
writeFiles ::
  FilePath ->
  FilePath ->
  String ->
  [ImageCredits] ->
  Integer ->
  [Integer] ->
  IO ()
writeFiles dir p pathname theImages theResolution gals =
  mapM_ go (Prelude.zip ([1 ..] :: [Integer]) theImages)
  where
    go (i, x) =
      do
        let filenamebase = (pathname ++ (show i))
        let filename =
              filenamebase ++ "." ++ (getExtension (wikiFilename x))
        filecontent <-
          Data.ByteString.readFile
            (dir </> (show (imageNumber x)))
        Data.ByteString.writeFile filename filecontent
        runFileMods
          (getExtensionBase (wikiFilename x))
          p
          filenamebase
          (getExtension (wikiFilename x))
          theResolution
          gals
          i
          pathname

-- | Write the image files of the LaTeX formulas to a given path name.
writeFiles2 ::
  -- | The temporary directory where to read the image files of the formulas
  -- from.
  String ->
  String -> --  ^ The target directory where to store the image files of the formulas.

  -- | A list of pairs. The first element of each pair is the filename of the
  -- image file of a formula. The second element of each pair is its width in
  -- pixels
  [(String, Int)] ->
  -- | The IO action copying the file to the target directory.
  IO ()
writeFiles2 tmpdir pathname forms = mapM_ go forms
  where
    go (x, _) =
      do
        filecontent <- Data.ByteString.readFile (tmpdir </> x)
        Data.ByteString.writeFile
          (pathname ++ (Prelude.last (splitOn "/" x)))
          filecontent

-- | Run LaTeX or any other result generating software  in the current process.
-- If an other output than LaTeX is requested it run other software to create
-- the desired result. E.g. it run calibre for EPUB output or libreoffice if ODT
-- is requested.
runLaTeX ::
  -- | The LaTeX configuration to run LaTeX or other external software on.
  LatexConfig ->
  -- | The final result file wrapped in the Imperative Monad since external
  -- software has to be called in the IO monad.
  ImperativeMonad ByteString
runLaTeX config =
  liftIO
    ( do
        home <- getHomeDirectory
        ( withTempDirectory
            home
            "MediaWiki2LaTeX-tmp"
            (runLaTeXCallback config)
          )
    )

-- | Run LaTeX in a new subprocess process to determine the list of maximum
-- column widths. A new subprocess is created because its memory gets freed for
-- sure on its termination, so a memory leak in mediawiki2latex can surely be
-- avoided this way. Mediawiki2latex is calling itself as a subprocess which
-- will in turn run LaTeX to generates final result.
runLaTeXIOAction ::
  -- | The LaTeX configuration to run LaTeX.
  LatexConfig ->
  -- | A template string to become part of the temporary directory in which the
  -- communication with the subprocess is done.
  String ->
  -- | The comma separated list of the column widths of all tables in the
  -- document to process.
  IO ByteString
runLaTeXIOAction config subpath =
  do
    home <- getHomeDirectory
    ( withTempDirectory
        home
        ("MediaWiki2LaTeX-" ++ subpath ++ "-tmp")
        ( \p -> do
            Data.ByteString.writeFile (p </> "config") (S.encode config)
            _ <-
              system
                ( "mediawiki2latex -x "
                    ++ (Hex.hex (show (fullconfigbase {ltxproc = Just p})))
                )
            te <-
              Control.Exception.catch
                (Tools.readFile (p </> "document/main" </> "main.txt"))
                catchFun
            case splitOn "\n" te of
              (x : xs) -> return (pack (encode (intercalate "," (map (strip "pt\r,\f") (x : xs)))))
              _ -> return (pack (encode ""))
        )
      )

-- | Run LaTeX or any other result generating software  in the current process.
-- If an other output than LaTeX is requested it run other software to create
-- the desired result. E.g. it run calibre for EPUB output or libreoffice if ODT
-- is requested. This procedure runs in a temporary directory where final result
-- is calculated.
runLaTeXCallback ::
  -- | The LaTeX configuration to run the result generating software on.
  LatexConfig ->
  -- | The temporary path where to run the result generating software in.
  FilePath ->
  -- | The final result wrapped in the IO monad.
  IO ByteString
runLaTeXCallback config pathname =
  do
    extract pathname
    case (ImperativeState.headers (fullConfig config)) of
      Just x -> do
        d <-
          readDirectoryWith
            Data.ByteString.readFile
            (x ++ "/./")
        _ <-
          writeDirectoryWith
            Data.ByteString.writeFile
            d {anchor = pathname </> "document/headers"}
        return ()
      _ -> return ()
    if os == "linux"
      then return ()
      else do
        d <-
          readDirectoryWith
            Data.ByteString.readFile
            ((mainPath (fullConfig config)) ++ "../fonts/main/")
        _ <-
          writeDirectoryWith
            Data.ByteString.writeFile
            d {anchor = pathname ++ "/document/"}
        return ()
    Tools.writeFile
      (pathname ++ "/document/main/main.tex")
      (content config)
    Tools.writeFile
      (pathname ++ "/document/index.html")
      ( (html (theResult config))
          ++ "<h2>List of Figures</h2>"
          ++ (figHTML config)
      )

    Tools.writeFile
      (pathname ++ "/document/headers/svg.tex")
      ( if vector (fullConfig config)
          then "\\newcommand{\\SVGExtension}{pdf}"
          else "\\newcommand{\\SVGExtension}{png}"
      )
    Tools.writeFile
      (pathname ++ "/document/headers/title.tex")
      (ltitle config)
    Tools.writeFile
      (pathname ++ "/document/headers/babel.tex")
      (makeBabel (lang config) (lhostname config))
    Tools.writeFile
      (pathname ++ "/document/headers/paper.tex")
      ("\\KOMAoption{paper}{" ++ (paper (fullConfig config)) ++ "}")
    if (onlyTables config)
      then return ()
      else do
        myprint " preparing images for LaTeX document"
        All.writeFiles
          (theTempDir config)
          (mainPath (fullConfig config))
          (pathname ++ "/document/images/")
          (figures config)
          (resolution (fullConfig config))
          (galleryNumbers (theResult config))
        All.writeFiles2
          (theTempDir config)
          (pathname ++ "/document/formulas/")
          (formulas config)
    _ <- mapM (\(n, c) -> (Tools.writeFile (pathname ++ "/document/table" ++ (show n) ++ ".html") c) >> system ("chromium --no-sandbox --no-pdf-header-footer --headless --print-to-pdf=\"" ++ pathname ++ "/document/table" ++ (show n) ++ ".pdf\" " ++ pathname ++ "/document/table" ++ (show n) ++ ".html >/dev/null 2>/dev/null") >> system ("pdfcrop --margins '0 0 0 0' " ++ pathname ++ "/document/table" ++ (show n) ++ ".pdf " ++ pathname ++ "/document/table" ++ (show n) ++ ".pdf >/dev/null 2>/dev/null")) (zip ([1 ..] :: [Integer]) (theHtmlTables config))
    _ <- mapM (\(n, c) -> (Tools.writeFile (pathname ++ "/document/map" ++ (show n) ++ ".html") c) >> system ("chromium --no-sandbox --no-pdf-header-footer --headless --print-to-pdf=\"" ++ pathname ++ "/document/map" ++ (show n) ++ ".pdf\" " ++ pathname ++ "/document/map" ++ (show n) ++ ".html >/dev/null 2>/dev/null") >> system ("pdfcrop --margins '0 0 0 0' " ++ pathname ++ "/document/map" ++ (show n) ++ ".pdf " ++ pathname ++ "/document/map" ++ (show n) ++ ".pdf >/dev/null 2>/dev/null")) (zip ([1 ..] :: [Integer]) (theHtmlMapes config))
    _ <- mapM (\(n, c) -> (Tools.writeFile (pathname ++ "/document/images/chess" ++ (show n) ++ ".tex") ("\\documentclass{article}\n\\usepackage[ps]{skak}\n\\begin{document}\n\\thispagestyle{empty}\n\\fenboard{" ++ (fst (toBoard c)) ++ "}\n\\showboard\n\\highlight[x]{" ++ (snd (toBoard c)) ++ "}\n\\end{document}")) >> (system ("latex -interaction=batchmode -output-directory " ++ pathname ++ "/document/images/" ++ " " ++ pathname ++ "/document/images/chess" ++ (show n) ++ ".tex")) >> (system ("dvips " ++ "-o " ++ pathname ++ "/document/images/chess" ++ (show n) ++ ".ps " ++ pathname ++ "/document/images/chess" ++ (show n) ++ ".dvi")) >> (system ("ps2pdf " ++ pathname ++ "/document/images/chess" ++ (show n) ++ ".ps " ++ pathname ++ "/document/images/chess" ++ (show n) ++ ".pdf")) >> (system ("pdfcrop " ++ pathname ++ "/document/images/chess" ++ (show n) ++ ".pdf " ++ pathname ++ "/document/images/chess" ++ (show n) ++ ".pdf "))) (zip ([1 ..] :: [Integer]) (theChessBoardes config))

    cwd <- getCurrentDirectory
    setCurrentDirectory (pathname ++ "/document/main")
    case (ImperativeState.copy (fullConfig config)) of
      Just x -> do
        d <-
          readDirectoryWith
            Data.ByteString.readFile
            "../../document/"
        _ <- writeDirectoryWith Data.ByteString.writeFile d {anchor = x}
        return ()
      _ -> return ()
    _ <-
      forM
        ( ( if onlyTables config
              then [1]
              else
                if ((outputType (fullConfig config)) /= PlainPDF)
                  then []
                  else [1, 2]
          ) ::
            [Integer]
        )
        ( \r ->
            do
              if (onlyTables config)
                then return ()
                else myprint (" generating PDF file. LaTeX run " ++ (show r) ++ " of 4")
              system
                ( ( if os == "linux"
                      then "buf_size=10000000 lualatex"
                      else
                        (mainPath (fullConfig config))
                          ++ "..\\miktex\\miktex\\bin\\xelatex.exe"
                  )
                    ++ " --interaction=batchmode main.tex > /dev/null"
                )
        )
    _ <-
      ( ( if onlyTables config
            then return ()
            else
              system
                ( ( if os == "linux"
                      then "makeindex"
                      else
                        (mainPath (fullConfig config))
                          ++ "..\\miktex\\miktex\\bin\\makeindex.exe"
                  )
                    ++ " main > /dev/null"
                )
                >> return ()
        )
        )
    _ <-
      forM
        ( ( if (onlyTables config)
              || ((outputType (fullConfig config)) /= PlainPDF)
              then []
              else [3, 4]
          ) ::
            [Integer]
        )
        ( \r ->
            do
              if (onlyTables config)
                then return ()
                else myprint (" generating PDF file. LaTeX run " ++ (show r) ++ " of 4")
              system
                ( ( if os == "linux"
                      then "buf_size=10000000 lualatex"
                      else
                        (mainPath (fullConfig config))
                          ++ "..\\miktex\\miktex\\bin\\xelatex.exe"
                  )
                    ++ " --interaction=batchmode main.tex > /dev/null"
                )
        )
    result <-
      if (onlyTables config)
        then do
          _ <- do
            system
              ( ( if os == "linux"
                    then ""
                    else (mainPath (fullConfig config)) ++ "..\\pdftotext\\"
                )
                  ++ "pdftotext main.pdf main.txt"
              )
          te <-
            Control.Exception.catch
              (Tools.readFile ("main.txt"))
              catchFun
          case splitOn "\n" te of
            (x : xs) -> return (pack (encode (intercalate "," (map (strip "pt\r,\f") (x : xs)))))
            _ -> return (pack (encode ""))
        else case outputType (fullConfig config) of
          PlainPDF -> Data.ByteString.readFile "main.pdf"
          ZipArchive -> do
            setCurrentDirectory pathname
            a <-
              addFilesToArchive
                [OptRecursive]
                emptyArchive
                ["document"]
            Data.ByteString.writeFile
              "main.zip"
              (LZ.toStrict (fromArchive a))
            Data.ByteString.readFile "main.zip"
          OdtFile -> do
            setCurrentDirectory ".."
            _ <- system "libreoffice --headless --convert-to odt index.html"
            _ <- system "libreoffice --headless --convert-to html:HTML:EmbedImages index.odt"
            _ <- system "libreoffice --headless --convert-to odt index.html"
            Data.ByteString.readFile "index.odt"
          EPubFile -> do
            _ <- system "ebook-convert ../index.html .epub"
            Data.ByteString.readFile "index.epub"
    setCurrentDirectory cwd
    return result

-- | Get the language code (e.g. en for English) of a HTML document from the
-- lang attribute of its root element. The root element is the html element.
getLang ::
  -- | The URL to the HTML document to determine the language code of.
  URL ->
  -- | The code wrapped into a Just value of the Maybe monad in case it could be
  -- determined, the Nothing value of the Maybe monad otherwise. The whole thing
  -- is wrapped into the IO monad since the Downloading of the URL requires an
  -- IO action.
  IO (Maybe String)
getLang u =
  let theUrl = exportURL u
   in do
        yy <- geturl theUrl
        case (deepGet2 "html" (parseHtml yy)) of
          ((Environment Tag (TagAttr _ m) _) : []) ->
            return $
              Data.Map.Strict.lookup "lang" m
          _ -> return $ Nothing

-- | Function for catching an IO exception when reading a file. It returns the
-- empty string in any case
catchFun ::
  -- | The IO exception that has occured
  IOException ->
  -- | The empty string
  IO String
catchFun _ = return ""

-- | Convert a parse tree section to LaTeX without advancing the state.
toLaTeX ::
  -- | The parse tree element to be converted to LaTeX
  [Anything Char] ->
  -- | The LaTeX representation of the parse tree section given in the first
  -- parameter.
  String
toLaTeX zz = fst (treeToLaTeX3 zz initialState)

-- | Make the list of authors of the images and return this list in LaTeX
-- notation together with the footnotes possibly generated by the parse tree
-- section to LaTeX compilation process.
makeImgAuthorsList ::
  -- | The list of image credits objects containing the information on the
  -- authors in parse tree notation.
  [ImageCredits] ->
  -- | A pair. Its first element is the information about the authors in LaTeX
  -- notation. The second element of the pair is the footnote information
  -- generated by the compilation process of the author information from parse
  -- tree notation to LaTeX notation.
  ([String], String)
makeImgAuthorsList l = let (aut, st) = (runState (mapM treeToLaTeX2 (map theAuthor l)) initialState {MyState.getInTab = 1}) in (aut, intercalate "\n" (Map.elems (footnoteMap st)))

-- | Make the list of figures in LaTeX notation
makeImgList ::
  -- | The information about the authors and licenses and so on of the images
  [ImageCredits] ->
  -- | The list of figures in LaTeX notation.
  String
makeImgList contrib =
  let z =
        Data.List.concat
          (map go (zip3 ([1 ..] :: [Integer]) contrib authors))
   in ((toString latexPreamble) ++ z ++ "\n\\end{longtable}\n" ++ footnotes ++ "\\pagebreak")
  where
    (authors, footnotes) = makeImgAuthorsList contrib
    go (i, crb, aut) =
      "\\href{"
        ++ ( replace2
               ( replace2
                   (Data.List.concat (map chartransforlink ((theDescUrl crb))))
                   "//"
                   "/"
               )
               "https:/"
               "https://"
           )
        ++ "}{"
        ++ (show i)
        ++ "}& "
        ++ aut
        ++ "&"
        ++ (toLaTeX (theLicense crb))
        ++ "\\\\ \\hline \n"

-- | Convert a parse tree section to HTML without advancing the state.
toHtml ::
  -- | The parse tree element to be converted to HTML
  [Anything Char] ->
  -- | The HTML representation of the parse tree section given in the first
  -- parameter.
  String
toHtml l = fst (treeToHtml3 (fromList []) Nothing "" l initialState)

-- | Make the list of figures in HTML notation
makeImgListHTML ::
  -- | The information about the authors and licenses and so on of the images
  [ImageCredits] ->
  -- | The list of figures in HTML notation.
  String
makeImgListHTML imgs =
  do
    let z = Data.List.concat (map go imgs)
     in ( "<table rules=\"all\"><tr><td>Number</td><td>Contributors</td><td>License</td></tr>"
            ++ z
            ++ "</table>"
        )
  where
    go crb =
      "<tr><td><a href=\""
        ++ ( replace2
               (replace2 (theDescUrl crb) "//" "/")
               "https:/"
               "https://"
           )
        ++ "\">"
        ++ (show (imageNumber crb))
        ++ "</a></td><td>"
        ++ (toHtml (theAuthor crb))
        ++ "</td><td>"
        ++ (toHtml (theLicense crb))
        ++ "</td></tr>"

-- | Extract the LaTeX formulas from a parse tree and convert them to PNG
-- images.
makeformulas ::
  -- | The directory where mediawiki2latex was started
  String ->
  -- | The temporary directory
  String ->
  -- | The parse tree where to extract the formulas from
  [Anything Char] ->
  -- | A list of pairs. The first element of each pair is the filename of the
  -- file where the png version of the formula is stored. The second element of
  -- each pair is the width of the image in pixels.
  ImperativeMonad [(String, Int)]
makeformulas p tempdir ll =
  do
    x <- allinfo
    return $ Data.List.concat x
  where
    allinfo :: ImperativeMonad [[(String, Int)]]
    allinfo = mapM processNode ll

    processNode :: Anything Char -> ImperativeMonad [(String, Int)]
    processNode (Environment Math _ l) =
      do
        myname <- return (((hex (show (hash (mathTransform l))))))
        sz <-
          liftIO
            ( do
                Tools.writeFile
                  (tempdir </> (myname ++ ".tex"))
                  ( "\\documentclass{article}\n\\usepackage{color}\n\\input{./defaultcolors}\n\\usepackage{amsfonts}\n\\usepackage{amsmath}\n\\usepackage{amssymb}\n\\begin{document}\n\\thispagestyle{empty}\n$"
                      ++ (mathTransform l)
                      ++ "$\n\\end{document}"
                  )
                Static.writeFiles tempdir headerFiles
                _ <- system ("latex2png " ++ (tempdir </> (myname ++ ".tex")))
                sz <- mysize (tempdir </> (myname ++ ".png"))
                _ <-
                  system
                    ("latex2png -c -d 1200 " ++ (tempdir </> (((myname)) ++ ".tex")))
                myprint (mathTransform l)
                return sz
            )
        return [(myname ++ ".png", sz)]
    processNode (Environment BigMath _ l) =
      do
        myname <- return (((hex (show (hash (mathTransform l))))))
        sz <-
          liftIO
            ( do
                Tools.writeFile
                  (tempdir </> (myname ++ ".tex"))
                  ( "\\documentclass{article}\n\\usepackage{color}\n\\input{./defaultcolors}\n\\usepackage{amsfonts}\n\\usepackage{amsmath}\n\\begin{document}\n\\thispagestyle{empty}\n$"
                      ++ (mathTransform l)
                      ++ "$\n\\end{document}"
                  )
                Static.writeFiles tempdir headerFiles
                _ <- system ("latex2png " ++ (tempdir </> (myname ++ ".tex")))
                sz <- mysize (tempdir </> (myname ++ ".png"))
                _ <-
                  system
                    ("latex2png -c -d 1200 " ++ (tempdir </> (((myname)) ++ ".tex")))
                myprint (mathTransform l)
                return sz
            )
        return [(myname ++ ".png", sz)]
    processNode (Environment _ _ l) = (makeformulas p tempdir l)
    processNode _ = return []

    mysize :: String -> IO Int
    mysize fn =
      do
        _ <-
          system
            ( (getConvert p)
                ++ "-verbose "
                ++ fn
                ++ " -format '%w'  "
                ++ tempdir
                ++ "nullfile.bmp"
                ++ " > "
                ++ tempdir
                ++ "dump2 2> "
                ++ tempdir
                ++ "dump"
            )
        dump <- Tools.readFile (tempdir ++ "dump")
        case reverse
          (Prelude.filter (\x -> (trim x) /= "") (splitOn "\n" dump)) of
          (x : _) -> case splitOn " " x of
            (_ : (_ : (y : _))) -> case splitOn "x" y of
              (z : _) -> case reads z of
                [(ii, _)] -> do
                  return
                    ii
                _ -> return 0
              _ -> return 0
            _ -> return 0
          _ -> return 0

-- | Build the LaTeX source document. Concatenates the header, the body, the
-- list of figures and the footer.
jjoin ::
  -- | The body of the LaTeX document.
  String ->
  -- | The list of figures.
  String ->
  -- | The LaTeX source document.
  String
jjoin theBody listOfFiguers =
  ( (toString (latexHeader))
      ++ theBody
      ++ listOfFiguers
      ++ (toString latexFooter)
  )

-- | Split the list of calculated maximum width of columns into sublists where
-- each sublist represents a single table.
tabgo ::
  -- | The list of the number of columns for each table
  [Int] ->
  -- | The maximum widths of all columns in the whole document
  [ByteString] ->
  -- | The maximum widths of all columns split into sublists. One sublist for
  -- each table.
  [[ByteString]]
tabgo (x : xs) bs = (take x bs) : (tabgo xs (drop x bs))
tabgo [] _ = []

-- | Forks a piece of work (given as an IO action in the first parameter) and
-- marks the a slot (in the list of slots given in the second parameter as use,
-- that means it sets its MVar to contain a False value)
forker ::
  -- | The IO action to be forked.
  IO () ->
  -- | The list of slots. Its length is the number of threads to run in
  -- parallel.
  [MVar Bool] ->
  -- | The IO action forking the thread with the work given in the first
  -- parameter.
  IO ()
forker w (x : xs) = do
  b <- readMVar x
  if b then (swapMVar x False) >> (forkFinally w (\_ -> swapMVar x True >> return ())) >> return () else forker w xs
forker _ _ = error "Error forking subprocess"

-- | Run a list of pieces of work given as list of IO actions in the first
-- parameter. Make sure the number of threads running in parallel is not higher
-- than the length of the list of slots given in the second parameter.
waiter ::
  -- | The list of pieces of work.
  [IO ()] ->
  -- | The list of slots. Its length is the maximum number of threads to run in
  -- parallel.
  [MVar Bool] ->
  -- | The IO action running all pieces of work given in the first parameter but
  -- never exceeding the length of the list given in the first parameter for the
  -- number of threads running at the same time.
  IO ()
waiter work l = do
  threadDelay 1000
  ll <- mapM readMVar l
  if or ll
    then case work of
      (w : ws) -> do
        forker w l
        waiter ws l
      _ -> if and ll then return () else waiter work l
    else waiter work l

-- | Perform a single run of mediawiki2latex.
all ::
  -- | The configuration for the single run of mediawiki2latex.
  FullConfig ->
  -- | The IO action performing the single run of mediawiki2latex.
  ImperativeMonad ()
all cfg =
  do
    liftIO $ myprint " processing started"
    systempdir <- liftIO getTemporaryDirectory
    tempdir <-
      liftIO $
        createTempDirectory systempdir "MediaWiki2LaTeXImages"
    st <- get
    templates <- case runMode cfg of
      UserTemplateFile _ filename -> liftIO (Tools.readFile filename)
      _ -> return userTemplates
    let uurl =
          replace2
            ( if (take 8 (inputUrl cfg)) == "https://"
                then "https://" ++ (drop 8 (inputUrl cfg))
                else
                  if (take 7 (inputUrl cfg)) == "http://"
                    then (inputUrl cfg)
                    else "https://" ++ (inputUrl cfg)
            )
            "_"
            " "
    purl <- parseUrl (escapeURIString isAllowedInURI uurl)
    language <- liftIO $ getLang (UrlAnalyse.url purl)
    put st {fullUrl = purl}
    liftIO $ myprint " downloading article and contributor information"
    text <- load cfg

    liftIO $ myprint " parsing article text"
    theFormulas <-
      if (outputType cfg) `elem` [EPubFile, OdtFile]
        then
          makeformulas
            (mainPath cfg)
            tempdir
            ( printPrepareTree
                False
                ( if ( case (runMode cfg) of
                         HTML Yes -> True
                         UserTemplateFile Yes _ -> True
                         StandardTemplates Yes -> True
                         ExpandedTemplates Yes -> True
                         _ -> False
                     )
                    then case (loadacu st) of Right _ -> (parseit minparsers text); _ -> []
                    else
                      ( parseit
                          (if (runMode cfg) == (HTML No) then minparsers else parsers)
                          text
                      )
                )
            )
        else return []
    liftIO
      ( myprint
          ( " number of bytes to be parsed: "
              ++ (show (Data.List.length text))
          )
      )
    result <-
      Compiler.compile
        (runMode cfg)
        text
        templates
        []
        ""
        Nothing
        (Map.fromList theFormulas)
        ((outputType cfg) `elem` [EPubFile, OdtFile])
        language
        (latexTables cfg)
    liftIO $
      myprint
        " forking threads to download of images and contributor information on them"
    liftIO
      ( myprint
          ( " number of images going to be downloaded: "
              ++ (show (Data.List.length (images result)))
          )
      )
    imageCredits <- getImages1 tempdir (images result) (wikiUrl purl)
    let joined = jjoin (body result) ""
    let theConfig =
          LatexConfig
            { content = joined,
              figures = [],
              ltitle = (makeTitle result purl),
              fullConfig = cfg,
              lhostname = (UrlAnalyse.hostname purl),
              theResult = result,
              onlyTables = True,
              lang = language,
              theTempDir = tempdir,
              formulas = theFormulas,
              figHTML = "",
              theHtmlTables = [],
              theHtmlMapes = [],
              theChessBoardes = []
            }
    liftIO $ myprint " precompiling table columns"
    let cols = (sum (map Data.List.length (tablelist result)))
    liftIO
      (myprint (" number of columns to be compiled: " ++ (show cols)))
    let chunks = (chunksOf 100 (Data.List.concat $ (tablelist result))) :: [[String]]
    mvars <- liftIO ((mapM (\_ -> newEmptyMVar) chunks))
    let work =
          map
            ( \(chun, cc, m) -> do
                myprint (" precompiling column number " ++ (show (1 + 100 * (cc - 1))))
                r <-
                  runLaTeXIOAction
                    theConfig
                      { content =
                          (toString (latexTableHeader))
                            ++ (intercalate "}\\the\\wd\\mybox{},\n\n\\let\\mybox\\undefined\\newsavebox{\\mybox}\\sbox{\\mybox}{" chun)
                            ++ "\n"
                            ++ (toString latexTableFooter)
                      }
                    (show cc)
                putMVar m r
            )
            (zip3 chunks ([1 ..] :: [Integer]) mvars)

    np <- liftIO getNumProcessors
    cores <- liftIO (mapM (\_ -> newMVar True) ([1 .. np] :: [Int]))
    liftIO (waiter work cores)
    ttabs <- liftIO (mapM takeMVar mvars)
    let tabs = tabgo (Prelude.map Data.List.length (tablelist result)) (Prelude.filter (Data.ByteString.Char8.any (== '.')) (Data.ByteString.split (Prelude.head (Data.ByteString.unpack (Data.ByteString.Char8.singleton ','))) (Data.ByteString.pack (Prelude.filter (not . Data.Word8.isSpace) (Data.ByteString.unpack (Data.ByteString.concat ttabs))))))
    liftIO $ myprint " generating LaTeX document"
    liftIO
      ( myprint
          ( " number of bytes to be parsed: "
              ++ (show (Data.List.length text))
          )
      )
    newResult <-
      Compiler.compile
        (runMode cfg)
        text
        templates
        tabs
        (makeTitle2 result purl)
        language
        (Map.fromList theFormulas)
        ((outputType cfg) `elem` [EPubFile, OdtFile])
        language
        (latexTables cfg)
    liftIO $
      myprint
        " joining threads to download the images and contributor information on them"
    liftIO
      ( myprint
          ( " number of images to be processed: "
              ++ (show (Data.List.length (images result)))
          )
      )
    let pp = makeImgList imageCredits
    let pphtml = makeImgListHTML imageCredits
    (contrib, contribHTML) <-
      makeContributors
        (Just (UrlAnalyse.url purl))
    let newContent = jjoin (body newResult) (contrib ++ pp)
    liftIO $ myprint " preparing for PDF generation"
    pdf <-
      runLaTeX
        theConfig
          { onlyTables = False,
            theResult = newResult,
            content = newContent,
            figures = imageCredits,
            figHTML = pphtml ++ contribHTML ++ "</body></html>",
            theHtmlTables = (theHtmlTabs newResult),
            theHtmlMapes = (theHtmlMaps newResult),
            theChessBoardes = (theChessBoards newResult)
          }
    liftIO (Data.ByteString.writeFile (outputFilename cfg) pdf)
    liftIO $ removeDirectoryRecursive tempdir
    liftIO $ myprint " finished"
