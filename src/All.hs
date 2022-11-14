{-DHUN| imperative part of the programm. The outer flow of the programm is controlled here DHUN-}
module All where
import System.Directory.Tree
import Data.String.HT
import Logger
import Control.Exception
import ImperativeState
import UrlAnalyse hiding (url)
import Load
import Compiler
import GetImages
import Control.Monad.State hiding (join)
import Tools
import Data.ByteString
       hiding (take, reverse, dropWhile, takeWhile, drop, map, concat,
               elem, zip, intercalate)
import System.Directory
import System.IO.Temp
import WikiHelper
import System.Info
import System.Process hiding (cwd)
import Static
import Babel
import Data.List.Split
import MagicStrings
import Codec.Binary.UTF8.String
import Data.Map.Strict hiding (map, take, drop)
import Data.ByteString.UTF8 (toString)
import Data.List
import Data.Maybe
import Data.Char
import MediaWikiParseTree
import qualified Data.Map.Strict as Map hiding (take, drop)
import MediaWikiParser
import SimpleContributors
import UrlAnalyse
import Network.URL
import System.FilePath
import Control.Concurrent.MVar ()
import Data.Hashable
import Hex
import Data.IORef
import System.IO
import Codec.Archive.Zip
import Data.ByteString.Lazy (toStrict)
import System.Exit
import System.IO.Error
import HtmlParser (parseHtml)
import Data.ByteString.Char8 (singleton,any)
import Data.Word8 (isSpace)
import Network.URI

{-DHUN| takes a filename as input parameter and returns the so called normalized extension for it. This is and extension classifying the type of the file while used in this program. Its not necessarily the real extension of the filename. For example .jpeg images will be converted to jpg and so on. DHUN-}

getExtension :: String -> String
getExtension s
  = normalizeExtension2
      (map toLower (reverse . (takeWhile (/= '.')) . reverse $ s))

{-DHUN| returns that name of image magick convert command on the current operating system. It takes the path from which mediawiki2latex was started as input parameter. On Windows convert.exe has to reside in a path relative to it for this to work. On linux it does not matter DHUN-}

getConvert :: FilePath -> String
getConvert p
  = if os == "linux" then convertexe else (getPathPrefix p) ++ convertexe
  where convertexe
          = if os == "linux" then "convert " else "convert.exe "

{-DHUN| generates the information for the titlepage. It takes the result of the wiki text compilation (CompileResult) as first input parameter and the FullWikiUrl to the article as second parameter. It title information is found in the CompileResult it is used, otherise it is generated from the FullWikiUrl DHUN-}

makeTitle :: CompileResult -> FullWikiUrl -> [Char]
makeTitle result fu = theTitle
  where theTitle
          = if (Compiler.title result) == "" then pub ++ tit else
              pub ++ (Compiler.title result)
        pub
          = "\\publishers{" ++
              (concat (map chartrans (UrlAnalyse.hostname fu))) ++ "}\n"
        tit
          = "\\title{" ++
              (concat (map (chartrans) ((removePrintVersion (lemma fu))))) ++
                "}\n"

makeTitle2 :: CompileResult -> FullWikiUrl -> [Char]
makeTitle2 result fu = theTitle
  where theTitle
          = if (Compiler.title result) == "" then tit else
              (Compiler.title result)
        tit = (concat (map (chartrans) ((removePrintVersion (lemma fu)))))

{-DHUN| returns the prefix of the path where addional needed software resides depending on the operation system DHUN-}

getPathPrefix :: FilePath -> String
getPathPrefix p = if os == "linux" then "" else (p ++ "..\\lib\\")

{-DHUN| applied the necessary image processing to a file so that it can be included in a latex document and does not take too much discspace. It takes the path form which mediawiki2latex was started as first input parameter. It takes filename with its exension stiped as second input parameter. It takes the normalized exension of the file as third parameter (see also function getExtension in this module). It takes the maximum resolution that images should have as fourth parameter. It takes the imagenumbers of the images residing in image galleries in the wiki source text as fifth parameter (those images are given a lower absolute with in cm in the pdf document and thus can be dithered to a loweder absolute with in pixels). It takes the image number of the current image as sixth parameter. DHUN-}

runFileMods ::
            FilePath ->
              String ->
                String -> Integer -> [Integer] -> Integer -> String -> IO ()
runFileMods p filenamebase extension theResolution gals imgNumber
  pathname
  = case extension of
        ('s' : ('v' : ('g' : _))) -> do _ <- system
                                               ((getPathPrefix p) ++
                                                  "rsvg-convert -u -o " ++
                                                    pngfilename ++ " -a -w 1250 " ++ filename)
                                        postprocpng pngfilename
                                        _ <- system
                                               ((getPathPrefix p) ++
                                                  "rsvg-convert -u --format=pdf -o " ++
                                                    (filenamebase ++ "." ++ "pdf") ++
                                                      " -a -w 1250 " ++ filename)
                                        return ()
        ('g' : ('i' : ('f' : _))) -> do stdfun
                                        b <- doesFileExist firstpngfilename
                                        if b then copyFile firstpngfilename newfilename else
                                          return ()
                                        postprocpng newfilename
        ('t' : ('i' : ('f' : _))) -> do stdfun
                                        postprocpng newfilename

        ('x' : ('c' : ('f' : _))) -> do stdfun
                                        postprocpng newfilename
        ('j' : ('p' : ('g' : _))) -> postprocjpg filename
        ('p' : ('n' : ('g' : _))) -> postprocpng filename
        ('d' : ('j' : ('v' :('u': _)))) -> do djvupng
                                              b <- doesFileExist firstpngfilename
                                              if b then copyFile firstpngfilename newfilename else
                                                 return ()
        ('w' : ('e' : ('b' :('p': _)))) -> do stdfun
                                              postprocpng newfilename
                                        
        _ -> return ()
  where firstpngfilename
          = (reverse . (drop 4) . reverse $ newfilename) ++ "-0.png"
        newfilename = filenamebase ++ "." ++ (normalizeExtension extension)
        filename = filenamebase ++ "." ++ extension
        stdfun
          = do _ <- system
                      ((getConvert p) ++
                         "\"" ++ filename ++ "\" \"" ++ newfilename ++ "\"")
               return ()

        
        djvupng  = do _ <- system ("ddjvu -format=pdf "++   "\"" ++ filename ++ "\"  \"" ++ filenamebase ++ ".pdf" ++ "\"")
                      _ <- system   ((getConvert p) ++
                         "\"" ++ filenamebase ++ ".pdf" ++ "\" \"" ++ newfilename ++ "\"")
                      return ()
        postprocpng fn
          = do _ <- background fn
               dither fn
               return ()
        postprocjpg fn
          = do _ <- system ((getConvert p) ++ "-verbose " ++ fn ++ " " ++ fn)
               dither fn
               return ()
        background fn
          = system
              ((getConvert p) ++ fn ++ " -background white -flatten " ++ fn)
        pngfilename = filenamebase ++ "." ++ "png"
        
        dither :: String -> IO ()
        dither fn
          = do _ <- system
                      ((getConvert p) ++
                         "-verbose " ++
                           fn ++
                             " -format '%w'  " ++
                               pathname ++
                                 "nullfile.bmp" ++
                                   " > " ++ pathname ++ "dump2 2> " ++ pathname ++ "dump")
               dump <- Tools.readFile (pathname ++ "dump")
               case
                 reverse
                   (Prelude.filter (\ x -> (trim x) /= "") (splitOn "\n" dump))
                 of
                   (x : _) -> case splitOn " " x of
                                  (_ : (_ : (y : _))) -> case splitOn "x" y of
                                                             (z : _) -> case reads z of
                                                                            [(ii,
                                                                              _)] -> do runDither fn
                                                                                          (if
                                                                                             imgNumber
                                                                                               `elem`
                                                                                               gals
                                                                                             then
                                                                                             galleryWidth
                                                                                             else
                                                                                             imageWidth)
                                                                                          ii
                                                                            _ -> return ()
                                                             _ -> return ()
                                  _ -> return ()
                   _ -> return ()
        
        runDither :: String -> Integer -> Integer -> IO ()
        runDither fn newSize oldSize
          = if newSize < oldSize then
              system
                ((getConvert p) ++
                   fn ++ " -resize " ++ (show newSize) ++ " " ++ fn)
                >> return ()
              else return ()
        textWidth = 10.5
        galleryImageWidth = 5.0
        
        centimetersPerInch :: Double
        centimetersPerInch = 2.54
        
        galleryWidth :: Integer
        galleryWidth
          = round
              ((fromIntegral theResolution) * galleryImageWidth /
                 centimetersPerInch)
        
        imageWidth :: Integer
        imageWidth
          = round
              ((fromIntegral theResolution) * textWidth / centimetersPerInch)

{-DHUN| function to write the image files into the temporary directory and modify the files for use in a LaTeX document. It takes the pathname of temporary image download directory as first input parameter. Please not the the temporary image download directory is always different from the temporary directory. It takes the pathname of the directory in which mediawiki2latex was stated as second input parameter. It takes the name of the temporary directory as third input parameter. It takes the result of the image downloading process as fourth input parameter. This structure has a Maybe monad as outer type since the image download may fail. Inside is a 3 element tuple. The first one is the filename of the image on the wiki. The second one is the number under which the image was stored in the temporary image download directory. The third one is a list of possible urls where the image may be found on the wiki. It takes maximum resolution that images should have as  fifth parameter. It takes the list of images number of images residing in galleries as sixth input parameter. DHUN-}

writeFiles ::
           FilePath ->
             FilePath ->
               String -> [Maybe ImageInfo] -> Integer -> [Integer] -> IO ()
writeFiles dir p pathname theImages theResolution gals
  = mapM_ go (Prelude.zip ([1 ..] :: [Integer]) theImages)
  where go (i, Just x)
          = do let filenamebase = (pathname ++ (show i))
               let filename
                     = filenamebase ++ "." ++ (getExtension (wikiFilename x))
               filecontent <- Data.ByteString.readFile
                                (dir </> (show (imageNumber x)))
               Data.ByteString.writeFile filename filecontent
               runFileMods p filenamebase (getExtension (wikiFilename x))
                 theResolution
                 gals
                 i
                 pathname
        go _ = return ()

writeFiles2 :: String -> String -> [(String, Int)] -> IO ()
writeFiles2 tmpdir pathname forms = mapM_ go forms
  where go (x, _)
          = do filecontent <- Data.ByteString.readFile (tmpdir </> x)
               Data.ByteString.writeFile
                 (pathname ++ (Prelude.last (splitOn "/" x)))
                 filecontent

data LatexConfig = LatexConfig{figures :: [Maybe ImageInfo],
                               title :: String, fullConfig :: FullConfig, content :: String,
                               hostname :: String, theResult :: CompileResult, onlyTables :: Bool,
                               lang :: Maybe String, theTempDir :: String,
                               formulas :: [(String, Int)], figHTML :: String}

runLaTeX :: LatexConfig -> ImperativeMonad ByteString
runLaTeX config
  = liftIO
      (withSystemTempDirectory "MediaWiki2LaTeX"
         (runLaTeXCallback config))

runLaTeXCallback :: LatexConfig -> FilePath -> IO ByteString
runLaTeXCallback config pathname
  = do extract pathname
       case (ImperativeState.headers (fullConfig config)) of
           Just x -> do d <- readDirectoryWith Data.ByteString.readFile
                               (x ++ "/./")
                        _ <- writeDirectoryWith Data.ByteString.writeFile
                               d{anchor = pathname </> "document/headers"}
                        return ()
           _ -> return ()
       if os == "linux" then return () else
         do d <- readDirectoryWith Data.ByteString.readFile
                   ((mainPath (fullConfig config)) ++ "../fonts/main/")
            _ <- writeDirectoryWith Data.ByteString.writeFile
                   d{anchor = pathname ++ "/document/"}
            return ()
       Tools.writeFile (pathname ++ "/document/main/main.tex")
         (content config)
       Tools.writeFile (pathname ++ "/document/index.html")
         ((html (theResult config)) ++
            "<h2>List of Figures</h2>" ++ (figHTML config))
       Tools.writeFile (pathname ++ "/document/headers/svg.tex")
         (if vector (fullConfig config) then
            "\\newcommand{\\SVGExtension}{pdf}" else
            "\\newcommand{\\SVGExtension}{png}")
       Tools.writeFile (pathname ++ "/document/headers/title.tex")
         (All.title config)
       Tools.writeFile (pathname ++ "/document/headers/babel.tex")
         (makeBabel (lang config) (All.hostname config))
       Tools.writeFile (pathname ++ "/document/headers/paper.tex")
         ("\\KOMAoption{paper}{" ++ (paper (fullConfig config)) ++ "}")
       if (onlyTables config) then return () else
         do myprint " preparing images for LaTeX document"
            All.writeFiles (theTempDir config) (mainPath (fullConfig config))
              (pathname ++ "/document/images/")
              (figures config)
              (resolution (fullConfig config))
              (galleryNumbers (theResult config))
            All.writeFiles2 (theTempDir config)
              (pathname ++ "/document/formulas/")
              (formulas config)
       cwd <- getCurrentDirectory
       setCurrentDirectory (pathname ++ "/document/main")
       case (ImperativeState.copy (fullConfig config)) of
           Just x -> do d <- readDirectoryWith Data.ByteString.readFile
                               "../../document/"
                        _ <- writeDirectoryWith Data.ByteString.writeFile d{anchor = x}
                        return ()
           _ -> return ()
       _ <- forM
              ((if onlyTables config then [1] else
                  if ((outputType (fullConfig config)) /= PlainPDF) then [] else
                    [1, 2])
                 :: [Integer])
              (\ r ->
                 do if (onlyTables config) then return () else
                      myprint (" generating PDF file. LaTeX run " ++ (show r) ++ " of 4")
                    mysystem
                      ((if os == "linux" then "buf_size=10000000 xelatex" else
                          (mainPath (fullConfig config)) ++
                            "..\\miktex\\miktex\\bin\\xelatex.exe")
                         ++ " --interaction=batchmode main.tex"))
       _ <- ((if onlyTables config then return () else
                mysystem
                  ((if os == "linux" then "makeindex" else
                      (mainPath (fullConfig config)) ++
                        "..\\miktex\\miktex\\bin\\makeindex.exe")
                     ++ " main")
                  >> return ()))
       _ <- forM
              ((if
                  (onlyTables config) ||
                    ((outputType (fullConfig config)) /= PlainPDF)
                  then [] else [3, 4])
                 :: [Integer])
              (\ r ->
                 do if (onlyTables config) then return () else
                      myprint (" generating PDF file. LaTeX run " ++ (show r) ++ " of 4")
                    mysystem
                      ((if os == "linux" then "buf_size=10000000 xelatex" else
                          (mainPath (fullConfig config)) ++
                            "..\\miktex\\miktex\\bin\\xelatex.exe")
                         ++ " --interaction=batchmode main.tex"))
       result <- if (onlyTables config) then
                   do _ <- do mysystem
                               ((if os == "linux" then "" else
                                 (mainPath (fullConfig config)) ++ "..\\pdftotext\\")
                                  ++ "pdftotext main.pdf main.txt")
                      te <- Control.Exception.catch (Tools.readFile ("main.txt"))
                              catchFun
                      case splitOn "\n" te of
                          (x : xs) -> return (pack (encode (intercalate "," (map (strip "pt\r,\f") (x:xs)))))
                          _ -> return (pack (encode ""))
                   else
                   case outputType (fullConfig config) of
                       PlainPDF -> Data.ByteString.readFile "main.pdf"
                       ZipArchive -> do setCurrentDirectory pathname
                                        a <- addFilesToArchive [OptRecursive] emptyArchive
                                               ["document"]
                                        Data.ByteString.writeFile "main.zip"
                                          (toStrict (fromArchive a))
                                        Data.ByteString.readFile "main.zip"
                       OdtFile -> do setCurrentDirectory ".."
                                     mysystem "libreoffice --headless --convert-to odt index.html"
                                     mysystem "libreoffice --headless --convert-to html:HTML:EmbedImages index.odt"
                                     mysystem "libreoffice --headless --convert-to odt index.html"
                                     Data.ByteString.readFile "index.odt"
                       EPubFile -> do mysystem "ebook-convert ../index.html .epub"
                                      Data.ByteString.readFile "index.epub"
       setCurrentDirectory cwd
       return result

ex :: ProcessHandle -> Handle -> Handle -> IO ()
ex h o e
  = do b <- hReady stdin
       if b then
         do terminateProcess h
            exitFailure
         else do return ()
       v <- tryIOError (hWaitForInput e 0)
       let xo
             = case v of
                   Right j -> j
                   _ -> False
       _ <- if xo then hGetChar e else return ' '
       w <- tryIOError (hWaitForInput o 0)
       let yo
             = case w of
                   Right j -> j
                   _ -> False
       _ <- if yo then hGetChar o else return ' '
       y <- getProcessExitCode h
       case y of
           Just _ -> do hClose o
                        hClose e
                        return ()
           _ -> ex h o e

mysystem :: String -> IO ()
mysystem x
  = if os == "linux" then
      do (_, o, e, h) <- runInteractiveCommand x
         ex h o e
         return ()
      else
      do _ <- system x
         return ()

getLang :: URL -> IO (Maybe String)
getLang u
  = let theUrl = exportURL u in
      do yy <- geturl theUrl
         case (deepGet2 "html" (parseHtml yy)) of
             ((Environment Tag (TagAttr _ m) _) : []) -> return $
                                                           Data.Map.Strict.lookup "lang" m
             _ -> return $ Nothing

catchFun :: IOException -> IO String
catchFun _ = return ""

strip :: (Eq a) => [a] -> [a] -> [a]
strip l = reverse . (dropWhile isBad) . reverse . dropWhile isBad
  where isBad x = x `elem` l

latexPostamble :: String
latexPostamble = "\n\\end{longtable}\n\\pagebreak"

runCtrb :: String -> ImperativeMonad ()
runCtrb dir
  = do t <- liftIO $ Tools.readFile (dir </> "input")
       cr <- imgContribback ((read t) :: (Maybe ImageInfo))
       liftIO $ Tools.writeFile (dir </> "output") (show cr)

imgContrib ::
           (Maybe ImageInfo) ->
             ImperativeMonad ((Maybe (String, Maybe String)))
imgContrib x
  = do systempdir <- liftIO getTemporaryDirectory
       tempdir <- liftIO $
                    createTempDirectory systempdir "MediaWiki2LaTeXImgContrib"
       liftIO $ Tools.writeFile (tempdir </> "input") (show x)
       _ <- liftIO $
              system
                ("mediawiki2latex -x " ++
                   (Hex.hex (show (fullconfigbase{imgctrb = Just tempdir}))))
       t <- liftIO $ Tools.readFile (tempdir </> "output")
       return (read t)

imgContribback ::
               (Maybe ImageInfo) ->
                 ImperativeMonad ((Maybe (String, Maybe String)))
imgContribback z
  = do x <- return z
       xx <- imgContrib2 x
       liftIO (go xx)
  where go (Just xxx) = return (Just xxx)
        go _ = return (Just ("", Nothing))

imgContrib2 ::
            Maybe ImageInfo -> ImperativeMonad ((Maybe (String, Maybe String)))
imgContrib2 (Just x)
  = do img <- getContributors (contributorUrls x)
       ffi <- liftIO $ (return . fst) img
       fif <- liftIO ((return . ffun . contribsum) ffi)
       ssn <- liftIO $ (return . snd) img
       sns <- liftIO ((return . msum) ssn)
       liftIO ((fun fif sns))
  where ffun :: Map String Contributor -> String
        ffun i = intercalate ", " (keys (i))
        
        fun :: String -> Maybe String -> IO (Maybe (String, Maybe String))
        fun fi sn = return (Just (fi, sn))
imgContrib2 _ = do liftIO (return Nothing)

makeImgList :: [(Maybe ImageInfo)] -> ImperativeMonad String
makeImgList imgs2
  = do ccontrib <- mapM (imgContrib) imgs2
       cccontrib <- liftIO (mapM (return . id) ccontrib)
       contrib <- liftIO (return cccontrib)
       let imgs = imgs2
       let z = concat
                 (map go (zip (zip ([1 ..] :: [Integer]) contrib) imgs))
       return ((toString latexPreamble) ++ z ++ (latexPostamble))
  where go ((i, Just (con, lic)), Just info)
          = "\\href{" ++
              (replace2
                 (replace2
                    (concat (map chartransforlink (exportURL (descriptionUrl info))))
                    "//"
                    "/")
                 "https:/"
                 "https://")
                ++
                "}{" ++
                  (show i) ++
                    "}& " ++ con ++ "&" ++ (fromMaybe "" lic) ++ "\\\\ \\hline \n"
        go (((i, _), _)) = (show i) ++ "&&\\\\ \\hline \n"

makeImgListHTML :: [(Maybe ImageInfo)] -> ImperativeMonad String
makeImgListHTML imgs2
  = do ccontrib <- mapM (imgContrib) imgs2
       cccontrib <- liftIO (mapM (return . id) ccontrib)
       contrib <- return cccontrib
       imgs <- liftIO (return imgs2)
       let z = concat
                 (map go (zip (zip ([1 ..] :: [Integer]) contrib) imgs))
       return
         ("<table rules=\"all\"><tr><td>Number</td><td>Contributors</td><td>License</td></tr>"
            ++ z ++ "</table>")
  where go ((i, Just (con, lic)), Just info)
          = "<tr><td><a href=\"" ++
              (replace2 (replace2 (((exportURL (descriptionUrl info)))) "//" "/")
                 "https:/"
                 "https://")
                ++
                "\">" ++
                  (show i) ++
                    "</a></td><td>" ++
                      con ++ "</td><td>" ++ (fromMaybe "" lic) ++ "</td></tr>"
        go (((i, _), _)) = (show i) ++ "&&\\\\ \\hline \n"

makeformulas ::
             String ->
               String -> [Anything Char] -> ImperativeMonad [(String, Int)]
makeformulas p tempdir ll
  = do x <- allinfo
       return $ concat x
  where allinfo :: ImperativeMonad [[(String, Int)]]
        allinfo = mapM processNode ll
        
        processNode :: Anything Char -> ImperativeMonad [(String, Int)]
        processNode (Environment Math _ l)
          = do myname <- return (((hex (show (hash (mathTransform l))))))
               sz <- liftIO
                       (do Tools.writeFile (tempdir </> (myname ++ ".tex"))
                             ("\\documentclass{article}\n\\usepackage{color}\n\\input{./defaultcolors}\n\\usepackage{amsfonts}\n\\usepackage{amsmath}\n\\usepackage{amssymb}\n\\begin{document}\n\\thispagestyle{empty}\n$"
                                ++ (mathTransform l) ++ "$\n\\end{document}")
                           Static.writeFiles tempdir headerFiles
                           mysystem ("latex2png " ++ (tempdir </> (myname ++ ".tex")))
                           sz <- mysize (tempdir </> (myname ++ ".png"))
                           mysystem
                             ("latex2png -c -d 1200 " ++ (tempdir </> (((myname)) ++ ".tex")))
                           print (mathTransform l)
                           return sz)
               return [(myname ++ ".png", sz)]
        processNode (Environment BigMath _ l)
          = do myname <- return (((hex (show (hash (mathTransform l))))))
               sz <- liftIO
                       (do Tools.writeFile (tempdir </> (myname ++ ".tex"))
                             ("\\documentclass{article}\n\\usepackage{color}\n\\input{./defaultcolors}\n\\usepackage{amsfonts}\n\\usepackage{amsmath}\n\\begin{document}\n\\thispagestyle{empty}\n$"
                                ++ (mathTransform l) ++ "$\n\\end{document}")
                           Static.writeFiles tempdir headerFiles
                           mysystem ("latex2png " ++ (tempdir </> (myname ++ ".tex")))
                           sz <- mysize (tempdir </> (myname ++ ".png"))
                           mysystem
                             ("latex2png -c -d 1200 " ++ (tempdir </> (((myname)) ++ ".tex")))
                           print (mathTransform l)
                           return sz)
               return [(myname ++ ".png", sz)]
        processNode (Environment _ _ l) = (makeformulas p tempdir l)
        processNode _ = return []
        
        mysize :: String -> IO Int
        mysize fn
          = do _ <- system
                      ((getConvert p) ++
                         "-verbose " ++
                           fn ++
                             " -format '%w'  " ++
                               tempdir ++
                                 "nullfile.bmp" ++
                                   " > " ++ tempdir ++ "dump2 2> " ++ tempdir ++ "dump")
               dump <- Tools.readFile (tempdir ++ "dump")
               case
                 reverse
                   (Prelude.filter (\ x -> (trim x) /= "") (splitOn "\n" dump))
                 of
                   (x : _) -> case splitOn " " x of
                                  (_ : (_ : (y : _))) -> case splitOn "x" y of
                                                             (z : _) -> case reads z of
                                                                            [(ii, _)] -> do return
                                                                                              ii
                                                                            _ -> return 0
                                                             _ -> return 0
                                  _ -> return 0
                   _ -> return 0

jjoin :: String -> String -> String
jjoin theBody listOfFiguers
  = ((toString (latexHeader)) ++
       theBody ++ listOfFiguers ++ (toString latexFooter))


tabgo :: [Int] -> [ByteString] -> [[ByteString]]
tabgo (x:xs) bs = (take x bs):(tabgo xs (drop x bs))
tabgo [] _ = []

all :: FullConfig -> ImperativeMonad ()
all cfg
  = do liftIO $ myprint " processing started"
       systempdir <- liftIO getTemporaryDirectory
       tempdir <- liftIO $
                    createTempDirectory systempdir "MediaWiki2LaTeXImages"
       st <- get
       templates <- case runMode cfg of
                        UserTemplateFile _ filename -> liftIO (Tools.readFile filename)
                        _ -> return userTemplates
       let uurl
             = replace2
                 (if (take 8 (inputUrl cfg)) == "https://" then
                    "https://" ++ (drop 8 (inputUrl cfg)) else
                    if (take 7 (inputUrl cfg)) == "http://" then (inputUrl cfg) else
                      "https://" ++ (inputUrl cfg))
                 "_"
                 " "
       purl <- parseUrl (escapeURIString isAllowedInURI uurl)
       language <- liftIO $ getLang (UrlAnalyse.url purl)
       put st{fullUrl = purl}
       minInit
       liftIO $ myprint " downloading article and contributor information"
       text <- load cfg
       



       liftIO $ myprint " parsing article text"
       theFormulas <- if (outputType cfg) `elem` [EPubFile, OdtFile] then
                        makeformulas (mainPath cfg) tempdir
                          (printPrepareTree (if (case (runMode cfg) of
                                                   HTML Yes -> True
                                                   UserTemplateFile Yes _ -> True
                                                   StandardTemplates Yes -> True
                                                   ExpandedTemplates Yes -> True 
                                                   _ -> False )then case (loadacu st) of {Right _->(parseit minparsers text);_->[]}else
                             (parseit (if (runMode cfg) == (HTML No) then minparsers else parsers)
                                text)))
                        else return []
       liftIO $ print (theFormulas)
       liftIO $ print (loadacu st)
       liftIO
         (myprint
            (" number of bytes to be parsed: " ++
               (show (Data.List.length text))))
       result <- Compiler.compile (runMode cfg) text templates [] ""
                   Nothing
                   (Map.fromList theFormulas)
                   ((outputType cfg) `elem` [EPubFile, OdtFile])
       liftIO $
         myprint
           " forking threads to download of images and contributor information on them"
       liftIO
         (myprint
            (" number of images going to be downloaded: " ++
               (show (Data.List.length (images result)))))
       theImages <- getImages tempdir (images result) (wikiUrl purl)
       let joined = jjoin (body result) ""
       let theConfig
             = LatexConfig{content = joined, figures = [],
                           All.title = (makeTitle result purl), fullConfig = cfg,
                           All.hostname = (UrlAnalyse.hostname purl), theResult = result,
                           onlyTables = True, lang = language, theTempDir = tempdir,
                           formulas = theFormulas, figHTML = ""}
       liftIO $ myprint " precompiling table columns"
       let cols = (sum (map Data.List.length (tablelist result)))
       ior <- liftIO (newIORef (0 :: Integer))
       liftIO
         (myprint (" number of columns to be compiled: " ++ (show cols)))

       ttabs <-          do liftIO (modifyIORef ior (+ 1))
                            cc <- liftIO (readIORef ior)
                            liftIO (myprint (" precompiling column number " ++ (show cc)))
                            runLaTeX
                              theConfig{content =
                                          (toString (latexTableHeader)) ++
                                            (intercalate "}\\the\\wd\\mybox{},\n\n\\let\\mybox\\undefined\\newsavebox{\\mybox}\\sbox{\\mybox}{" (concat (tablelist result))) ++"\n"++ (toString latexTableFooter)}
       let tabs = tabgo (Prelude.map Data.List.length (tablelist result)) (Prelude.filter (Data.ByteString.Char8.any (=='.')) (Data.ByteString.split (Prelude.head (Data.ByteString.unpack  (Data.ByteString.Char8.singleton ','))) (Data.ByteString.pack (Prelude.filter (not . Data.Word8.isSpace) (Data.ByteString.unpack ttabs)))))
       liftIO $ myprint " generating LaTeX document"
       liftIO
         (myprint
            (" number of bytes to be parsed: " ++
               (show (Data.List.length text))))
       newResult <- Compiler.compile (runMode cfg) text templates tabs
                      (makeTitle2 result purl)
                      language
                      (Map.fromList theFormulas)
                      ((outputType cfg) `elem` [EPubFile, OdtFile])
       liftIO $
         myprint
           " joining threads to download the images and contributor information on them"
       liftIO
         (myprint
            (" number of images to be processed: " ++
               (show (Data.List.length (images result)))))
       pp <- makeImgList theImages
       pphtml <- makeImgListHTML theImages
       (contrib, contribHTML) <- makeContributors
                                   (Just (UrlAnalyse.url purl))
       let newContent = jjoin (body newResult) (contrib ++ pp)
       thetheImages <- liftIO $
                         do ii <- return theImages
                            return ii
       liftIO $ myprint " preparing for PDF generation"
       pdf <- runLaTeX
                theConfig{onlyTables = False, theResult = newResult,
                          content = newContent, figures = thetheImages,
                          figHTML = pphtml ++ contribHTML ++ "</body></html>"}
       liftIO (Data.ByteString.writeFile (outputFilename cfg) pdf)
       liftIO $ removeDirectoryRecursive tempdir
       liftIO $ myprint " finished"
