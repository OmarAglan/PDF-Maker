{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-DHUN| Server module, allows mediawiki2latex to run as webserver DHUN-}
module Server where
import Data.Text (Text)
import Happstack.Server hiding (body)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.Directory
import System.IO
import Control.Monad.Trans
import Data.ByteString.UTF8
import Data.ByteString.Lazy
       hiding (pack, reverse, takeWhile, dropWhile)
import Control.Concurrent
import Control.Monad.State
import ImperativeState hiding (name)
import Hex
import Data.Map.Strict
import Data.Maybe
import System.Process hiding (cwd)
import Data.List
import Text.Blaze.Internal
import Data.Time.Clock.POSIX
import Data.Text (pack)
import Control.DeepSeq
import System.Exit

mytext :: String -> H.Html
mytext = text . pack

pageFrame :: H.Html -> H.Html
pageFrame input
  = H.table H.!
      A.style
        "min-width:100%;border:0px;padding:0;border-spacing:0px 0px;"
      $
      do H.tr H.! A.style "min-width:100%;border:0px;padding:0" $
           do H.td H.! A.style "background-color:#444444" H.! A.colspan "3" $
                do H.div H.!
                     A.style
                       "color:white;font-size:60;border:20px;font-family:Arial,Helvetica,sans-serif;"
                     $ H.b $ do mytext "MediaWiki2LaTeX"
         H.tr H.! A.style "min-width:100%;border:0px;padding:0" $
           do H.td H.! A.style "background-color:#222222" H.! A.colspan "3" $
                H.div H.! A.style "padding: 5px;padding-left: 10px" $
                  H.div H.!
                    A.style
                      "padding:3px;display:inline;background-color:#595959;border-radius:3px"
                    $
                    H.div H.!
                      A.style
                        "font-family: times, serif;display:inline;font-size:20;color:#cccccc"
                      $ "Home"
         H.tr $
           do H.td $
                H.table H.! A.style "padding:20px" $
                  do H.tr $
                       infoBox "MediaWiki to LaTeX"
                         "MediaWiki to LaTeX converts Wiki pages to LaTeX and PDF. It works with any website running MediaWiki, especially Wikipedia and Wikibooks. MediaWiki to LaTeX is purely written in the purely functional language Haskell. It was mainly devolved by Dirk H\252nniger. The source code is freely available under the terms of the GNU General Public License. Binary releases for the most common operating systems are available for download. The Debian package is maintained by Georges Khaznadar."
                     H.tr $
                       infoBox "Contact" $
                         do mytext "Dirk H\252nniger"
                            H.br
                            mytext "Emil Schweitzer Str. S 10"
                            H.br
                            mytext "D-47506 Neukirchen Vluyn"
                            H.br
                            mytext "Germany"
                            H.br
                            mytext "Telephone ++49-2845-3799993"
                            H.br
                            H.a H.!
                              A.href
                                "mailto:dirk.hunniger@googlemail.com?Subject=MediaWiki2LaTeX"
                              $ "dirk.hunniger@googlemail.com"
              input
              H.td $
                H.table H.! A.style "padding:20px" $
                  do H.tr $
                       infoBox "Documentation and Links" $
                         do H.ul $
                              do H.li $
                                   H.a H.!
                                     A.href
                                       "http://de.wikibooks.org/wiki/Benutzer:Dirk_Huenniger/wb2pdf/manual"
                                     $ "Users Manual"
                                 H.li $
                                   H.a H.!
                                     A.href
                                       "http://de.wikibooks.org/wiki/Benutzer:Dirk_Huenniger/wb2pdf"
                                     $ "Project Wiki Page"
                                 H.li $
                                   H.div H.! A.style "font-size:x-large" $
                                     do H.b $
                                          H.a H.!
                                            A.href
                                              "https://de.wikibooks.org/wiki/Benutzer:Dirk_Huenniger/wb2pdf"
                                            $ "Download Full Version for Free"
                                 H.li $
                                   H.a H.!
                                     A.href "http://sourceforge.net/p/wb2pdf/git/ci/master/tree/"
                                     $ "Git Sourcecode Repository"
                                 H.li $
                                   H.a H.!
                                     A.href
                                       "http://de.wikibooks.org/wiki/Benutzer_Diskussion:Dirk_Huenniger/wb2pdf/Requests"
                                     $ "Bug Tracker"
                     H.tr $
                       infoBox "Web Interface" $
                         do mytext
                              "The web interface was artistically designed by Thomas Blume using "
                            H.a H.! A.href "http://www.yaml.org/" $ "YAML"
                            mytext
                              ". It was technically implemented by Dirk H\252nniger using the Blaze Html Framework in Haskell. Important ideas on the design of the user interface were contributed by Georges Khaznadar and Martin Wermers. The server this interface is running on is run by a WMF, who have to cover the costs, so "
                            H.b "please consider downloading the full version for free"
                            mytext
                              " and running at on your own hardware instead of using this server."

infoBox :: String -> H.Html -> H.Html
infoBox heading content
  = H.td $
      H.div H.! A.style "padding:10px" $
        H.div H.!
          A.style
            "text-align:jusitfy;text-justify:inter-word;padding:20px;border-color:#aaaaaa;background-color:#f4f4f4;border-radius:5px;border-width:1px;border-style:solid;font-family:Arial,Helvetica,sans-serif"
          $
          do H.div H.! A.style "font-size:30" $ H.b (mytext heading)
             H.br
             H.div H.! A.style "text-align:jusitfy" $ content

data ProgressInfo = ProgressInfo{progress :: Double,
                                 filename :: Maybe String, startTime :: Double, barValue :: Double,
                                 lastStepTime :: Double, lastRuntime :: Double, extension :: String,
                                 failed :: Bool}
  deriving (Show)
instance NFData ProgressInfo where
        rnf a = a `seq` ()

{-DHUN| IO action to run the server DHUN-}

serve :: Int -> IO ()
serve p
  = do a <- newMVar Data.Map.Strict.empty
       simpleHTTP nullConf{port = p, timeout = 100000} $
         msum
           [dirs "progress" $ path $ \ subject -> progressBar a subject,
            dirs "file" $ path $ \ subject -> fileFrame a subject,
            dirs "fill" $ path $ \ subject -> formPage a subject,
            formPage a ""]

{-DHUN| template for the start page of the server DHUN-}

template :: Text -> H.Html -> Response
template title body
  = toResponse $
      H.docTypeHtml $
        do H.head $
             do H.meta H.! A.charset "utf-8"
                H.title (H.toHtml title)
           H.body $ do body

{-DHUN| takes an url to a wiki article and a filename for the temporary file to be created and return a shell command to run mediawiki2latex to compile a pdf document from the given url and write it to the given temporary filename DHUN-}

mainAction :: FullConfig -> IO String
mainAction oldcfg
  = do cwd <- getCurrentDirectory
       let cfg = oldcfg{mainPath = cwd}
       return (hex (show cfg))

{-DHUN| main webpage of the server containing a from with an entry for an url to a wiki article with returns the pdf on submission of the form DHUN-}

gogo :: Eq b => [(a, b)] -> b -> b
gogo ((_, v) : xs) vv
  = if vv == v then
      case xs of
          (h : _) -> (snd h)
          _ -> v
      else gogo xs vv
gogo [] vv = vv

progressBar ::
            MVar (Map Int ProgressInfo) -> String -> ServerPart Response
progressBar t sub
  = do let theIndex
             = case
                 (reads (Data.List.takeWhile (/= '.') sub)) :: [(Int, String)] of
                   ((k, _) : _) -> k
                   _ -> 1
       m <- liftIO (takeMVar t)
       let uu
             = (fromMaybe progressInfoZero (Data.Map.Strict.lookup theIndex m))
       let nextKnownRelativeProgressToBeReached
             = gogo mylist (progress uu)
       let lastReachedKnownRelativeProgress = progress uu
       let timeOfLastReachedKnownRelativeProgress = lastStepTime uu
       ttime <- liftIO getPOSIXTime
       let time = (realToFrac ttime)
       let expectedRuntime
             = if lastReachedKnownRelativeProgress < 1.0e-2 then 60.0 else
                 (time - (startTime uu)) / lastReachedKnownRelativeProgress
       let runtime = time - startTime uu
       let p = lastReachedKnownRelativeProgress +
                 (1 -
                    exp
                      (-(time - timeOfLastReachedKnownRelativeProgress) /
                          (expectedRuntime *
                             (nextKnownRelativeProgressToBeReached -
                                lastReachedKnownRelativeProgress))))
                   *
                   (nextKnownRelativeProgressToBeReached -
                      lastReachedKnownRelativeProgress)
       let oldProgressBarValue = barValue uu
       let progressBarValue
             = oldProgressBarValue +
                 (max
                    ((p - oldProgressBarValue) * (runtime - (lastRuntime uu)) / 5.0)
                    0.0)
       let prog
             = if lastReachedKnownRelativeProgress == 1.0 then 1000 else
                 if failed uu then 0 else
                   round (progressBarValue * 1000.0) :: Integer
       liftIO $
         if not (member theIndex m) then putMVar t m else
           putMVar t $!!
             (Data.Map.Strict.insert theIndex
                uu{barValue = progressBarValue, lastRuntime = runtime}
                m)
       case filename uu of
           Nothing -> do method GET
                         ok $
                           template "Converting" $
                             pageFrame $
                               infoBox
                                 (if not (member theIndex m) then
                                    "Not enough resources available to process your request! Your request has been dropped! Please download the full version for free and run it on your own computer!"
                                    else
                                    (if not (failed uu) then "Conversion Running" else
                                       "Conversion Failed due to timeout or non zero exit code"))
                                 $
                                 do H.meta H.! A.httpEquiv "refresh" H.! A.content "1"
                                    H.table $
                                      do H.tr $
                                           do H.td $
                                                do H.progress H.! A.style wwidth H.!
                                                     A.value (stringValue (show (prog)))
                                                     H.! A.max "1000"
                                                     $ ""
           Just _ -> do method GET
                        ok $
                          template "Conversion Finished" $
                            pageFrame $
                              infoBox "Conversion Finished. Click on the arrow in the right upper corner of your browser in order to view the result." $
                                do H.meta H.! A.httpEquiv "refresh" H.!
                                     A.content
                                       (stringValue
                                          ("0;url=/file/" ++
                                             (show theIndex) ++ "." ++ (extension uu)))
                                   H.table $
                                     do H.tr $
                                          do H.td $
                                               do H.progress H.! A.style wwidth H.!
                                                    A.value (stringValue (show (1000 :: Integer)))
                                                    H.! A.max "1000"
                                                    $ ""

fileFrame ::
          MVar (Map Int ProgressInfo) -> String -> ServerPart Response
fileFrame t sub
  = do let theIndex
             = case
                 (reads (Data.List.takeWhile (/= '.') sub)) :: [(Int, String)] of
                   ((k, _) : _) -> k
                   _ -> 1
       m <- liftIO (takeMVar t)
       liftIO $ putMVar t m
       let uu
             = (fromMaybe progressInfoZero (Data.Map.Strict.lookup theIndex m))
       case filename uu of
           Just x -> do f <- serveFile (guessContentTypeM mimeTypes) x
                        _ <- liftIO
                               (forkIO
                                  (do threadDelay 200000000
                                      removeFile x))
                        return f
           Nothing -> do method GET
                         ok $
                           template "Conversion Failed" $
                             pageFrame $
                               infoBox "Conversion Failed" $
                                 (mytext "We are sorry the converion failed, please contact our us")

currentlyrunning :: Map Int ProgressInfo -> Int
currentlyrunning m
  = sum
      (Data.List.map (fromEnum.((\ x -> (isNothing (filename x)) && (not (failed x)))))
         (Data.Map.Strict.elems m))

wwidth2 :: [Char]
wwidth2 = "width:400px"

wwidth :: AttributeValue
wwidth = stringValue wwidth2

formPage ::
         MVar (Map Int ProgressInfo) -> String -> ServerPart Response
formPage m s
  = do decodeBody (defaultBodyPolicy "/tmp/" 0 1000000 1000000)
       msum [viewForm, processForm]
  where viewForm :: ServerPart Response
        viewForm
          = do method GET
               ok $
                 template "MediaWiki2LaTeX" $
                   pageFrame $
                     infoBox "Create Your PDF" $
                       do mytext
                            "To compile MediaWiki pages via LaTeX to PDF choose any URL from "
                          H.a H.! A.href "http://en.wikipedia.org/" $ "Wikipedia"
                          mytext
                            " or any other website running MediaWiki. If you intent to compile a wikibook make sure you use the link to the printable version of the book."
                          H.br
                          H.div H.! A.style "font-size:20" $ H.b $ mytext "Send your request"
                          H.form H.! A.action "/form" H.! A.enctype "multipart/form-data" H.!
                            A.method "POST"
                            $
                            H.div $
                              do H.table $
                                   do H.tr $
                                        do H.td "Full web URL of the book page"
                                           H.td $
                                             do H.input H.! A.style wwidth H.! A.type_ "text" H.!
                                                  A.id "msg"
                                                  H.! A.name "msg"
                                                  H.! A.value (stringValue s)
                                      H.tr $
                                        do H.td "Output Format"
                                           H.td $
                                             do H.select H.! A.style wwidth H.! A.name "output" $
                                                  do H.option H.! A.value "pdf" $ "PDF"
                                                     H.option H.! A.value "zip" $ "LaTeX zip"
                                                     H.option H.! A.value "epub" $ "EPUB"
                                                     H.option H.! A.value "odt" $
                                                       "ODT (Word Processor)"
                                      H.tr $
                                        do H.td "Template expansion"
                                           H.td $
                                             do H.select H.! A.style wwidth H.! A.name "expansion" $
                                                  do H.option H.! A.value "Print" $ "Standard"
                                                     H.option H.! A.value "BookMode" $ "Book / Collection"
                                                     H.option H.! A.value "BookNoParent" $ "Book Contents Page"
                                                     H.option H.! A.value "MediaWiki" $ "Expand Templates by MediaWiki"
                                                     H.option H.! A.value "Normal" $ "Expand Templates Internally"
                                      H.tr $
                                        do H.td "Page size"
                                           H.td $
                                             do H.select H.! A.style wwidth H.! A.name "paper" $
                                                  do H.option H.! A.value "A4" $ "A4"
                                                     H.option H.! A.value "A5" $ "A5"
                                                     H.option H.! A.value "B5" $ "B5"
                                                     H.option H.! A.value "letter" $ "Letter"
                                                     H.option H.! A.value "legal" $ "Legal"
                                                     H.option H.! A.value "executive" $ "Executive"
                                      H.tr $
                                        do H.td "Table typesetting"
                                           H.td $
                                             do H.select H.! A.style wwidth H.! A.name "table" $
                                                  do H.option H.! A.value "Chromium" $ "Chromium"
                                                     H.option H.! A.value "LaTeX" $
                                                       "LaTeX"
                                      H.tr $
                                        do H.td "Vector graphics"
                                           H.td $
                                             do H.select H.! A.style wwidth H.! A.name "vector" $
                                                  do H.option H.! A.value "Rasterize" $ "Rasterize"
                                                     H.option H.! A.value "Keep Vector Form" $
                                                       "Keep Vector Form"
                                      H.tr $
                                        do H.td ""
                                           H.td $
                                             do H.input H.!
                                                  A.style (stringValue (wwidth2 ++ ";height:60px"))
                                                  H.! A.type_ "submit"
                                                  H.! A.value "Start!"
                          H.div H.! A.style "text-decoration:underline" $
                            mytext "Please note:"
                          H.br
                          do mytext "The LaTeX source code will be compiled several times to make sure all references are resolved. The whole process will usually take about one minute." 
                             H.br
                             H.br
                             H.div H.! A.style "font-size:large" $ H.b "There is a time limit of four hours (≈ 2000 pages in PDF) on this server!"
                             H.br
                             mytext "Requests taking longer will be terminated and a \"Conversion Failed due to timeout or non zero exit code\" message will be displayed. There is no limit in the downloadable version of the software, see link on right." 

        getRunmode "Print" = ImperativeState.HTML ImperativeState.No
        getRunmode "MediaWiki" = ImperativeState.ExpandedTemplates ImperativeState.No
        getRunmode "Normal" = ImperativeState.StandardTemplates ImperativeState.No
        getRunmode "BookMode" = ImperativeState.HTML ImperativeState.Yes 
        getRunmode "BookNoParent" = ImperativeState.HTML ImperativeState.Yes 

        getRunmode _ = ImperativeState.HTML ImperativeState.No
        
        processForm :: ServerPart Response
        processForm
          = do msg <- lookBS "msg"
               paperOpt <- lookBS "paper"
               vectorOpt <- lookBS "vector"
               tableOpt <- lookBS "table"
               expansion <- lookBS "expansion"
               output <- lookBS "output"
               zzz <- liftIO $
                        do tmpDir <- getTemporaryDirectory
                           (name, handle) <- openTempFile tmpDir
                                               ("MediaWiki2LaTeX" ++
                                                  if (toString (toStrict output)) == "zip" then
                                                    "zip" else
                                                    if (toString (toStrict output)) == "epub" then
                                                      "epub" else
                                                      if (toString (toStrict output)) == "odt" then
                                                        "odt" else "pdf")
                           hClose handle >> removeFile name
                           act <- mainAction
                                    FullConfig{selfTest = Nothing, headers = Nothing,
                                               resolution = 300, outputFilename = name,
                                               inputUrl = (toString (toStrict msg)),
                                               runMode =
                                                 if
                                                   (isInfixOf ("Book:" :: [Char])
                                                      (toString (toStrict msg)))
                                                   then getRunmode ("BookMode" :: [Char]) else
                                                   getRunmode (toString (toStrict expansion)),
                                               paper = (toString (toStrict paperOpt)),
                                               vector =
                                                 (toString (toStrict vectorOpt)) ==
                                                   "Keep Vector Form",
                                               ImperativeState.copy = Nothing, mainPath = "",
                                               server = Nothing,
                                               outputType =
                                                 if (toString (toStrict output)) == "zip" then
                                                   ImperativeState.ZipArchive else
                                                   if (toString (toStrict output)) == "epub" then
                                                     ImperativeState.EPubFile else
                                                     if (toString (toStrict output)) == "odt" then
                                                       ImperativeState.OdtFile else
                                                       ImperativeState.PlainPDF,
                                               compile = Nothing, convert=Nothing, noparent= "BookNoParent" == (toString (toStrict expansion)), imgctrburl=Nothing,ctrb=Nothing,latexTables = ((toString (toStrict tableOpt)) ==
                                                   "LaTeX" )}
                           yy <- newEmptyMVar
                           mm <- takeMVar m
                           _ <- if ((currentlyrunning mm)<=3) then
                                  do _ <- forkIO $
                                            do (i, o, e, h) <- runInteractiveCommand
                                                                 ("mediawiki2latex -x " ++ act)
                                               tt <- getPOSIXTime
                                               let ss
                                                     = (if (toString (toStrict output)) == "zip"
                                                          then "zip" else
                                                          if (toString (toStrict output)) == "epub"
                                                            then "epub" else
                                                            if (toString (toStrict output)) == "odt"
                                                              then "odt" else "pdf")
                                               zz <- forkProgressDriver o m name ss
                                               putMVar yy zz
                                               ex h i e ((realToFrac tt) + (4*3600.0)) m zz ss
                                     return ()
                                  else
                                  do putMVar yy (-1)
                                     return ()
                           putMVar m mm
                           zzzz <- takeMVar yy
                           return zzzz
               method POST
               ok $
                 toResponse $
                   template "Redirect" $
                     do H.meta H.! A.httpEquiv "refresh" H.!
                          A.content
                            (stringValue
                               ("0;url=/progress/" ++
                                  (show zzz) ++
                                    "." ++
                                      (if (toString (toStrict output)) == "zip" then "html" else
                                         "html")))

ex ::
   ProcessHandle ->
     Handle ->
       Handle ->
         Double -> MVar (Map Int ProgressInfo) -> Int -> String -> IO ()
ex h i e t m n s
  = do tt <- getPOSIXTime
       threadDelay 100
       if (realToFrac tt) > t then
         do System.IO.hPutStr i "\n"
            mm <- takeMVar m
            putMVar m
              (Data.Map.Strict.update (\ x -> Just x{failed = True}) n mm)
         else return ()
       xi <- hIsOpen i
       if xi then hFlush i else return ()
       xo <- hIsOpen e
       _ <- if xo then System.IO.hGetContents e else return ""
       y <- getProcessExitCode h
       case y of
           Just ee -> if ExitSuccess == ee then
                        do threadDelay 3000000
                           mm <- takeMVar m
                           putMVar m
                             (Data.Map.Strict.insertWith
                                (\ new old ->
                                   old{progress = progress new, lastStepTime = realToFrac tt})
                                n
                                progressInfoZero{progress = 1.0}
                                mm)
                           mmm <- takeMVar m
                           putMVar m mmm
                           case Data.Map.Strict.lookup n mmm of
                               Just yy -> do _ <- takeMVar m
                                             putMVar m
                                               (Data.Map.Strict.insert n yy{filename = Just s} mmm)
                               _ -> return ()
                        else
                        do mm <- takeMVar m
                           putMVar m
                             (Data.Map.Strict.update (\ x -> Just x{failed = True}) n mm)
           _ -> if (realToFrac tt) > t then return () else ex h i e t m n s

mylist :: [([Char], Double)]
mylist
  = [("downloading article and contributor information",
      2.540431143798292e-2),
     ("parsing article text", 5.7625916241286344e-2),
     ("forking threads to download of images and contributor information on them",
      5.8045207449988465e-2),
     ("precompiling table columns", 8.075814224942594e-2),
     ("joining threads to download the images and contributor information on them",
      0.37003149457779727),
     ("preparing for PDF generation", 0.5479855803098518),
     ("preparing images for LaTeX document", 0.637605216120732),
     ("generating PDF file. LaTeX run 1 of 4", 0.6911489294291799),
     ("generating PDF file. LaTeX run 2 of 4", 0.7673758195622185),
     ("generating PDF file. LaTeX run 3 of 4", 0.8463397892914045),
     ("generating PDF file. LaTeX run 4 of 4", 0.9231746180088297),
     ("finished", 1.0)]

wwait :: Handle -> IO ()
wwait h
  = do b <- hIsEOF h
       if b then
         do threadDelay 1000
            wwait h
         else return ()

progressDriver ::
               Int -> Handle -> MVar (Map Int ProgressInfo) -> String -> IO ()
progressDriver n o t s
  = do xo <- hIsOpen o
       threadDelay 100
       tt <- getPOSIXTime
       l <- if xo then
              do wwait o
                 hGetLine o
              else return ""
       case
         msum
           (Data.List.map
              (\ (k, v) -> if isInfixOf k l then Just v else Nothing)
              mylist)
         of
           Just x -> do m <- takeMVar t
                        putMVar t
                          (Data.Map.Strict.insertWith
                             (\ new old ->
                                old{progress = progress new, lastStepTime = realToFrac tt})
                             n
                             progressInfoZero{progress = x}
                             m)
           _ -> return ()
       m <- readMVar t
       case Data.Map.Strict.lookup n m of
           Just yy | (progress yy) == 1.0 ->
                     do _ <- takeMVar t
                        putMVar t (Data.Map.Strict.insert n yy{filename = Just s} m)
           Just yy | (failed yy) -> return ()
           _ -> progressDriver n o t s

progressInfoZero :: ProgressInfo
progressInfoZero
  = ProgressInfo{progress = 0.0, filename = Nothing, startTime = 0.0,
                 failed = False, barValue = 0.0, lastStepTime = 0.0,
                 lastRuntime = 0.0, extension = ""}

forkProgressDriver ::
                   Handle -> MVar (Map Int ProgressInfo) -> String -> String -> IO Int
forkProgressDriver o t s ext
  = do m <- takeMVar t
       tt <- getPOSIXTime
       let mm
             = case (keys m) of
                   (x : xs) -> Data.List.maximum (x : xs)
                   _ -> 0
       putMVar t
         (Data.Map.Strict.insert (mm + 1)
            ProgressInfo{progress = 0.0, filename = Nothing,
                         startTime = realToFrac tt, barValue = 0.0, lastStepTime = 0.0,
                         lastRuntime = 0.0, extension = ext, failed = False}
            m)
       _ <- forkIO (progressDriver (mm + 1) o t s)
       return (mm + 1)
