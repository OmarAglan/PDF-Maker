{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server module, allows mediawiki2latex to run as web server
module Server where

import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans
import Data.ByteString.Lazy hiding
  ( dropWhile,
    pack,
    reverse,
    takeWhile,
  )
import Data.ByteString.UTF8
import Data.List
import Data.Map.Strict
import Data.Maybe
import Data.Text (Text, pack)
import Data.Time.Clock.POSIX
import Happstack.Server hiding (body)
import Hex
import ImperativeState hiding (name)
import System.Directory
import System.Exit
import System.IO
import System.Process hiding (cwd)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal

-- | Convert a string to an HTML element.
mytext ::
  -- | The input string to be converted to an HTML element.
  String ->
  -- | The output HTML element containing the string given as first parameter.
  H.Html
mytext = text . pack

-- | Construct the servers general HTML page embedding a specific sub element to
-- be display in the middle of it.
pageFrame ::
  -- | The specific sub element to be embedded
  H.Html ->
  -- | The general server HTML page with the sub element embedded in it.
  H.Html
pageFrame input =
  H.table
    H.! A.style
      "min-width:100%;border:0px;padding:0;border-spacing:0px 0px;"
    $ do
      H.tr H.! A.style "min-width:100%;border:0px;padding:0" $
        do
          H.td H.! A.style "background-color:#444444" H.! A.colspan "3" $
            do
              H.div
                H.! A.style
                  "color:white;font-size:60;border:20px;font-family:Arial,Helvetica,sans-serif;"
                $ H.b
                $ do mytext "MediaWiki2LaTeX"
      H.tr H.! A.style "min-width:100%;border:0px;padding:0" $
        do
          H.td H.! A.style "background-color:#222222" H.! A.colspan "3"
            $ H.div H.! A.style "padding: 5px;padding-left: 10px"
            $ H.div
              H.! A.style
                "padding:3px;display:inline;background-color:#595959;border-radius:3px"
            $ H.div
              H.! A.style
                "font-family: times, serif;display:inline;font-size:20;color:#cccccc"
            $ "Home"
      H.tr $
        do
          H.td $
            H.table H.! A.style "padding:20px" $
              do
                H.tr $
                  infoBox
                    "MediaWiki to LaTeX"
                    "MediaWiki to LaTeX converts Wiki pages to LaTeX and PDF. It works with any website running MediaWiki, especially Wikipedia and Wikibooks. MediaWiki to LaTeX is purely written in the purely functional language Haskell. It was mainly developed by Dirk H\252nniger. The source code is freely available under the terms of the GNU General Public License. Binary releases for the most common operating systems are available for download. The Debian package is maintained by Georges Khaznadar."
                H.tr $
                  infoBox "Contact" $
                    do
                      mytext "Dirk H\252nniger"
                      H.br
                      mytext "Emil Schweitzer Str. S 10"
                      H.br
                      mytext "D-47506 Neukirchen Vluyn"
                      H.br
                      mytext "Germany"
                      H.br
                      mytext "Telephone ++49-2845-3799993"
                      H.br
                      H.a
                        H.! A.href
                          "mailto:dirk.hunniger@googlemail.com?Subject=MediaWiki2LaTeX"
                        $ "dirk.hunniger@googlemail.com"
          input
          H.td $
            H.table H.! A.style "padding:20px" $
              do
                H.tr $
                  infoBox "Documentation and Links" $
                    do
                      H.ul $
                        do
                          H.li
                            $ H.a
                              H.! A.href
                                "http://de.wikibooks.org/wiki/Benutzer:Dirk_Huenniger/wb2pdf/manual"
                            $ "Users Manual"
                          H.li
                            $ H.a
                              H.! A.href
                                "http://de.wikibooks.org/wiki/Benutzer:Dirk_Huenniger/wb2pdf"
                            $ "Project Wiki Page"
                          H.li $
                            H.div H.! A.style "font-size:x-large" $
                              do
                                H.b
                                  $ H.a
                                    H.! A.href
                                      "https://de.wikibooks.org/wiki/Benutzer:Dirk_Huenniger/wb2pdf"
                                  $ "Download Full Version for Free"
                          H.li
                            $ H.a
                              H.! A.href "http://sourceforge.net/p/wb2pdf/git/ci/master/tree/"
                            $ "Git Sourcecode Repository"
                          H.li
                            $ H.a
                              H.! A.href
                                "http://de.wikibooks.org/wiki/Benutzer_Diskussion:Dirk_Huenniger/wb2pdf/Requests"
                            $ "Bug Tracker"
                H.tr $
                  infoBox "Web Interface" $
                    do
                      mytext
                        "The web interface was artistically designed by Thomas Blume using "
                      H.a H.! A.href "http://www.yaml.org/" $ "YAML"
                      mytext
                        ". It was technically implemented by Dirk H\252nniger using the Blaze Html Framework in Haskell. Important ideas on the design of the user interface were contributed by Georges Khaznadar and Martin Wermers. The server this interface is running on is run by a WMF, who have to cover the costs, so "
                      H.b "please consider downloading the full version for free"
                      mytext
                        " and running at on your own hardware instead of using this server."

-- | construct a HTML info box with a given heading and content
infoBox ::
  -- | The heading of the info box
  String ->
  -- | The content of the info box
  H.Html ->
  -- | The info box containing the heading given in the first parameter as well
  -- as the content given in the second parameter.
  H.Html
infoBox heading ccontent =
  H.td
    $ H.div H.! A.style "padding:10px"
    $ H.div
      H.! A.style
        "text-align:jusitfy;text-justify:inter-word;padding:20px;border-color:#aaaaaa;background-color:#f4f4f4;border-radius:5px;border-width:1px;border-style:solid;font-family:Arial,Helvetica,sans-serif"
    $ do
      H.div H.! A.style "font-size:30" $ H.b (mytext heading)
      H.br
      H.div H.! A.style "text-align:jusitfy" $ ccontent

-- | Datatype to track progress of the background process calculating the result
-- and provide information about the related progress bar shown in the web
-- interface.
data ProgressInfo = ProgressInfo
  { -- | The last relative progress (between zero and one inclusive) that has been reached. Meaning that the command line version of mediawiki2latex processing the document in the background has emitted the progress indicator for the given relative process.
    progress :: Double,
    -- | The output filename wrapped into a Just value of the Maybe monad as
    -- soon as the output has been fully calculated. The Nothing value of the
    -- Maybe monad if the output has not yet been fully calculated or the
    -- calculation of the output has failed.
    filename :: Maybe String,
    -- | The time when the background process to calculate the result has been
    -- started as a UNIX timestamp.
    startTime :: Double,
    -- | The progress shown in the progress bar (between zero and one inclusive)
    -- at the last time the progress bar has been updated
    barValue :: Double,
    -- | The UNIX timestamp when the last progress update to the relative
    -- progress of the background process doing the calculation in the
    -- background has been received.
    lastStepTime :: Double,
    -- | The runtime (in seconds) of the background process calculating the
    -- result, at the last time the progress bar has been updated.
    lastRuntime :: Double,
    -- | The extension for the filename of result file to be calculated
    extension :: String,
    -- | True if the process calculating the result has failed. So no result
    -- could be calculated
    failed :: Bool
  }
  deriving (Show)

instance NFData ProgressInfo where
  rnf a = a `seq` ()

-- | IO action to run the server on a given port
serve ::
  -- | The port to run the server on
  Int ->
  -- | The IO action running the server
  IO ()
serve p =
  do
    a <- newMVar Data.Map.Strict.empty
    simpleHTTP nullConf {port = p, timeout = 100000} $
      msum
        [ dirs "progress" $ path $ \subject -> progressBar a subject,
          dirs "file" $ path $ \subject -> fileFrame a subject,
          dirs "fill" $ path $ \subject -> formPage a subject,
          formPage a ""
        ]

-- | Template for the start page of the server.
template ::
  -- | The title of the page.
  Text ->
  -- | The HTML body of the page.
  H.Html ->
  -- | The HTTP response returning the given title and body.
  Response
template ttitle bbody =
  toResponse $
    H.docTypeHtml $
      do
        H.head $
          do
            H.meta H.! A.charset "utf-8"
            H.title (H.toHtml ttitle)
        H.body $ do bbody

-- | Takes an URL to a wiki article and a filename for the temporary file to be
-- created and return a shell command line parameter to run mediawiki2latex to
-- compile a PDF document from the given URL and write it to the given temporary
-- filename
mainAction :: FullConfig -> IO String
mainAction oldcfg =
  do
    cwd <- getCurrentDirectory
    let cfg = oldcfg {mainPath = cwd}
    return (hex (show cfg))

-- | Takes a given relative progress milestone as Double and returns the next
-- progress milestone as a Double.
getNextRelativeProgress ::
  -- | The list of relative progress milestones.
  [(String, Double)] ->
  -- | The current relative progress milestone.
  Double ->
  -- | The next relative progress milestone.
  Double
getNextRelativeProgress ((_, v) : xs) vv =
  if vv == v
    then case xs of
      (h : _) -> (snd h)
      _ -> v
    else getNextRelativeProgress xs vv
getNextRelativeProgress [] vv = vv

-- | Return a HTTP response containing the progress bar showing the progress of
-- a given compilation process.
progressBar ::
  -- | The MVar containing a map from process identifier (Int) to progress
  -- status information.
  MVar (Map Int ProgressInfo) ->
  -- | The process identifier given as a string to be converted to an integer to
  -- be looked up in the map given in the first parameter .
  String ->
  -- | The HTTP response showing the current progress bar for the given process
  -- identifier.
  ServerPart Response
progressBar t sub =
  do
    let theIndex =
          case (reads (Data.List.takeWhile (/= '.') sub)) :: [(Int, String)] of
            ((k, _) : _) -> k
            _ -> 1
    m <- liftIO (takeMVar t)
    let uu =
          (fromMaybe progressInfoZero (Data.Map.Strict.lookup theIndex m))
    let nextKnownRelativeProgressToBeReached =
          getNextRelativeProgress progressMileStones (progress uu)
    let lastReachedKnownRelativeProgress = progress uu
    let timeOfLastReachedKnownRelativeProgress = lastStepTime uu
    ttime <- liftIO getPOSIXTime
    let time = (realToFrac ttime)
    let expectedRuntime =
          if lastReachedKnownRelativeProgress < 1.0e-2
            then 60.0
            else (time - (startTime uu)) / lastReachedKnownRelativeProgress
    let runtime = time - startTime uu
    let p =
          lastReachedKnownRelativeProgress
            + ( 1
                  - exp
                    ( -(time - timeOfLastReachedKnownRelativeProgress)
                        / ( expectedRuntime
                              * ( nextKnownRelativeProgressToBeReached
                                    - lastReachedKnownRelativeProgress
                                )
                          )
                    )
              )
              * ( nextKnownRelativeProgressToBeReached
                    - lastReachedKnownRelativeProgress
                )
    let oldProgressBarValue = barValue uu
    let progressBarValue =
          oldProgressBarValue
            + ( max
                  ((p - oldProgressBarValue) * (runtime - (lastRuntime uu)) / 5.0)
                  0.0
              )
    let prog =
          if lastReachedKnownRelativeProgress == 1.0
            then 1000
            else
              if failed uu
                then 0
                else round (progressBarValue * 1000.0) :: Integer
    liftIO $
      if not (member theIndex m)
        then putMVar t m
        else
          putMVar t $!!
            ( Data.Map.Strict.insert
                theIndex
                uu {barValue = progressBarValue, lastRuntime = runtime}
                m
            )
    case filename uu of
      Nothing -> do
        method GET
        ok
          $ template "Converting"
          $ pageFrame
          $ infoBox
            ( if not (member theIndex m)
                then "Not enough resources available to process your request! Your request has been dropped! Please download the full version for free and run it on your own computer!"
                else
                  ( if not (failed uu)
                      then "Conversion Running"
                      else "Conversion Failed due to timeout or non zero exit code"
                  )
            )
          $ do
            H.meta H.! A.httpEquiv "refresh" H.! A.content "1"
            H.table $
              do
                H.tr $
                  do
                    H.td $
                      do
                        H.progress
                          H.! A.style wwidth
                          H.! A.value (stringValue (show (prog)))
                          H.! A.max "1000"
                          $ ""
      Just _ -> do
        method GET
        ok $
          template "Conversion Finished" $
            pageFrame $
              infoBox "Conversion Finished. Click on the arrow in the right upper corner of your browser in order to view the result." $
                do
                  H.meta
                    H.! A.httpEquiv "refresh"
                    H.! A.content
                      ( stringValue
                          ( "0;url=/file/"
                              ++ (show theIndex)
                              ++ "."
                              ++ (extension uu)
                          )
                      )
                  H.table $
                    do
                      H.tr $
                        do
                          H.td $
                            do
                              H.progress
                                H.! A.style wwidth
                                H.! A.value (stringValue (show (1000 :: Integer)))
                                H.! A.max "1000"
                                $ ""

-- | Returns HTTP response for a process that has finished. In case the process
-- produced a file, this file is returned in this response. Otherwise an error
-- message is shown.
fileFrame ::
  -- | The MVar containing a map from process identifier (Int) to progress
  -- status information.
  MVar (Map Int ProgressInfo) ->
  -- | The process identifier given as a string to be converted to an integer to
  -- be looked up in the map given in the first parameter.
  String ->
  -- | The HTTP response signaling the conclusion of the process.
  ServerPart Response
fileFrame t sub =
  do
    let theIndex =
          case (reads (Data.List.takeWhile (/= '.') sub)) :: [(Int, String)] of
            ((k, _) : _) -> k
            _ -> 1
    m <- liftIO (takeMVar t)
    liftIO $ putMVar t m
    let uu =
          (fromMaybe progressInfoZero (Data.Map.Strict.lookup theIndex m))
    case filename uu of
      Just x -> do
        f <- serveFile (guessContentTypeM mimeTypes) x
        _ <-
          liftIO
            ( forkIO
                ( do
                    threadDelay 200000000
                    removeFile x
                )
            )
        return f
      Nothing -> do
        method GET
        ok $
          template "Conversion Failed" $
            pageFrame $
              infoBox "Conversion Failed" $
                (mytext "We are sorry the conversion failed, please contact our us")

-- | Calculate the number of rendering processes currently running
currentlyrunning ::
  -- | The MVar containing a map from process identifier (Int) to progress
  -- status information.
  Map Int ProgressInfo ->
  -- | The number of processes which were already started but have not yet
  -- concluded.
  Int
currentlyrunning m =
  sum
    ( Data.List.map
        (fromEnum . ((\x -> (isNothing (filename x)) && (not (failed x)))))
        (Data.Map.Strict.elems m)
    )

-- | The width of the HTML form to request the start a rendering process as a
-- string.
wwidth2 ::
  -- | The HTML string to set the width of the form to request the start of a
  -- rendering process.
  String
wwidth2 = "width:400px"

-- | The width of the HTML form to request the start a rendering process as a
-- HTML attribute.
wwidth ::
  -- | The HTML attribute to set the width of the form to request the start of a
  -- rendering process.
  AttributeValue
wwidth = stringValue wwidth2

-- | The HTTP response showing a form in which the start of a rendering process
-- can be requested.
formPage ::
  -- | The MVar containing a map from process identifier (Int) to progress
  -- status information.
  MVar (Map Int ProgressInfo) ->
  -- | The default entry for the URL in field in the form to be returned.
  String ->
  -- | The HTTP response containing the form to request the start of a rendering
  -- process.
  ServerPart Response
formPage m s =
  do
    decodeBody (defaultBodyPolicy "/tmp/" 0 1000000 1000000)
    msum [viewForm, processForm]
  where
    viewForm :: ServerPart Response
    viewForm =
      do
        method GET
        ok $
          template "MediaWiki2LaTeX" $
            pageFrame $
              infoBox "Create Your PDF" $
                do
                  mytext
                    "To compile MediaWiki pages via LaTeX to PDF choose any URL from "
                  H.a H.! A.href "http://en.wikipedia.org/" $ "Wikipedia"
                  mytext
                    " or any other website running MediaWiki. If you intent to compile a wikibook make sure you use the link to the printable version of the book."
                  H.br
                  H.div H.! A.style "font-size:20" $ H.b $ mytext "Send your request"
                  H.form
                    H.! A.action "/form"
                    H.! A.enctype "multipart/form-data"
                    H.! A.method "POST"
                    $ H.div
                    $ do
                      H.table $
                        do
                          H.tr $
                            do
                              H.td "Full web URL of the book page"
                              H.td $
                                do
                                  H.input
                                    H.! A.style wwidth
                                    H.! A.type_ "text"
                                    H.! A.id "msg"
                                    H.! A.name "msg"
                                    H.! A.value (stringValue s)
                          H.tr $
                            do
                              H.td "Output Format"
                              H.td $
                                do
                                  H.select H.! A.style wwidth H.! A.name "output" $
                                    do
                                      H.option H.! A.value "pdf" $ "PDF"
                                      H.option H.! A.value "zip" $ "LaTeX zip"
                                      H.option H.! A.value "epub" $ "EPUB"
                                      H.option H.! A.value "odt" $
                                        "ODT (Word Processor)"
                          H.tr $
                            do
                              H.td "Template expansion"
                              H.td $
                                do
                                  H.select H.! A.style wwidth H.! A.name "expansion" $
                                    do
                                      H.option H.! A.value "Print" $ "Standard"
                                      H.option H.! A.value "BookMode" $ "Book / Collection"
                                      H.option H.! A.value "BookNoParent" $ "Book Contents Page"
                                      H.option H.! A.value "MediaWiki" $ "Expand Templates by MediaWiki"
                                      H.option H.! A.value "Normal" $ "Expand Templates Internally"
                          H.tr $
                            do
                              H.td "Page size"
                              H.td $
                                do
                                  H.select H.! A.style wwidth H.! A.name "paper" $
                                    do
                                      H.option H.! A.value "A4" $ "A4"
                                      H.option H.! A.value "A5" $ "A5"
                                      H.option H.! A.value "B5" $ "B5"
                                      H.option H.! A.value "letter" $ "Letter"
                                      H.option H.! A.value "legal" $ "Legal"
                                      H.option H.! A.value "executive" $ "Executive"
                          H.tr $
                            do
                              H.td "Table typesetting"
                              H.td $
                                do
                                  H.select H.! A.style wwidth H.! A.name "table" $
                                    do
                                      H.option H.! A.value "Chromium" $ "Chromium"
                                      H.option H.! A.value "LaTeX" $
                                        "LaTeX"
                          H.tr $
                            do
                              H.td "Vector graphics"
                              H.td $
                                do
                                  H.select H.! A.style wwidth H.! A.name "vector" $
                                    do
                                      H.option H.! A.value "Rasterize" $ "Rasterize"
                                      H.option H.! A.value "Keep Vector Form" $
                                        "Keep Vector Form"
                          H.tr $
                            do
                              H.td ""
                              H.td $
                                do
                                  H.input
                                    H.! A.style (stringValue (wwidth2 ++ ";height:60px"))
                                    H.! A.type_ "submit"
                                    H.! A.value "Start!"
                  H.div H.! A.style "text-decoration:underline" $
                    mytext "Please note:"
                  H.br
                  do
                    mytext "The LaTeX source code will be compiled several times to make sure all references are resolved. The whole process will usually take about one minute."
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
    processForm =
      do
        msg <- lookBS "msg"
        paperOpt <- lookBS "paper"
        vectorOpt <- lookBS "vector"
        tableOpt <- lookBS "table"
        expansion <- lookBS "expansion"
        output <- lookBS "output"
        zzz <- liftIO $
          do
            tmpDir <- getTemporaryDirectory
            (name, handle) <-
              openTempFile
                tmpDir
                ( "MediaWiki2LaTeX"
                    ++ if (toString (toStrict output)) == "zip"
                      then "zip"
                      else
                        if (toString (toStrict output)) == "epub"
                          then "epub"
                          else
                            if (toString (toStrict output)) == "odt"
                              then "odt"
                              else "pdf"
                )
            hClose handle >> removeFile name
            act <-
              mainAction
                FullConfig
                  { ltxproc = Nothing,
                    testMode = False,
                    headers = Nothing,
                    resolution = 300,
                    outputFilename = name,
                    inputUrl = (toString (toStrict msg)),
                    runMode =
                      if ( isInfixOf
                             ("Book:" :: [Char])
                             (toString (toStrict msg))
                         )
                        then getRunmode ("BookMode" :: [Char])
                        else getRunmode (toString (toStrict expansion)),
                    paper = (toString (toStrict paperOpt)),
                    vector =
                      (toString (toStrict vectorOpt))
                        == "Keep Vector Form",
                    ImperativeState.copy = Nothing,
                    mainPath = "",
                    server = Nothing,
                    outputType =
                      if (toString (toStrict output)) == "zip"
                        then ImperativeState.ZipArchive
                        else
                          if (toString (toStrict output)) == "epub"
                            then ImperativeState.EPubFile
                            else
                              if (toString (toStrict output)) == "odt"
                                then ImperativeState.OdtFile
                                else ImperativeState.PlainPDF,
                    compile = Nothing,
                    convert = Nothing,
                    noparent = "BookNoParent" == (toString (toStrict expansion)),
                    imgctrburl = Nothing,
                    ctrb = Nothing,
                    latexTables =
                      ( (toString (toStrict tableOpt))
                          == "LaTeX"
                      )
                  }
            yy <- newEmptyMVar
            mm <- takeMVar m
            _ <-
              if ((currentlyrunning mm) <= 3)
                then do
                  _ <- forkIO $
                    do
                      (i, o, e, h) <-
                        runInteractiveCommand
                          ("mediawiki2latex -x " ++ act)
                      tt <- getPOSIXTime
                      let ss =
                            ( if (toString (toStrict output)) == "zip"
                                then "zip"
                                else
                                  if (toString (toStrict output)) == "epub"
                                    then "epub"
                                    else
                                      if (toString (toStrict output)) == "odt"
                                        then "odt"
                                        else "pdf"
                            )
                      zz <- forkProgressDriver o m name ss
                      putMVar yy zz
                      ex h i e ((realToFrac tt) + (4 * 3600.0)) m zz name
                  return ()
                else do
                  putMVar yy (-1)
                  return ()
            putMVar m mm
            zzzz <- takeMVar yy
            return zzzz
        method POST
        ok $
          toResponse $
            template "Redirect" $
              do
                H.meta
                  H.! A.httpEquiv "refresh"
                  H.! A.content
                    ( stringValue
                        ( "0;url=/progress/"
                            ++ (show zzz)
                            ++ "."
                            ++ ( if (toString (toStrict output)) == "zip"
                                   then "html"
                                   else "html"
                               )
                        )
                    )

-- | Communicate with a rendering process of mediawiki2latex. Terminate the
-- rendering process if it runs into timeout by sending it a newline character.
ex ::
  -- | the process handle of the rendering process. It can be checked for the
  -- termination of the rendering process
  ProcessHandle ->
  -- | The handle to the input stream of the rendering process. This is where a
  -- newline character can be send to the rendering process in order to ask it
  -- to terminate.
  Handle ->
  -- | The handle to the error output stream of rendering process. This has to
  -- be read regularly to avoid buffer overflows of the error output stream
  Handle ->
  -- | The UNIX time stamp when the process shall be terminated due to timeout.
  Double ->
  -- | The MVar containing a map from process identifier (Int) to progress
  -- status information.
  MVar (Map Int ProgressInfo) ->
  -- | The process identifier where to look up the progress information in the
  -- map wrapped into an MVar given in the preceding parameter.
  Int ->
  -- | The filename of the output file.
  String ->
  -- | The IO action running the rendering process
  IO ()
ex h i e t m n s =
  do
    tt <- getPOSIXTime
    threadDelay 100
    if (realToFrac tt) > t
      then do
        System.IO.hPutStr i "\n"
        mm <- takeMVar m
        putMVar
          m
          (Data.Map.Strict.update (\x -> Just x {failed = True}) n mm)
      else return ()
    xi <- hIsOpen i
    if xi then hFlush i else return ()
    xo <- hIsOpen e
    _ <- if xo then System.IO.hGetContents e else return ""
    y <- getProcessExitCode h
    case y of
      Just ee ->
        if ExitSuccess == ee
          then do
            threadDelay 3000000
            mm <- takeMVar m
            putMVar
              m
              ( Data.Map.Strict.insertWith
                  ( \new old ->
                      old {progress = progress new, lastStepTime = realToFrac tt}
                  )
                  n
                  progressInfoZero {progress = 1.0}
                  mm
              )
            mmm <- takeMVar m
            putMVar m mmm
            case Data.Map.Strict.lookup n mmm of
              Just yy -> do
                _ <- takeMVar m
                putMVar
                  m
                  (Data.Map.Strict.insert n yy {filename = Just s} mmm)
              _ -> return ()
          else do
            mm <- takeMVar m
            putMVar
              m
              (Data.Map.Strict.update (\x -> Just x {failed = True}) n mm)
      _ -> if (realToFrac tt) > t then return () else ex h i e t m n s

-- | A list of pairs. The first element of pair is the progress information
-- string emitted by the mediawiki2latex command line application as soon a a
-- certain (average) relative progress had been reached. The second element is
-- the reached relative progress as a double between zero and one. Zero an one
-- are included in the possible range.
progressMileStones ::
  -- | The list of progress milestones.
  [([Char], Double)]
progressMileStones =
  [ ( "downloading article and contributor information",
      2.540431143798292e-2
    ),
    ("parsing article text", 5.7625916241286344e-2),
    ( "forking threads to download of images and contributor information on them",
      5.8045207449988465e-2
    ),
    ("precompiling table columns", 8.075814224942594e-2),
    ( "joining threads to download the images and contributor information on them",
      0.37003149457779727
    ),
    ("preparing for PDF generation", 0.5479855803098518),
    ("preparing images for LaTeX document", 0.637605216120732),
    ("generating PDF file. LaTeX run 1 of 4", 0.6911489294291799),
    ("generating PDF file. LaTeX run 2 of 4", 0.7673758195622185),
    ("generating PDF file. LaTeX run 3 of 4", 0.8463397892914045),
    ("generating PDF file. LaTeX run 4 of 4", 0.9231746180088297),
    ("finished", 1.0)
  ]

-- | Wait for the given handle to contain information the can be read from it.
wwait ::
  -- | The handle to wait for .
  Handle ->
  -- | The IO action to wait for the handle.
  IO ()
wwait h =
  do
    b <- hIsEOF h
    if b
      then do
        threadDelay 1000
        wwait h
      else return ()

-- | Read progress information from a rendering process, and update the progress
-- information structure given in the third parameter.
progressDriver ::
  -- | The process identifier to look up the process in the map wrapped into an
  -- MVar given in the third parameter
  Int ->
  -- | The handle to the standard output of rendering process
  Handle ->
  -- | The MVar containing a map from process identifier (Int) to progress
  -- status information.
  MVar (Map Int ProgressInfo) ->
  -- | The filename where the rendering process should store its result.
  String ->
  -- | The IO action to read the progress information.
  IO ()
progressDriver n o t s =
  do
    xo <- hIsOpen o
    threadDelay 100
    tt <- getPOSIXTime
    l <-
      if xo
        then do
          wwait o
          hGetLine o
        else return ""
    case msum
      ( Data.List.map
          (\(k, v) -> if isInfixOf k l then Just v else Nothing)
          progressMileStones
      ) of
      Just x -> do
        m <- takeMVar t
        putMVar
          t
          ( Data.Map.Strict.insertWith
              ( \new old ->
                  old {progress = progress new, lastStepTime = realToFrac tt}
              )
              n
              progressInfoZero {progress = x}
              m
          )
      _ -> return ()
    m <- readMVar t
    case Data.Map.Strict.lookup n m of
      Just yy | (progress yy) == 1.0 ->
        do
          _ <- takeMVar t
          putMVar t (Data.Map.Strict.insert n yy {filename = Just s} m)
      Just yy | (failed yy) -> return ()
      _ -> progressDriver n o t s

-- | The default instance of the progress information structure
progressInfoZero ::
  -- | The default instance of the progress information structure. It indicates
  -- that no progress has been made by the respective process yet.
  ProgressInfo
progressInfoZero =
  ProgressInfo
    { progress = 0.0,
      filename = Nothing,
      startTime = 0.0,
      failed = False,
      barValue = 0.0,
      lastStepTime = 0.0,
      lastRuntime = 0.0,
      extension = ""
    }

-- | Fork a thread that will keep track of the progress a rendering process
-- makes.
forkProgressDriver ::
  -- | The handle to the standard output of the rendering process to be tracked.
  Handle ->
  -- | The MVar containing a map from process identifier (Int) to progress
  -- status information.
  MVar (Map Int ProgressInfo) ->
  -- | The filename where the tracked rendering process will store its result.
  String ->
  -- | The extension of the filename where the tracked rendering process will
  -- store its result
  String ->
  -- | IO action to fork the tracking thread tracking a rendering process and
  -- return the process identifier of the process to be tracked, which can be
  -- used to look up the progress of the rendering process in the data structure
  -- given as second parameter.
  IO Int
forkProgressDriver o t s ext =
  do
    m <- takeMVar t
    tt <- getPOSIXTime
    let mm =
          case (keys m) of
            (x : xs) -> Data.List.maximum (x : xs)
            _ -> 0
    putMVar
      t
      ( Data.Map.Strict.insert
          (mm + 1)
          ProgressInfo
            { progress = 0.0,
              filename = Nothing,
              startTime = realToFrac tt,
              barValue = 0.0,
              lastStepTime = 0.0,
              lastRuntime = 0.0,
              extension = ext,
              failed = False
            }
          m
      )
    _ <- forkIO (progressDriver (mm + 1) o t s)
    return (mm + 1)
