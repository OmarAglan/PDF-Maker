{-# LANGUAGE DefaultSignatures, DeriveAnyClass #-}
{-DHUN| module to compile mediawiki page to latex documents. This module is called from the All module.|DHUN-}
module Compiler where
import ImperativeState
import MyState
import MediaWikiParseTree
import MediaWikiParser
import LatexRenderer
import HtmlRenderer
import Tools
import System.FilePath.Posix
import qualified Data.Map as Map
import Data.Set ()
import Control.Monad.State
import UrlAnalyse
import Data.ByteString.UTF8 (toString)
import Data.ByteString as B
       hiding (take, reverse, dropWhile, takeWhile, drop, map, concat,
               elem, length, zip, head, filter, minimum, isInfixOf)
import Data.Serialize as S (encode, decode)
import Hex
import System.Process
import System.IO.Temp
import System.Directory

runCompile :: String -> FullConfig-> ImperativeMonad ()
runCompile dir cfg
  = do t <- liftIO $ Tools.readFile (dir </> "input")
       p <- return $ case runMode cfg of
                       (HTML _) -> minparsers
                       _ -> parsers
       cr <- return (printPrepareTree (parseit p t))
       liftIO $ B.writeFile (dir </> "output") (encode cr)


runNewTree :: String -> ImperativeMonad ()
runNewTree dir 
  = do t <- liftIO $ B.readFile (dir </> "output")
       let tr=(case S.decode t of {Right k->k::[Anything Char];_->[]}) 
       intus <- liftIO $ Tools.readFile (dir </> "intus")
       let us = read intus
       let (nus,ntr) = makeLables tr us
       liftIO $ B.writeFile (dir </> "newtree") (encode ntr)
       liftIO $ B.writeFile (dir </> "us") (encode nus)


runTreeToLaTeX :: String ->String->  ImperativeMonad ()
runTreeToLaTeX instfn dir 
  = do t <- liftIO $ B.readFile (dir </> "newtree")
       let l=(case S.decode t of {Right k->k;_->[]}) 
       inst <- liftIO $ Tools.readFile (instfn </> "inst")
       let st = read inst
       let (ltx,newst) = runState (treeToLaTeX2 l) st
       liftIO $ B.writeFile (dir </> "ltx") (encode (ltx::String))
       liftIO $ B.writeFile (dir </> "st") (encode newst)
       liftIO $ Tools.writeFile (dir </> "inst") (show newst)



{-DHUN| main function to compile mediawiki pages   |DHUN-}

compile ::
        RunMode ->
          String ->
            String ->
              [[ByteString]] ->
                String ->
                  Maybe String ->
                    Map.Map String Int -> Bool -> ImperativeMonad CompileResult
compile theRunMode text templates tabs mytitle mylanguage formulas
  b
  = do st <- get
       case theRunMode of
           StandardTemplates No -> return
                                     (run b mylanguage mytitle (parseit parsers text)
                                       (parseit parsers text)
                                       (hostname . fullUrl $ st)
                                       templates
                                       tabs
                                       formulas)
           UserTemplateFile No _ -> return
                                      (run b mylanguage mytitle (parseit parsers text)
                                        (parseit parsers text)
                                        (hostname . fullUrl $ st)
                                        templates
                                        tabs
                                        formulas)
           HTML No  -> do --liftIO (Tools.writeFile "/home/dirk/dhudhu" (show (printPrepareTree(parseit minparsers text))))
                          return
                           (run b mylanguage mytitle
                            (printPrepareTree (parseit minparsers text))
                            (printPrepareTree (parseit minparsers text))
                            (hostname . fullUrl $ st)
                             templates
                             tabs
                             formulas)
           ExpandedTemplates No -> do return
                                       (run b mylanguage mytitle (parseit parsers text)
                                        (parseit parsers text)
                                        (hostname . fullUrl $ st)
                                        templates
                                        tabs
                                        formulas)
           _ -> do case loadacu st of 
                       Right pt -> return  (run b mylanguage mytitle pt pt  (hostname . fullUrl $ st)   templates tabs  formulas)
                       Left pt -> (runcheap b mylanguage mytitle pt  (hostname . fullUrl $ st)   templates tabs  formulas theRunMode)

{-DHUN| pathname of the temporary directory of the compiler |DHUN-}

dirpref :: [Char]
dirpref = "../tmp/compiler/"

{-DHUN| converts a wiki source document received from mediawiki when requesting it for Special:Export to a parse tree to be converted to LaTeX be treeToLaTeX3. It also signals to compiler.py that the source code was read using the temporary compiler directory |DHUN-}

shortparse :: String -> IO [Anything Char]
shortparse x
  = do Tools.writeFile (dirpref ++ "done") ""
       return (parseit parsers x)

{-DHUN| return a parse tree of a source file. The first argument is the source file. The second argument is a list of command line parameter. If it contains the keyword print. The source file is understood to be the HTML code returned by mediawiki when being requested for the print version of a wiki page, otherwise it is understood to be the wiki source code of a wiki page, that is what you get when issuing a Special:Export request to mediawiki. This function return a parse tree ready to be turned into a latex document by treeToLaTeX3 |DHUN-}

getparse :: String -> [String] -> IO [Anything Char]
getparse x args
  = if ("print" `elem` args) then printparse x else shortparse x

{-DHUN| prepares a HTML document received from mediawiki when requesting it for the print version of a wiki page to a parse tree to be converted to LaTeX be treeToLaTeX3. It also signals to compiler.py that the source code was read using the temporary compiler directory |DHUN-}

printparse :: String -> IO [Anything Char]
printparse x
  = do Tools.writeFile (dirpref ++ "done") ""
       return (printPrepareTree (parseit minparsers x))

{-DHUN| the pathname of the temporary directory |DHUN-}

tmppath :: [Char]
tmppath = "../tmp/"

{-DHUN| the function takes a list of the following format as first input [([table1,column1],width_x),([table1,column2],width_y),...,([table2,column1],width_z),...] parameter. Thats is a list containing the maximum width for each column of each table of the document. The maximum width of the column is the width that the column could have if it was printed on paper of infinite size. So the size without line breaks. The second parameter is the accumulator which should be the empty map when calling this function from outside. The function returns a map mapping tablenumber to a map mapping columnnumbers to maximum columns width. This data structure contains all information needed to make the decisions on the final width of the columns in the document |DHUN-}

maketabmap ::
           [([Int], Double)] ->
             Map.Map Int (Map.Map Int Double) ->
               Map.Map Int (Map.Map Int Double)
maketabmap (x : xs) m
  = case x of
        (t : (s : []), b) -> maketabmap xs (Map.alter (f s b) t m)
        _ -> m
  where f s1 b1 xx
          = case xx of
                Nothing -> Just (Map.singleton s1 b1)
                Just m1 -> Just (Map.insert s1 b1 m1)
maketabmap _ m = m

{-DHUN| prepare the result of maketabmap for further procession. Some indices are offset corrected. Space for the rules of the table is added to the width of the columns DHUN-}

postproctabmap ::
                 (Fractional a, Num k1, Ord k1, Ord a) =>
                 Map.Map k (Map.Map k1 a) -> Map.Map k (Map.Map k1 a)
postproctabmap m = Map.map f m
  where f m1
          = Map.delete 0
              (Map.mapKeys (\ k -> k - 1)
                 (Map.map (\ x -> (x + 12.333748 - (minimum (Map.elems m1)))) m1))

{-DHUN| datatype for the results of a compulation process. The field images contains the strings enlclose in double square brackes in the wiki used for image inclusion. The field body contains the body of the latex document compiled form the wiki documents. The field tablelist contains a list of lists of  bodies of latex document containing the latex source for a single column. Those can be compiled with the propper headers and footers on arbitrary huge paper to determine the maximum reasonable width for a each column in each table of the document which is need for automatic calculation of column widths for the document. The field gallery numbers contain the image numbers of images include in the wiki source inside of galleries. These got a smaller dimension in cm in the final pdf document and can thus be dithers to a small width in pixels. The field title contains the title of the document if a template defining the title was part of the parse wiki source |DHUN-}

data CompileResult = CompileResult{images :: [String],
                                   body :: String, tablelist :: [[String]],
                                   galleryNumbers :: [Integer], title :: String, html :: String}
                   deriving Show

{-DHUN| the first parameter is the parse tree created by get parse of the document currently being processed. the second parameter is the URL under which the document was downloaded. the third parameter is the netloc describing the wiki this page belongs to. The fourth parameter is a mapping file defined by the user for the mapping of mediawiki templates to latex commands. the fifth parameter is a possible parse tree created by precious run the the should be added before the begging of the newly created parse tree. This function writes out all results to temporary files that will be further processed by compiler.py DHUN-}

run ::
    Bool ->
      Maybe String ->
        String ->
          [Anything Char] ->
            [Anything Char] ->
              String ->
                String -> [[ByteString]] -> Map.Map String Int -> CompileResult
run bb mylanguage mytitle parsetree parsetree2 netloc tmpl
  someTables formulas
  = CompileResult{images = img, body = bdy, tablelist = theTables,
                  galleryNumbers = gals, title = tit,
                  html = if bb then trda3 else []}
  where alldata2 g u
          = (treeToLaTeX3 ((snd . newtree $ g))
               initialState{urld = analyseNetloc netloc}{tabmap = u,
                                                         templateMap =
                                                           getUserTemplateMap
                                                             (read tmpl :: [[String]])}{urls =
                                                                                          mUrlState
                                                                                            .
                                                                                            fst .
                                                                                              newtree
                                                                                            $ g})
        alldata3 g u
          = (treeToHtml3 formulas mylanguage mytitle ((snd . newtree $ g))
               initialState{urld = analyseNetloc netloc}{tabmap = u,
                                                         templateMap =
                                                           getUserTemplateMap
                                                             (read tmpl :: [[String]])}{urls =
                                                                                          mUrlState
                                                                                            .
                                                                                            fst .
                                                                                              newtree
                                                                                            $ g})
        newtree g = makeLables g initialUrlState
        tm = (postproctabmap (maketabmap theNewSizes Map.empty))
        (trda, trst) = (alldata2 parsetree tm)
        (trda3, _) = (alldata3 parsetree2 tm)
        img = (map (\ g -> ((replace '\n' ' ' g))) (getImages trst))
        theTables = reverse (tablist trst)
        bdy = doUnicode trda
        gals = getGalleryNumbers trst
        tit = getTitle trst
        
        fun :: ByteString -> Double
        fun x
          = case reads (toString x) of
                [(f, _)] -> f
                _ -> (1000.0)
        theSizes
          = zip [1 ..] (map (\ x -> zip [1 ..] (map fun x)) someTables)
        theNewSizes = concat (map sizeFun theSizes)
        sizeFun (t, k) = map (\ (s, b) -> ([t, s], b)) k



runcheap ::
    Bool ->
      Maybe String ->
        String ->
          [String] ->
              String ->
                String -> [[ByteString]] -> Map.Map String Int -> RunMode ->ImperativeMonad CompileResult
runcheap _ _ _ input netloc tmpl
  someTables _ theRunMode
  = do ntree<-labelit input

                          

       trst<-ttl3 initialState{urld = analyseNetloc netloc}{tabmap = tm,
                                                         templateMap =
                                                           getUserTemplateMap
                                                             (read tmpl :: [[String]])}{urls =
                                                                                          mUrlState
                                                                                            ntree} input
       lis<-mapM treeToLaTeX2ldr input
       let bdy = doUnicode (concat lis)
       let tit = getTitle trst
       let gals = getGalleryNumbers trst
       let theTables = reverse (tablist trst)
       let img = (map (\ g -> ((replace '\n' ' ' g))) (getImages trst))
        
       return CompileResult{images = img, body = bdy, tablelist = theTables,
                  galleryNumbers = gals, title = tit,
                  html = []}
  where 
        labelit g= foldM labelloc initialUrlState g
        labelloc us fn = do _ <- liftIO $ Tools.writeFile (fn </> "intus") (show us)
                            _ <- liftIO $
                                  system
                                   ("mediawiki2latex -x " ++
                                      (Hex.hex (show (fullconfigbase{convert = Just (NewTree fn), runMode= theRunMode}))))
                            t <- liftIO $ B.readFile (fn </> "us")
                            case S.decode t of 
                               Right nus->return (nus::UrlState)
                               _->return us
        treeToLaTeX2ext instfn fn = do _ <-liftIO $
                                        system
                                         ("mediawiki2latex -x " ++
                                            (Hex.hex (show (fullconfigbase{convert = Just (TreeToLaTeX instfn fn), runMode= theRunMode}))))
                                       return ()


        treeToLaTeX2ldr fn =    do ltx <- liftIO $ B.readFile (fn </> "ltx")
                                   case S.decode ltx of 
                                        Right lt->return (lt::String)
                                        _->return []


--        fullttl::String->[String]->MyState
        fullttl instfn g = do let hh = instfn:g
                              _<-mapM (\(x,y) ->treeToLaTeX2ext x y) (zip hh (Prelude.tail hh))
                              stx <- liftIO $ B.readFile ((Prelude.last g) </> "st")
                              case S.decode stx of 
                                  Right nus->return (nus::MyState)
                                  _-> undefined
 
--        ttl3::MyState->[String]->MyState 
        ttl3 st g = do systempdir <- liftIO getTemporaryDirectory
                       tempdir <- liftIO $
                         createTempDirectory systempdir "MediaWiki2LaTeXStateIO"
                       liftIO (Tools.writeFile (tempdir </> "inst") (show st))                                                        
                       sst <- fullttl tempdir g
                       tempdir2 <- liftIO $
                         createTempDirectory systempdir "MediaWiki2LaTeXStateIO"
                       liftIO (Tools.writeFile (tempdir2 </> "inst") (show st{fndict = fndict sst}))                                                        
                       fullttl tempdir2 g

        tm = (postproctabmap (maketabmap theNewSizes Map.empty))
        
        
        fun :: ByteString -> Double
        fun x
          = case reads (toString x) of
                [(f, _)] -> f
                _ -> (1000.0)
        theSizes
          = zip [1 ..] (map (\ x -> zip [1 ..] (map fun x)) someTables)
        theNewSizes = concat (map sizeFun theSizes)
        sizeFun (t, k) = map (\ (s, b) -> ([t, s], b)) k
