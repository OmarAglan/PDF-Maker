{-# LANGUAGE DefaultSignatures, DeriveAnyClass #-}
{-DHUN| module to compile mediawiki page to latex documents. This module is called from the All module.|DHUN-}
module Compiler where
import Control.Monad
import ImperativeState
import MyState
import MediaWikiParseTree
import MediaWikiParser
import LatexRenderer
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
       ppt <- return $ case runMode cfg of
                         (HTML _) -> printPrepareTree2 
                         _ -> printPrepareTree
                       
       st <- lift get
       cr <- return (ppt (not (latexTable st)) (parseit p t))
       liftIO $ B.writeFile (dir </> "output") (encode cr)


runNewTree :: String -> ImperativeMonad ()
runNewTree dir 
  = do t <- liftIO $ B.readFile (dir </> "output")
       let tr=(case S.decode t of {Right k->k::[Anything Char];_->[]}) 
       intus <- liftIO $ B.readFile (dir </> "intus")
       let us = case decode intus of 
                  Right r -> r
                  _ -> error "Could not read State intus"
       let (nus,ntr) = makeLables tr us
       liftIO $ B.writeFile (dir </> "newtree") (encode ntr)
       liftIO $ B.writeFile (dir </> "us") (encode nus)


runTreeToLaTeX :: String ->String->  ImperativeMonad ()
runTreeToLaTeX instfn dir 
  = do t <- liftIO $ B.readFile (dir </> "newtree")
       let l=(case S.decode t of {Right k->k;_->[]}) 
       inst <- liftIO $ B.readFile (instfn </> "inst")
       let st = case decode inst of 
                  Right r -> r
                  _ -> error "Could not read State inst"
                       
       let (ltx,newst) = runState (treeToLaTeX2 l) st
       liftIO $ B.writeFile (dir </> "ltx") (encode (ltx::String))
       liftIO $ B.writeFile (dir </> "st") (encode newst)
       liftIO $ B.writeFile (dir </> "inst") (encode newst)



{-DHUN| main function to compile mediawiki pages   |DHUN-}

compile ::
        RunMode ->
          String ->
            String ->
              [[ByteString]] ->
                String ->
                  Maybe String ->
                    Map.Map String Int -> Bool -> Maybe String->Bool->ImperativeMonad CompileResult
compile theRunMode text templates tabs mytitle mylanguage fformulas
  b llang latexTabels
  = do st <- get
       case theRunMode of
           StandardTemplates No -> return
                                     (run b mylanguage mytitle (parseit parsers text)
                                       (parseit parsers text)
                                       (hostname . fullUrl $ st)
                                       templates
                                       tabs
                                       fformulas (vectorr st) llang latexTabels)
           UserTemplateFile No _ -> return
                                      (run b mylanguage mytitle (parseit parsers text)
                                        (parseit parsers text)
                                        (hostname . fullUrl $ st)
                                        templates
                                        tabs
                                        fformulas (vectorr st) llang latexTabels)
           HTML No  -> do --liftIO (Tools.writeFile "/home/dirk/dhudhu" (show (printPrepareTree2 (not latexTabels) (parseit minparsers text))))
                          return
                           (run b mylanguage mytitle
                            (printPrepareTree2 (not latexTabels) (parseit minparsers text))
                            (printPrepareTree2 (not latexTabels) (parseit minparsers text))
                            (hostname . fullUrl $ st)
                             templates
                             tabs
                             fformulas (vectorr st) llang latexTabels)
           ExpandedTemplates No -> do return
                                       (run b mylanguage mytitle (parseit parsers text)
                                        (parseit parsers text)
                                        (hostname . fullUrl $ st)
                                        templates
                                        tabs
                                        fformulas (vectorr st) llang latexTabels)
           _ -> do case loadacu st of 
                       Right pt -> return  (run b mylanguage mytitle pt pt  (hostname . fullUrl $ st)   templates tabs  fformulas (vectorr st) llang latexTabels)
                       Left pt -> (runcheap b mylanguage mytitle pt  (hostname . fullUrl $ st)   templates tabs  fformulas theRunMode latexTabels)

{-DHUN| pathname of the temporary directory of the compiler |DHUN-}

dirpref :: [Char]
dirpref = "../tmp/compiler/"

{-DHUN| converts a wiki source document received from mediawiki when requesting it for Special:Export to a parse tree to be converted to LaTeX be treeToLaTeX3. It also signals to compiler.py that the source code was read using the temporary compiler directory |DHUN-}

shortparse :: String -> IO [Anything Char]
shortparse x
  = do Tools.writeFile (dirpref ++ "done") ""
       return (parseit parsers x)

{-DHUN| return a parse tree of a source file. The first argument is the source file. The second argument is a list of command line parameter. If it contains the keyword print. The source file is understood to be the HTML code returned by mediawiki when being requested for the print version of a wiki page, otherwise it is understood to be the wiki source code of a wiki page, that is what you get when issuing a Special:Export request to mediawiki. This function return a parse tree ready to be turned into a latex document by treeToLaTeX3 |DHUN-}
{-
getparse :: String -> [String] -> IO [Anything Char]
getparse x args
  = if ("print" `elem` args) then printparse x else shortparse x
-}
{-DHUN| prepares a HTML document received from mediawiki when requesting it for the print version of a wiki page to a parse tree to be converted to LaTeX be treeToLaTeX3. It also signals to compiler.py that the source code was read using the temporary compiler directory |DHUN-}
{-
printparse :: String -> IO [Anything Char]
printparse x
  = do Tools.writeFile (dirpref ++ "done") ""
       return (printPrepareTree (parseit minparsers x))
-}
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


{-DHUN| the first parameter is the parse tree created by get parse of the document currently being processed. the second parameter is the URL under which the document was downloaded. the third parameter is the netloc describing the wiki this page belongs to. The fourth parameter is a mapping file defined by the user for the mapping of mediawiki templates to latex commands. the fifth parameter is a possible parse tree created by precious run the the should be added before the begging of the newly created parse tree. This function writes out all results to temporary files that will be further processed by compiler.py DHUN-}

run ::
    Bool ->
      Maybe String ->
        String ->
          [Anything Char] ->
            [Anything Char] ->
              String ->
                String -> [[ByteString]] -> Map.Map String Int -> Bool ->Maybe String->Bool->CompileResult
run bb mylanguage mytitle parsetree parsetree2 netloc tmpl
  someTables fformulas vec llang latexTabels
  = CompileResult{images = img, body = bdy, tablelist = theTables,
                  galleryNumbers = gals, title = tit,
                  html = if bb then trda3 else [],theHtmlTabs =htmlTabs, theHtmlMaps=htmlMs, theHtmlColors= htmlCols}
  where alldata2 g u
          = (treeToLaTeX3 ((snd . newtree $ g))
               initialState{urld = analyseNetloc netloc}{langu=llang, tabmap = u,
                                                         templateMap =
                                                           getUserTemplateMap
                                                             (read tmpl :: [[String]])}{urls =
                                                                                          mUrlState
                                                                                            .
                                                                                            fst .
                                                                                              newtree
                                                                                            $ g, latexTabs=latexTabels})
        alldata3 g u
          = (treeToHtml3 fformulas mylanguage mytitle ((snd . newtree $ g))
               initialState{urld = analyseNetloc netloc}{langu=llang,MyState.vector=vec, tabmap = u,
                                                         templateMap =
                                                           getUserTemplateMap
                                                             (read tmpl :: [[String]])}{urls =
                                                                                          mUrlState
                                                                                            .
                                                                                            fst .
                                                                                              newtree
                                                                                            $ g, latexTabs=latexTabels})
        newtree g = makeLables g initialUrlState
        tm = (postproctabmap (maketabmap theNewSizes Map.empty))
        (trda, trst) = (alldata2 parsetree tm)
        (trda3, _) = (alldata3 parsetree2 tm)
        img = (map (\ g -> ((replace '\n' ' ' g))) (getImages trst))
        theTables = reverse (tablist trst)
        bdy = doUnicode trda
        gals = getGalleryNumbers trst
        tit = getTitle trst
        htmlTabs = htmlTables trst
        htmlCols = htmlColors trst
        
        htmlMs = htmlMaps trst
        
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
                String -> [[ByteString]] -> Map.Map String Int -> RunMode ->Bool->ImperativeMonad CompileResult
runcheap _ _ _ input netloc tmpl
  someTables _ theRunMode theLatexTables
  = do ntree<-labelit input

                          

       trst<-ttl3 initialState{ latexTabs=theLatexTables,urld = analyseNetloc netloc}{tabmap = tm,
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
                  html = [],theHtmlTabs=htmlTables trst, theHtmlMaps=htmlMaps trst, theHtmlColors=htmlColors trst}
  where 
        labelit g= foldM labelloc initialUrlState g
        labelloc us fn = do _ <- liftIO $ B.writeFile (fn </> "intus") (encode us)
                            _ <- liftIO $
                                  system
                                   ("mediawiki2latex -x " ++
                                      (Hex.hex (show (fullconfigbase{convert = Just (NewTree fn), runMode= theRunMode,latexTables=theLatexTables}))))
                            t <- liftIO $ B.readFile (fn </> "us")
                            case S.decode t of 
                               Right nus->return (nus::UrlState)
                               _-> error "could not read state nus"
        treeToLaTeX2ext instfn fn = do _ <-liftIO $
                                        system
                                         ("mediawiki2latex -x " ++
                                            (Hex.hex (show (fullconfigbase{convert = Just (TreeToLaTeX instfn fn), runMode= theRunMode,latexTables=theLatexTables}))))
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
                       liftIO (B.writeFile (tempdir </> "inst") (encode st))                                                        
                       sst <- fullttl tempdir g
                       tempdir2 <- liftIO $
                         createTempDirectory systempdir "MediaWiki2LaTeXStateIO"
                       liftIO (B.writeFile (tempdir2 </> "inst") (encode st{fndict = fndict sst}))                                                        
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
