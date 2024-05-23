{-DHUN| module to download images form the wiki DHUN-}
module GetImages where
import ImperativeState
import UrlAnalyse
import qualified Data.ByteString as BStr
import Network.URL as URL
import qualified Data.ByteString.UTF8 as UTF8Str
import Data.List.Split (splitOn)
import Control.Monad
import Data.Maybe
import Control.Monad.State
import Data.List
import Tools
import System.FilePath
import System.Process
import Data.Map hiding (map,take,drop,filter)
import MediaWikiParseTree
import MediaWikiParser
import Hex
import HtmlParser (parseHtmlFast)
import SimpleContributors
import Data.Serialize as S (encode, decode)
import Data.ByteString as BStr (writeFile, readFile)
import Control.DeepSeq
import WikiHelper
import GHC.IO.Exception
import qualified Data.Map as Map
modpath2 :: String -> URL -> URL
modpath2 s u
  = u{url_path =
        if p /= [] then p ++ "/File:" ++ s else "/File:" ++ s}
  where pp = (url_path u)
        p = case reverse pp of
                ('/' : xs) -> (reverse xs)
                xs -> (reverse xs)

conv :: URL -> String -> String
conv u s
  = if take 5 s == "http:" then s else
      if take 6 s == "https:" then "https:" ++ (drop 6 s) else
        if (take 2 s) == "//" then "https:" ++ s else
          (replace2 (replace2 
            (exportURL
               u{url_path =
                   case s of
                       ('/' : xs) -> xs
                       _ -> s})
             "%27" "'" )
            "%25"
            "%")

getImageUrl2 :: (String, URL) -> Maybe String
getImageUrl2 (s, u)
  = (getImageUrl "fullImageLink" u s) `mplus`
      (getImageUrl "fullMedia" u s)

getImageUrl3 :: String -> Maybe String
getImageUrl3 s = return s

getImageUrl :: String -> URL -> String -> Maybe String
getImageUrl fi u ss
  = if isInfixOf fil s then
      case splitOn fil s of
          (_ : (y : _)) -> case splitOn theHref y of
                               (_ : (yy : _)) -> case splitOn q yy of
                                                     (z : _) -> Just
                                                                  ((conv u) . UTF8Str.toString $
                                                                     (BStr.pack z))
                                                     _ -> Nothing
                               _ -> Nothing
          _ -> Nothing
      else Nothing
  where s = BStr.unpack (UTF8Str.fromString ss)
        fil = BStr.unpack (UTF8Str.fromString fi)
        theHref = BStr.unpack (UTF8Str.fromString "href=\"")
        q = BStr.unpack (UTF8Str.fromString "\"")

{-DHUN| downloads a single image form the wiki.  It takes the temporary image download directory as first parameter. It takes the WikiUrl of the wiki website currently being processed as second parameter. It takes a tuple as third input parameter. The first element of the tuple is the image number so just an integer that can be used to identify the image uniquely) . The second element of the tupele is image include string of the image from the wiki source, that is the text in between the square brackets as second input parameter. It returns a tuple. The first element of the tuple is a list of urls under which the image may be found on the wiki. The second element of the tuple is the image number as described above. The third element of the tuple is the Url where the description page of the image on the wiki is located DHUN-}




getImagePage1 ::
             String ->
               WikiUrl -> (Integer, String) ->(String,[String],[String])
getImagePage1 dir u (i, ss)
  =   (concat(map go numberedurls), map moo numberedurls, myloadurls)
  where 
        
        numberedurls=(zip [1..] myurls)::[(Integer,String)]
        go (n,myurl) =myurl++"\"\noutput = "++(moo (n,myurl))++"\n"
        moo (n,_) = (dir </>(show i))++"."++(show n)++".html"
        myurls= map (("url = \""++).kds.unify . exportURL . modpath2 ss) (parses u)::[String]
        myloadurls= (map (kds.unify . exportURL . modpath2 ss) (parses u))::[String]
        kds ('h':'t':'t':'p':'s':':':'/':'/':xs)=('h':'t':'t':'p':'s':':':'/':'/':(kds xs))
        kds ('/':'/':xs)='/':(kds xs)
        kds (x:xs) = x:( kds xs)
        kds [] = []


getImagePage2 ::
             String ->
               WikiUrl -> (Integer, String) -> IO (Maybe ([String], Integer, URL,String))
getImagePage2 dir u (i, ss)
  = do l <- (mapM (\k->Tools.readFile (dir </> (show i)++"."++(show k)++".html")) [1..(length (parses u))])
              :: IO [String]
       
       let xx = (map (getImageUrl2) (zip l (parses u))) :: [Maybe String]
       let gg = (zip (parses u) xx) :: [(URL, Maybe String)]
       let yy = (map go gg) :: [[(URL, String)]]
       let zz = (listToMaybe (concat yy)) :: Maybe (URL, String)
       case zz of
           Just (du, x) -> return (Just
                                   (map (unify . exportURL . (modpath2 ss)) (parses u), i,
                                    modpath2 ss du, x))
           _ -> return Nothing
  where go :: (URL, Maybe String) -> [(URL, String)]
        go (uu, Just x) = [(uu, x)]
        go _ = []


{-DHUN| downloads a single image form the wiki.  It takes the temporary image download directory as first parameter. It takes a tuple as second input parameter. The first element of the tuple is the image number so just an integer that can be used to identify the image uniquely) . The second element of the tupele is image include string of the image from the wiki source, that is the text in between the square brackets as second input parameter. It takes the WikiUrl of the wiki websitze currently being processed as thrird parameter. See function getImages in this module for documentation on the returned data type DHUN-}



doImage1 ::
        String -> WikiUrl -> (Integer, String) -> IO (String,[String],[String])
doImage1 dir theWikiUrl img
  = do return (getImagePage1 dir theWikiUrl (fst img, theName))
  where theName
          = case dropWhile (/= ':') (takeWhile (/= '|') (snd img)) of
                (_ : xs) -> replace2 (replace2 xs "%27" "'") "%" "%25"
                _ -> []


doImage2 ::
        String -> WikiUrl -> (Integer, String) -> IO (Maybe String)
doImage2 dir theWikiUrl img
  = do p <- getImagePage2 dir theWikiUrl (fst img, theName)
       case p of
           Just (_, _, _, x) -> return (Just x)
           _ -> return Nothing
 where theName
          = case dropWhile (/= ':') (takeWhile (/= '|') (snd img)) of
                (_ : xs) -> replace2 (replace2 xs "%27" "'") "%" "%25"
                _ -> []




getImgContribUrl ::
           String -> String ->
             IO ()
getImgContribUrl theHost x
  = do _ <-  system
                ("mediawiki2latex -x " ++
                   (Hex.hex (show (fullconfigbase{imgctrburl = Just (x,theHost)}))))
       return ()

getImgContrib ::
           String ->
             IO ()
getImgContrib theFileName
  = do _ <-  system
                ("mediawiki2latex -x " ++
                   (Hex.hex (show (fullconfigbase{ctrb = Just theFileName}))))
       return ()


getContribCallBack ::
                   [Char] -> ImperativeMonad ()
getContribCallBack theFileName
  = do x <- liftIO (Tools.readFile theFileName)
       let ff= (force (parseHtmlFast x))
       let dd
             = (((deepGet "a" "class" "new mw-userlink" ff)
                  ++ (deepGet "a" "class" "mw-userlink" ff)))
                 :: [Anything Char]

       let ll = (filter pre (map go dd))
       let n = (nub ll) :: [(String, String)]
       let out = (map go2 (zip (map (count ll) n) n))::[(String, String, Int)]
       liftIO (BStr.writeFile (theFileName++".out") (S.encode out))
       return ()
  where go :: Anything Char -> (String, String)
        go (Environment Tag (TagAttr _ m) l)
          = ((shallowFlatten (deepFlatten l)), findWithDefault "" "href" m)
        go _ = ("", "")
        go2 (c, (a, h)) = (a, h, c)



runCtrbUrl :: String -> String -> ImperativeMonad ()
runCtrbUrl ctrbfilename theHost
  = do yy<-liftIO (Tools.readFile ctrbfilename)
       let ht = (force (parseHtmlFast yy))
       let aut =fromMaybe [] (getAuthor ht )::[Anything Char]
       let lic = map C (fromMaybe [] (getLicense ht)) ::[Anything Char]
       --liftIO (print lic)
       --liftIO (print aut)
       liftIO (BStr.writeFile (ctrbfilename++".author") (S.encode aut))
       liftIO (BStr.writeFile (ctrbfilename++".license") (S.encode lic))
       let gg = (deepGet "li" "id" "ca-history" ht)
       let theUrl
             = makeUrl4
                 (case gg of
                      ((Environment Tag (TagAttr _ _) l) : []) -> case deepGet2 "a" l of
                                                                      [Environment Tag
                                                                         (TagAttr _ mm) _] -> case
                                                                                                Data.Map.lookup
                                                                                                  "href"
                                                                                                  mm
                                                                                                of
                                                                                                  (Just
                                                                                                     x) -> if (Data.List.take 8 x == "https://") then        (replace2 x "&amp;" "&") else "https://"
                                                                                                             ++
                                                                                                             theHost
                                                                                                               ++
                                                                                                               (replace2
                                                                                                                  x
                                                                                                                  "&amp;"
                                                                                                                  "&")
                                                                                                  _ -> []
                                                                      _ -> []
                      _ -> [])
       liftIO (Tools.writeFile (ctrbfilename++".histurl") theUrl)
       return ()



{-DHUN| main function to download images. It takes the temporary image download directory as first parameter. It takes image include strings of the images from the wiki source, that is the text in between the square brackets as second input parameter. It takes the WikiUrl of the wiki websitze currently being processed as thrird parameter. This function runs as a background process. So it returns a list of empty MVars immediately when being called. They are later on filled with information on the downloaded images including their location in the temporary image download directory. The returend MVars contain ImageInfo values. See description in the module ImperativeState for a detailed description. DHUN-}

            

fullmake ::String->String -> IO ()
fullmake dir s = do _ <- system ("curl --retry-all-errors --retry 3 -s -w \"%{http_code} %{url} %{filename_effective}\\n\" --compressed -K "++(dir </> ("curlrun"++s))++" --parallel -L > "++(dir </> ("curloutput"++s++".1")))
                    mymake dir s 1 

mymake :: String->String->Integer-> IO ()
mymake dir s n = do text <-Tools.readFile (dir </>("curloutput"++s++"."++(show n)))
                    let list=(filter ppred (map (splitOn " ") (splitOn "\n" text)))
                    putStrLn ("Number of Failures "++(show (length list)))
                    Tools.writeFile (dir </>("curlrun"++s++"."++(show(n+1)))) (concat ((map jjoin list)::[String]))
                    _<-if (list==[]) then (return ExitSuccess) else system ("curl --retry-all-errors --retry 3 -s --compressed -K "++(dir</> ("curlrun"++s++"."++(show (n+1))))++" --parallel -L -w \"%{http_code} %{url} %{filename_effective}\\n\"  > "++(dir </> ("curloutput"++s++"."++(show (n+1)))))
                    if ((list==[])) then return () else (mymake dir s (n+1))
  where 
    ppred (x:_) | x=="429" = True
    ppred _ = False
    jjoin:: [String]->String
    jjoin (_:y:z:[]) = "url = \""++y++"\"\noutput = \""++z++"\"\n"  
    jjoin _ = [] 
   
   
            
getImages1 ::
          String -> [String] -> WikiUrl -> ImperativeMonad ([ImageCredits])
getImages1 dir images theWikiUrl
  = do liftIO $
         do let ddir = dir
            let thetheWikiUrl = theWikiUrl
            let iimages = ((zip [1 ..] (map (premap2 . premap) images)))
            helper<-(mapM (doImage1 ddir thetheWikiUrl) iimages)
            let l = map first helper
            let hosts = concat (repeat (map hof (parses theWikiUrl)))
            
            let imgdescfiles = concat (map second helper)
            let imgdescurls = (map (gogo.third) helper)
            myprint (" curl run (1/3)")
            Tools.writeFile (dir </> "curlrun1") (concat l)
            fullmake dir "1"
            res <- (mapM (doImage2 ddir thetheWikiUrl) iimages)
            let cnt =concat (map go (zip ([1..]::[Integer]) res))
            Tools.writeFile (dir </> "curlrun2") (cnt)
            myprint (" curl run (2/3)")
            fullmake dir "2"
            _<-mapM (\(theHost,theFile)->getImgContribUrl theHost theFile) (zip hosts imgdescfiles)
            histories<-mapM fun imgdescfiles
            Tools.writeFile (dir </> "curlrun3") (concat histories)
            myprint (" curl run (3/3)")
            fullmake dir "3"
            authorList<-mapM fun2 imgdescfiles
            mapM (getCredits dir) (zip (makautlist authorList) (zip images (zip ([1..(length images)]) imgdescurls)))
  where
    premap2 x = case splitOn "?lang=" x of
                 (y:_) -> y
                 _->x
    premap x = case splitOn "?page=" x of
                 (y:_) -> y
                 _->x
    lu = (length (parses theWikiUrl))
    makautlist [] = []  
    makautlist xs = (concat(take lu xs)):(makautlist(drop lu xs))
    gogo (x:_) = x
    gogo _ = "" 
    first (x,_,_)=x
    second (_,x,_)=x
    third (_,_,x)=x
    getCredits mydir (author,(wwi,(i,descurl))) = 
      do credits <- mapM (getCredit author (mydir </> (show i))) [1..(length (parses theWikiUrl))]
         return ((joinImageCredits credits){theDescUrl=descurl,wikiFilename = theName wwi, imageNumber = i})
    getCredit ::String->String->Int->IO ImageCredits 
    getCredit auth path j = do aut<- getTheAuthor path j
                               lic<- getTheLicense path j
                               return (ImageCredits {theAltAuthors=auth ,theAuthor=aut, theLicense =lic, theDescUrl="",imageNumber=0,wikiFilename=""})
    getTheAuthor path j = do t<-BStr.readFile (path++"."++(show j)++".html.author")                           
                             let r=(S.decode t) ::Either String [Anything Char]
                             case r of 
                               Right rr -> return (killTree rr)
                               _-> return []
    getTheLicense path j =do t<-BStr.readFile (path++"."++(show j)++".html.license")                           
                             let r=(S.decode t) ::Either String [Anything Char]
                             case r of 
                               Right rr -> return rr
                               _-> return []
                            
    theName wi = case dropWhile (/= ':') (takeWhile (/= '|') wi) of
                  (_ : xs) -> replace2 (replace2 xs "%27" "'") "%" "%25"   
                  _ -> []

    joinImageCredits:: [ImageCredits]->ImageCredits
    joinImageCredits c = let au = intercalate [C ','] (nub((filter (/=[]) (map theAuthor c)))) in ImageCredits{theAuthor=if au==[] then (map C (intercalate "," (nub((filter (/=[]) (map theAltAuthors c)))))) else au,theLicense=intercalate [C ','] (nub(filter (/=[])  (map theLicense c))),theDescUrl="",wikiFilename="",imageNumber=0,theAltAuthors=""}
    
    fun x = do s<-Tools.readFile (x++".histurl")
               if s=="" then return "" else return ("url = \""++s++"\"\noutput = \""++x++".history\"\n")
    fun2 x = do s<-Tools.readFile (x++".histurl")
                if s=="" then return [] else do getImgContrib (x++".history") 
                                                t<-BStr.readFile (x++".history.out")
                                                let r=(S.decode t) ::Either String [(String, String, Int)]
                                                case r of 
                                                  Right xs -> return (intercalate ", " (map atos xs))
                                                  _ -> return []
    atos (x,_,_)=x
    go (number,Just u) = "url = \""++u++"\"\noutput = \""++(dir </> (show number))++"\"\n"
    go _ = ""
    hof u= case url_type u of
                           Absolute h->host h
                           _->""
    
    
    killTree ll = concat (map killNode ll)

    killNode (Environment Tag (TagAttr "table" m) l)
          = case (Map.lookup "class" m) of
               Just "commons-creator-table mw-collapsible mw-collapsed mw-content-ltr" -> case (deepGet2 "th" l) of 
                 (x:_)->[x]
                 _ -> []
               _ -> [Environment Tag (TagAttr "table" m) (killTree l)]
    killNode (Environment Tag (TagAttr "div" m) l) = case  (Map.lookup "style" m) of
                                                                Just "display: none;" -> []
                                                                _-> [Environment Tag (TagAttr "div" m) (killTree l)]
    killNode (Environment x y l)
          = [Environment x y (killTree l)]
    killNode x = [x]

