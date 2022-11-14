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
          replace2
            (exportURL
               u{url_path =
                   case s of
                       ('/' : xs) -> xs
                       _ -> s})
            "%25"
            "%"

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

getImagePage ::
             String ->
               WikiUrl -> (Integer, String) -> IO (Maybe ([String], Integer, URL))
getImagePage dir u (i, ss)
  = do l <- (mapM (geturl . kds.unify . exportURL . modpath2 ss)
               (parses u))
              :: IO [String]
       let xx = (map (getImageUrl2) (zip l (parses u))) :: [Maybe String]
       let gg = (zip (parses u) xx) :: [(URL, Maybe String)]
       let yy = (map go gg) :: [[(URL, String)]]
       let zz = (listToMaybe (concat yy)) :: Maybe (URL, String)
       case zz of
           Just (du, x) -> do img <- (geturl2 x) :: (IO BStr.ByteString)
                              BStr.writeFile (dir </> (show i)) img
                              return
                                (Just
                                   (map (unify . exportURL . (modpath2 ss)) (parses u), i,
                                    modpath2 ss du))
           _ -> return Nothing
  where go :: (URL, Maybe String) -> [(URL, String)]
        go (uu, Just x) = [(uu, x)]
        go _ = []
        kds ('h':'t':'t':'p':'s':':':'/':'/':xs)=('h':'t':'t':'p':'s':':':'/':'/':(kds xs))
        kds ('/':'/':xs)='/':(kds xs)
        kds (x:xs) = x:( kds xs)
        kds [] = []

{-DHUN| downloads a single image form the wiki.  It takes the temporary image download directory as first parameter. It takes a tuple as second input parameter. The first element of the tuple is the image number so just an integer that can be used to identify the image uniquely) . The second element of the tupele is image include string of the image from the wiki source, that is the text in between the square brackets as second input parameter. It takes the WikiUrl of the wiki websitze currently being processed as thrird parameter. See function getImages in this module for documentation on the returned data type DHUN-}

doImage ::
        String -> WikiUrl -> (Integer, String) -> IO (Maybe ImageInfo)
doImage dir theWikiUrl img
  = do myprint (show img)
       p <- getImagePage dir theWikiUrl (fst img, theName)
       case p of
           Just (u, pp, du) -> return
                                 (Just
                                    ImageInfo{wikiFilename = theName, imageNumber = pp,
                                              contributorUrls = u, descriptionUrl = du})
           _ -> return Nothing
  where theName
          = case dropWhile (/= ':') (takeWhile (/= '|') (snd img)) of
                (_ : xs) -> replace2 xs "%" "%25"
                _ -> []

{-DHUN| main function to download images. It takes the temporary image download directory as first parameter. It takes image include strings of the images from the wiki source, that is the text in between the square brackets as second input parameter. It takes the WikiUrl of the wiki websitze currently being processed as thrird parameter. This function runs as a background process. So it returns a list of empty MVars immediately when being called. They are later on filled with information on the downloaded images including their location in the temporary image download directory. The returend MVars contain ImageInfo values. See description in the module ImperativeState for a detailed description. DHUN-}

getImages ::
          String -> [String] -> WikiUrl -> ImperativeMonad [Maybe ImageInfo]
getImages dir images theWikiUrl
  = do liftIO $
         do let ddir = dir
            let thetheWikiUrl = theWikiUrl
            let iimages = ((zip [1 ..] images))
            (mapM (doImage ddir thetheWikiUrl) iimages)
