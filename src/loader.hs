import Load
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
import Compiler (runCompile)
import Tools (replace2)
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
import Network.URI.Encode (encode)


deepGet3 :: [Char] -> [Anything Char] -> [String]
deepGet3 tag ll = concat $ map go ll
  where go (Environment Tag (TagAttr t m) l)
          | t == tag =
            [shallowFlatten l] ++ (deepGet3 tag l)
        go (Environment _ _ l) = (deepGet3 tag l)
        go _ = []

deepGet2 :: [Char] -> [Anything a] -> [Anything a]
deepGet2 tag ll = concat $ map go ll
  where go (Environment Tag (TagAttr t m) l)
          | t == tag =
            [Environment Tag (TagAttr tag m) l] ++ (deepGet2 tag l)
        go (Environment _ _ l) = (deepGet2 tag l)
        go _ = []



main = do a <- getArgs
          cfg<-return  (FullConfig{ImperativeState.headers = Nothing,
                                                      resolution = 300,
                                                      selfTest = Nothing,
                                                      outputFilename = "mediawiki2latex.cache",
                                                      inputUrl = (head a) , runMode = ImperativeState.HTML Yes,
                                                      paper = "A4", vector = False,
                                                      copy = Nothing,
                                                      mainPath =
                                                        "." ++ (if os == "linux" then "" else "\\"),
                                                      server = Nothing,
                                                      outputType = PlainPDF,
                                                      compile = Nothing, imgctrb = Nothing})
          stz <- imperativeStateZero
          (runStateT (runExceptT (loader cfg)) stz)

loader :: FullConfig -> ImperativeMonad ()
loader cfg = do  st<-get
                 purl <- parseUrl (inputUrl cfg)
                 put st{fullUrl = purl}
                 x<-loadHTML st{fullUrl = purl}
                 let y=(printPrepareTree (parseit minparsers x))
                 let z=(deepGet "div" "class" "mw-category-group" y)
                 let v=deepGet3 "a" (deepGet "div" "class" "mw-category-group" y)
                 let a=gogo (gogo (deepGet "div" "class" "mw-category-generated" y))
                 let b= head(reverse (deepGet2 "a" (takeWhile pred a)))
                 --liftIO (print b)
                 liftIO (putStr (concat  (map go v)))
                 case b of  
                   Environment Tag (TagAttr "a" m) ll | (ll== map C "next page") -> case (Map.lookup "href" m) of 
                         (Just h) -> loader cfg{inputUrl="https://en.wikipedia.org"++h}
                         _ -> return ()
                   _ -> return ()

  where
    go x = "mediawiki2latex -k -u https://en.wikipedia.org/wiki/"++(encode x)++" -o "++(encode x)++".pdf \n"
    pred (Environment Tag (TagAttr "div" m) _) | ((Map.lookup "class" m) == Just "mw-content-ltr") = False
    pred _ = True   
    gogo [(Environment Tag _ ll)] = ll
    gogo _ = []
