{-DHUN| pretty print a single Haskell file. Takes the filename of the Haskell module to be printed as only command line argument, prints the pretty version of is the the standard output. Strips all comments. But keeps pragmas. DHUN-}
module Main (main) where
import Language.Haskell.Exts
import Language.Haskell.Exts.Pretty
import Data.Maybe
import System.Environment

{-DHUN| save function to access an list by index. If an element at that index exists the element is returned wrapped in a Just of the maybe monad. Otherwise the value Nothing of the Maybe monad is returned DHUN-}
ind l n = if length l > n then Just $ l !! n else Nothing

{-DHUN| Takes a String as an single input parameter. It has to be the source code of module written in Haskell. It return a parse tree of that module DHUN-}
parseModuleFromFile inp = fromParseResult $ parseFileContents inp

{-DHUN| the main function, see documentation at the head of this module DHUN-}

main :: IO ()
main
  = do args <- getArgs
       let file = ind args 0
       let inp = maybe getContents readFile file
       inpStr <- inp
       let m = parseModuleFromFile inpStr
       putStrLn $ prettyPrint m
