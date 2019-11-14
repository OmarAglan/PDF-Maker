import Load
import Hex
import System.Exit
import System.Environment
import ImperativeState
import Control.Monad.State
import Control.Monad.Error
import Control.Concurrent.MVar
import UrlAnalyse
import System.IO
import Data.Maybe
main = do args<-getArgs
          let xx=case args of 
                   [hh]-> case reads (unhex (head args)) :: [(((FullWikiUrl,String),[String]),String)] of
                           [(n, _)] -> Just n
                           _ -> Nothing
                   _->Nothing
          gg<-case xx of 
                Just ((fu,tp),l) -> do (yy,_)<-(runStateT (runErrorT (getContributors l))
                                                imperativeStateZero {fullUrl=fu,tmpPath=tp})
                                       case yy of
                                        Right z -> return $ Just z
                                        _-> return Nothing

                _-> return Nothing
          case gg of 
            Just hh -> do h1<-mapM readMVar (fst hh)
                          h2<-mapM readMVar (snd hh)
                          hPutStrLn stderr (hex (show (h1,h2)))
                          exitSuccess  
            _-> do putStrLn "This is executable is an internal part of mediawiki2latex. It is only intended to be called by mediawiki2latex itself. Please refer to the manpage of mediawiki2latex for more information."
                   exitFailure


module Main (main) where
