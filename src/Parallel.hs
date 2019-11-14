{-DHUN| module for parallel computations. The paralellism is reached by forking threads with liftIO. When starting a thread and empty MVar is returned immediately. The MVar is filled on completing of the thread. Normal function can be lifted to threaded function operation on MVar of the parameters instead of the parameters themself. Thus it is possible to define dependencies of function on the return values of other functions and let the order of excecution evolove automatically, so a function is called as soon as all its parameters have been calculated. All functions considered in this module are understood to have an IO return type. This module is mainly used for paraller downloading from the web. DHUN-}
module Parallel where
import Control.Concurrent.MVar

{-DHUN| takes a function of return type IO which takes on parameter and an MVar containing at the same type as the parameter of the function and returns and IO action containing an MVar containing the same type as the tyoe contained in the IO Action returned by the function. This function retuns immediately and passed an empty MVar as return type. As soon as the MVar containing the same type as the parameter of the function is ready for reading the the function is executed in a new thread. As soon as the function return a value, the it is written into the returned MVar.  DHUN-}

(<$$>) :: MVar Int -> (a -> IO b) -> MVar a -> IO (MVar b)
(<$$>) vv f x
  = do var <- newEmptyMVar
       _ <- myFork vv (go var)
       return var
  where go v
          = do xx <- readMVar x
               result <- f xx
               putMVar v result

{-DHUN| takes a value and returns an IO action that contain a filled MVar conatining the value DHUN-}

base :: MVar Int -> a -> IO (MVar a)
base v x = ((ppure v) . return) x

{-DHUN| takes an IO action and return an MVar containing the and IO action the returns an MVar containing the same type as contained by the IO action passed to this function. The IO action passed as first parameter to this function is executed in a new thread. As soon as it finishes its result is written into the returned MVar DHUN-}

ppure :: MVar Int -> IO a -> IO (MVar a)
ppure vv x
  = do var <- newEmptyMVar
       _ <- myFork vv (go var)
       return var
  where go v
          = do result <- x
               putMVar v result

{-DHUN| alias to switch between forkOS and forkIO for testing DHUN-}

myFork :: MVar Int -> IO () -> IO ()
myFork _ x = x

{-DHUN| takes a function which takes a list and return an IO action as first parameter. It takes a list of MVar containing the same type as the type contained in the list as second parameter. It returnes an empty MVar immediately. As soon as all MVar in the list given as second parameter could be read, the function is started in a new thread. As soon as it finishes, the returend MVar is filled with the value returend by the function DHUN-}

liftList :: MVar Int -> ([a] -> IO b) -> [MVar a] -> IO (MVar b)
liftList vv f x
  = do var <- newEmptyMVar
       _ <- myFork vv (go var)
       return var
  where go v
          = do xx <- mapM readMVar x
               result <- f xx
               putMVar v result

{-DHUN| the same as liftList. Only difference is that the seconds parameter is not a list of MVars containing values but an MVar containing a list of MVars containing values. So an addinal step is made to read the out MVar DHUN-}

liftList2 ::
          MVar Int -> ([a] -> IO b) -> MVar [MVar a] -> IO (MVar b)
liftList2 vv f x
  = do var <- newEmptyMVar
       _ <- myFork vv (go var)
       return var
  where go v
          = do xxx <- readMVar x
               xx <- mapM readMVar xxx
               result <- f xx
               putMVar v result

{-DHUN| prefix version of the angle bracked dollar operator described above DHUN-}

liftA :: MVar Int -> (a -> IO b) -> MVar a -> IO (MVar b)
liftA v f x = (<$$>) v f x

{-DHUN| same as liftA. Just function must have exactly two parameter instead of only one DHUN-}

liftA2 ::
       MVar Int -> (a -> b -> IO c) -> MVar a -> MVar b -> IO (MVar c)
liftA2 vv f x y
  = do var <- newEmptyMVar
       _ <- myFork vv (go var)
       return var
  where go v
          = do xx <- readMVar x
               yy <- readMVar y
               result <- f xx yy
               putMVar v result

{-DHUN| same as liftA. Just function must have exactly three parameter instead of only one DHUN-}

liftA3 ::
       MVar Int ->
         (a -> b -> c -> IO d) -> MVar a -> MVar b -> MVar c -> IO (MVar d)
liftA3 vv f x y z
  = do var <- newEmptyMVar
       _ <- myFork vv (go var)
       return var
  where go v
          = do xx <- readMVar x
               yy <- readMVar y
               zz <- readMVar z
               result <- f xx yy zz
               putMVar v result
