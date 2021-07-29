module Ch23a where

import Prelude

import Control.Concurrent ( threadDelay, forkIO, runInBoundThread, killThread )
import Control.Concurrent.MVar ( takeMVar, putMVar, MVar, readMVar, newEmptyMVar )
import Data.Functor ( void )

test :: IO ()
test = do
  ttMVar <- newEmptyMVar
  clockThread <- forkIO $ clock ttMVar
  putMVar ttMVar Tick
  _ <- runInBoundThread $ bomb ttMVar 3
  killThread clockThread

data TickTock = Tick | Tock deriving (Eq)

data BombState = WaitForTick | WaitForTock

clock :: MVar TickTock -> IO ()
clock ttMVar = do
  void $ takeMVar ttMVar
  threadDelay 1000000
  putMVar ttMVar Tock
  void $ takeMVar ttMVar
  threadDelay 1000000
  putMVar ttMVar Tick
  clock ttMVar

bomb :: (Eq a, Num a) => MVar TickTock -> a -> IO ()
bomb ttMVar destinationCount = go 0 WaitForTick
  where
    go count state = do
      if count == destinationCount then putStrLn "BOOM!!!"
      else do
        threadDelay 500000
        tt <- readMVar ttMVar
        case state of
          WaitForTick ->
            if tt == Tick then putStrLn "Tick" *> go count WaitForTock
            else go count state
          WaitForTock ->
            if tt == Tock then putStrLn "Tock" *> go (count + 1) WaitForTick
            else go count state