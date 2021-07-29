{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
module Ch23b where

import Prelude

import Data.Fixed ( HasResolution(resolution) )
import Data.Functor ( void )
import Data.List ( unfoldr ) 
import Data.Time
    ( getZonedTime,
      LocalTime(LocalTime, localTimeOfDay),
      TimeOfDay(TimeOfDay, todSec),
      ZonedTime(ZonedTime, zonedTimeToLocalTime) )
import Control.Concurrent ( MVar, forkIO, threadDelay, putMVar, newEmptyMVar, runInBoundThread, killThread, takeMVar ) 
import Control.Concurrent.MVar ( MVar, readMVar )
import Control.Monad (forever, unless, when)
import Control.Monad.Trans.Class ( MonadTrans(lift) ) 
import Control.Monad.Trans.Reader ( ReaderT (runReaderT), ask )
import Control.Monad.Trans.State.Strict ( StateT (runStateT), get, put )
import System.Random ( uniformR, mkStdGen, RandomGen, StdGen )

test :: IO ()
test = do
  fakeBus <- newEmptyMVar
  let forkThreadM = forkIO . runThreadM fakeBus
  let runBoundThreadM = runInBoundThread . runThreadM fakeBus
  loggerThread <- forkThreadM logger
  thread1 <- forkThreadM $ randomGenerator "greater than 0.5" (> 0.5)
  thread2 <- forkThreadM $ randomGenerator "less than 0.5" (< 0.5)
  runBoundThreadM $ randomGenerator "greather than 0.1" (> 0.1)
  killThread loggerThread
  killThread thread1
  killThread thread2

newtype Config = Config { fakeBus :: MVar [Char] }
newtype State = State { count :: Int }
type ThreadM a = ReaderT Config (StateT State IO) a

runThreadM :: MVar [Char] -> ThreadM () -> IO ()
runThreadM fakeBus = void . flip runStateT (State { count = 10 }) . flip runReaderT (Config { fakeBus })

logger :: ThreadM ()
logger = forever do
  Config { fakeBus } <- ask
  s <- liftIOToThreadM $ takeMVar fakeBus
  liftIOToThreadM $ putStrLn $ "Logger: " <> s

liftIOToThreadM :: IO a -> ThreadM a
liftIOToThreadM = lift . lift

randomGenerator :: [Char] -> (Float -> Bool) -> ThreadM ()
randomGenerator valueType pred = do
  State { count } <- lift get
  unless (count <= 0) do
    Config { fakeBus } <- ask
    liftIOToThreadM do
      n <- delayRandom
      when (pred n) $ putMVar fakeBus $ "Found a value that is " <> valueType <> " (" <> show n <> ")"
    lift $ put (State { count = count - 1 })
    randomGenerator valueType pred

delayRandom :: IO Float
delayRandom = threadDelay 1000000 *> (randomEff <$> pureGen)

constant :: IO Float
constant = pure 0.0

pureGen :: IO StdGen
pureGen = mkStdGen . fromInteger <$> getCurrentLocalSeconds

randomEff :: RandomGen g => g -> Float
randomEff = head . unfoldr (Just . uniformR (0.0, 1.0))

getCurrentLocalSeconds :: IO Integer
getCurrentLocalSeconds = do
  ZonedTime { zonedTimeToLocalTime } <- getZonedTime
  LocalTime { localTimeOfDay } <- pure zonedTimeToLocalTime
  TimeOfDay { todSec } <- pure localTimeOfDay
  pure $ resolution todSec