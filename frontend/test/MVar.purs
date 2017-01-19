module Test.MVar where

import Prelude

import Control.Monad.Aff (Aff, runAff, forkAff, later', attempt)
import Control.Monad.Aff.MVar (MVAR, makeMVar, takeMVar, putMVar, killMVar)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (forever)

import Data.Either (either)
import Data.Array ((..))
import Data.Foldable (traverse_)

type TestMVar a = forall e. Aff (console :: CONSOLE, mvar :: MVAR | e) a

test_putTakeMVar :: TestMVar Unit
test_putTakeMVar = do
  m <- makeMVar
  forkAff (later' 100 $ putMVar m 1.0)
  a <- takeMVar m
  if a == 1.0
    then pure unit
    else throwError (error ("test_putTakeMVar fail. Expect 1.0, Got: " <> show a))

test_killMVar :: TestMVar Unit
test_killMVar = do
  v <- makeMVar
  killMVar v (error "DOA")
  e <- attempt $ takeMVar v
  either
    (const $ liftEff $ log "Success: Killed MVar")
    (const $ liftEff $ log "Failure: Oh noes, MVar survived!")
    e

test_roundRobin :: TestMVar Unit
test_roundRobin = do
  v <- makeMVar
  forkAff $ forever $ do
    val <- takeMVar v
    liftEff $ log ("received value (Thread #1): " <> show val)
  forkAff $ forever $ do
    val <- takeMVar v
    liftEff $ log ("received value (Thread #2): " <> show val)
  traverse_ (putMVar v) (0..10)

test_roundRobinPut :: TestMVar Unit
test_roundRobinPut = do
  v <- makeMVar
  forkAff $ traverse_ (putMVar v) (0..10)
  forkAff $ traverse_ (putMVar v) (11..20)
  void $ forkAff $ forever $ do
    val <- takeMVar v
    liftEff $ log ("received: " <> show val)

main :: forall e. Eff (console :: CONSOLE, mvar :: MVAR, err :: EXCEPTION | e) Unit
main =
  void $ runAff throwException (const (pure unit)) $ do
    liftEff $ log "Testing put and take MVar"
    test_putTakeMVar

    liftEff $ log "Testing kill var"
    test_killMVar

    liftEff $ log "Testing round robin"
    test_roundRobin

    liftEff $ log "Testing put block until it consumed"
    test_roundRobinPut
