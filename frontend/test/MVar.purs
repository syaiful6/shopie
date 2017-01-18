module Test.MVar where

import Prelude

import Control.Monad.Aff (Aff, runAff, forkAff, later', attempt)
import Control.Monad.Aff.MVar (MVAR, makeMVar, takeMVar, putMVar, killMVar)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Error.Class (throwError)

import Data.Either (either)

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
  either (const $ liftEff $ log "Success: Killed MVar") (const $ liftEff $ log "Failure: Oh noes, MVar survived!") e

main :: forall e. Eff (console :: CONSOLE, mvar :: MVAR, err :: EXCEPTION | e) Unit
main =
  void $ runAff throwException (const (pure unit)) $ do
    liftEff $ log "Testing put and take MVar"
    test_putTakeMVar

    liftEff $ log "Testing kill var"
    test_killMVar
