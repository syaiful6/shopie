module Test.MVar where

import Prelude

import Control.Monad.Aff (Aff, runAff, forkAff, later, later', attempt, cancel)
import Control.Monad.Aff.MVar (MVAR, makeMVar, makeMVar', takeMVar, putMVar, killMVar,
  peekMVar, modifyMVar)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Error.Class (throwError)

import Data.Either (either)
import Data.Maybe (Maybe(..))

type TestMVar a = forall e. Aff (console :: CONSOLE, mvar :: MVAR | e) a

timeout :: Int -> TestMVar Unit -> TestMVar Unit
timeout ms aff = do
  exn <- makeMVar
  clr1 <- forkAff (later' ms (putMVar exn (Just "Timed out")))
  clr2 <- forkAff (aff *> putMVar exn Nothing)
  res <- takeMVar exn
  liftEff $ log (show res)
  case res of
    Nothing -> void (clr1 `cancel` error "Done")
    Just e -> void (clr2 `cancel` error "Done") *> throwError (error e)

test_putTakeMVar :: TestMVar Unit
test_putTakeMVar = do
  m <- makeMVar
  forkAff (later' 100 $ putMVar m 1.0)
  a <- takeMVar m
  if a == 1.0
    then liftEff $ log "Success: put and take MVar"
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

test_peekMVar :: TestMVar Unit
test_peekMVar = do
  timeout 1000 do
    v <- makeMVar
    forkAff (later $ putMVar v 1.0)
    a1 <- peekMVar v
    a2 <- peekMVar v
    when (a1 /= a2) do
      throwError (error "Something horrible went wrong - peeked var is not equal to taken var")
    liftEff $ log ("Success: Peeked value not consumed")

  timeout 1000 do
    w <- makeMVar
    putMVar w true
    b <- peekMVar w
    when (not b) do
      throwError (error "Something horrible went wrong - peeked var is not true")
    liftEff $ log ("Success: Peeked value read from written var")

  timeout 1000 do
    x <- makeMVar
    res <- makeMVar' 1
    forkAff do
      c <- peekMVar x
      modifyMVar (_ + c) res
      putMVar x 1000
    forkAff do
      c1 <- peekMVar x
      modifyMVar (_ + c1) res
      putMVar x 500
    putMVar x 10
    count <- takeMVar res
    e <- takeMVar x
    f <- takeMVar x
    g <- takeMVar x
    when (not (count == 21 && e == 10 && f == 1000 && g == 500)) do
      throwError (error "Something horrible went wrong - peeked consumers/producer ordering")
    liftEff $ log "Success: peekVar consumer/producer order maintained"

main :: forall e. Eff (console :: CONSOLE, mvar :: MVAR, err :: EXCEPTION | e) Unit
main =
  void $ runAff throwException (const (pure unit)) $ do
    liftEff $ log "Testing put and take MVar"
    test_putTakeMVar

    liftEff $ log "Testing kill var"
    test_killMVar

    liftEff $ log "Testing peekMVar"
    test_peekMVar
