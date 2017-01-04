module Shopie.Auth.StorageF where

import Prelude

import Data.Maybe (Maybe)


data StorageF a
  = Persist String String a
  | Restore String (Maybe String -> a)
  | Remove String a

instance functorStorageF :: Functor StorageF where
  map f (Persist s s2 a) = Persist s s2 (f a)
  map f (Restore s k) = Restore s (f <<< k)
  map f (Remove s a) = Remove s (f a)

persist :: String -> String -> StorageF Unit
persist k v = Persist k v unit

restore :: String -> StorageF (Maybe String)
restore = flip Restore id

remove :: String -> StorageF Unit
remove = flip Remove unit
