module Data.Qyson.QysonF
  ( module Data.Qyson.QysonF
  , module Data.Qyson.Error
  , module Data.Qyson.Types
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe)
import Data.Qyson.Error (type (!~>), ResponseQ, ErrorQ(..), UnauthorizedMessage(..),
                         lowerErrorQ, printErrorQ)
import Data.Qyson.Types (AnyPath, FilePath, DirPath, Pagination, Vars)


data QysonF a
  = ReadQuery DirPath Vars (Maybe Pagination) (Json !~> a)
  | ReadFile FilePath (Maybe Pagination) (Json !~> a)
  | WriteFile FilePath Json (Unit !~> a)
  | AppendFile FilePath Json (Unit !~> a)
  | DeleteData AnyPath (Unit !~> a)

instance functorQysonF :: Functor QysonF where
  map f (ReadQuery d vp mp g) = ReadQuery d vp mp (f <<< g)
  map f (ReadFile fp mp g) = ReadFile fp mp (f <<< g)
  map f (WriteFile fp dc g) = WriteFile fp dc (f <<< g)
  map f (AppendFile fp dc g) = AppendFile fp dc (f <<< g)
  map f (DeleteData apt g) = DeleteData apt (f <<< g)

type QysonFE a = QysonF (ResponseQ a)

readQuery :: DirPath -> Vars -> Maybe Pagination -> QysonFE Json
readQuery d vp mp = ReadQuery d vp mp id

readFile :: FilePath -> Maybe Pagination -> QysonFE Json
readFile fp mp = ReadFile fp mp id

writeFile :: FilePath -> Json -> QysonFE Unit
writeFile fp jp = WriteFile fp jp id

appendFile :: FilePath -> Json -> QysonFE Unit
appendFile fp jp = AppendFile fp jp id

deleteData :: AnyPath -> QysonFE Unit
deleteData apt = DeleteData apt id
