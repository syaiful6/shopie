module Qyson.QysonF
  ( module Qyson.QysonF
  , module Qyson.Data
  , module Qyson.Error
  , module Qyson.Types
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe)

import Qyson.Data (DataQ, JSONMode, applicationVndJSONApi, jsonApi, json, urlEncoded
                       , customData, jsonModeApi, jsonModeNormal, jsonMediaType)
import Qyson.Error (type (:~>), ResponseQ, ErrorQ(..), UnauthorizedMessage(..),
                         lowerErrorQ, printErrorQ)
import Qyson.Types (AnyPath, FilePath, DirPath, Pagination(..), Vars)


data QysonF a
  = ReadQuery JSONMode DirPath Vars (Maybe Pagination) (Json :~> a)
  | WriteQuery DirPath DataQ (Json :~> a)
  | ReadFile JSONMode FilePath (Maybe Pagination) (Json :~> a)
  | WriteFile FilePath DataQ (Json :~> a)
  | AppendFile FilePath DataQ (Json :~> a)
  | DeleteFile AnyPath (Unit :~> a)

instance functorQysonF :: Functor QysonF where
  map f (ReadQuery md d vp mp g) = ReadQuery md d vp mp (f <<< g)
  map f (WriteQuery d jp g) = WriteQuery d jp (f <<< g)
  map f (ReadFile md fp mp g) = ReadFile md fp mp (f <<< g)
  map f (WriteFile fp dc g) = WriteFile fp dc (f <<< g)
  map f (AppendFile fp dc g) = AppendFile fp dc (f <<< g)
  map f (DeleteFile apt g) = DeleteFile apt (f <<< g)

type QysonFE a = QysonF (ResponseQ a)

readQuery :: JSONMode -> DirPath -> Vars -> Maybe Pagination -> QysonFE Json
readQuery md d vp mp = ReadQuery md d vp mp id

writeQuery :: DirPath -> DataQ -> QysonFE Json
writeQuery dp js = WriteQuery dp js id

readFile :: JSONMode -> FilePath -> Maybe Pagination -> QysonFE Json
readFile md fp mp = ReadFile md fp mp id

writeFile :: FilePath -> DataQ -> QysonFE Json
writeFile fp jp = WriteFile fp jp id

appendFile :: FilePath -> DataQ -> QysonFE Json
appendFile fp jp = AppendFile fp jp id

deleteFile :: AnyPath -> QysonFE Unit
deleteFile apt = DeleteFile apt id
