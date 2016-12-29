module Data.Qyson.QysonF
  ( module Data.Qyson.QysonF
  , module Data.Qyson.Error
  , module Data.Qyson.Types
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Functor.Pairing (Pairing)
import Data.Maybe (Maybe)

import Data.Qyson.Error (type (:~>), type (<~:), ResponseQ, ErrorQ(..), UnauthorizedMessage(..),
                         lowerErrorQ, printErrorQ)
import Data.Qyson.Types (AnyPath, FilePath, DirPath, Pagination(..), Vars)

import Data.Qyson.Utils (pairArrowTuple)

data QysonF a
  = ReadQuery DirPath Vars (Maybe Pagination) (Json :~> a)
  | ReadFile FilePath (Maybe Pagination) (Json :~> a)
  | WriteFile FilePath Json (Unit :~> a)
  | AppendFile FilePath Json (Unit :~> a)
  | DeleteFile AnyPath (Unit :~> a)

instance functorQysonF :: Functor QysonF where
  map f (ReadQuery d vp mp g) = ReadQuery d vp mp (f <<< g)
  map f (ReadFile fp mp g) = ReadFile fp mp (f <<< g)
  map f (WriteFile fp dc g) = WriteFile fp dc (f <<< g)
  map f (AppendFile fp dc g) = AppendFile fp dc (f <<< g)
  map f (DeleteFile apt g) = DeleteFile apt (f <<< g)

type QysonFE a = QysonF (ResponseQ a)

readQuery :: DirPath -> Vars -> Maybe Pagination -> QysonFE Json
readQuery d vp mp = ReadQuery d vp mp id

readFile :: FilePath -> Maybe Pagination -> QysonFE Json
readFile fp mp = ReadFile fp mp id

writeFile :: FilePath -> Json -> QysonFE Unit
writeFile fp jp = WriteFile fp jp id

appendFile :: FilePath -> Json -> QysonFE Unit
appendFile fp jp = AppendFile fp jp id

deleteFile :: AnyPath -> QysonFE Unit
deleteFile apt = DeleteFile apt id

type CoqysonR a =
  { readQueryH  :: DirPath -> Vars -> Maybe Pagination -> Json <~: a
  , readfileH   :: FilePath -> Maybe Pagination -> Json <~: a
  , writeFileH  :: FilePath -> Json -> Unit <~: a
  , appendFileH :: FilePath -> Json -> Unit <~: a
  , deleteFileH :: AnyPath -> Unit <~: a
  }

data CoqysonF a = CoqysonF (CoqysonR a)

instance functorCoQysonF :: Functor CoqysonF where
  map f (CoqysonF rc) = CoqysonF $
    { readQueryH: map (map (map (map f))) rc.readQueryH
    , readfileH: map (map (map f)) rc.readfileH
    , writeFileH: map (map (map f)) rc.writeFileH
    , appendFileH: map (map (map f)) rc.appendFileH
    , deleteFileH: map (map f) rc.deleteFileH
    }

type CoqysonFE a = CoqysonF (ResponseQ a)

pairQysonF :: Pairing QysonF CoqysonF
pairQysonF f (ReadQuery d vp mp k) (CoqysonF rc) = pairArrowTuple f k (rc.readQueryH d vp mp)
pairQysonF f (ReadFile fp mp k) (CoqysonF rc) = pairArrowTuple f k (rc.readfileH fp mp)
pairQysonF f (WriteFile fp dc k) (CoqysonF rc) = pairArrowTuple f k (rc.writeFileH fp dc)
pairQysonF f (AppendFile fp dc k) (CoqysonF rc) = pairArrowTuple f k (rc.appendFileH fp dc)
pairQysonF f (DeleteFile apt k) (CoqysonF rc) = pairArrowTuple f k (rc.deleteFileH apt)
