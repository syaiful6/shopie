module Qyson.Types where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Path.Pathy (AbsPath, AbsFile, AbsDir, Sandboxed, (</>))
import Data.Path.Pathy as Py
import Data.StrMap (StrMap)


type AnyPath = AbsPath Sandboxed
type DirPath = AbsDir Sandboxed
type FilePath = AbsFile Sandboxed

type Vars = StrMap String

data Pagination = Pagination Int Int

offset :: Pagination -> Int
offset (Pagination ofs _) = ofs

limit :: Pagination -> Int
limit (Pagination _ lm) = lm

parseFile :: String -> Either String (Py.AbsFile Py.Sandboxed)
parseFile pt =
  Py.parseAbsFile pt
  >>= Py.sandbox Py.rootDir
  <#> (Py.rootDir </> _)
  # maybe (Left "Incorrect resource") pure
