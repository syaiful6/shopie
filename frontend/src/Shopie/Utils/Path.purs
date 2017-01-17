module Shopie.Utils.Path where

import Prelude

import Control.Alt ((<|>))

import Data.Either (Either(..), either, fromRight)
import Data.Maybe (Maybe, maybe, fromMaybe)
import Data.Path.Pathy (Sandboxed, Unsandboxed, Abs, Path, File, Rel, Dir, DirName(..),
  FileName(..), peel, rootDir, (</>), file, parseAbsDir, parseAbsFile, dir, relativeTo, renameDir)
import Data.Path.Pathy as P
import Data.String as S
import Data.String.Regex as Rgx
import Data.String.Regex.Flags as RXF
import Data.Tuple (snd, fst)

import Global as G

import Qyson.Types (AnyPath, DirPath, FilePath)

import Partial.Unsafe (unsafePartial)


renameDirExt :: forall a s. Path a Dir s -> String -> Path a Dir s
renameDirExt p ext = renameDir (changeDirExt $ const ext) p

rootify :: DirPath -> Path Rel Dir Sandboxed
rootify p = fromMaybe (dir "/") $ relativeTo p rootDir

rootifyFile :: FilePath -> Path Rel File Sandboxed
rootifyFile p = fromMaybe (file "") $ relativeTo p rootDir

sandbox :: forall b. Path Abs b Unsandboxed -> Maybe (Path Abs b Sandboxed)
sandbox = map (rootDir </> _) <<< P.sandbox rootDir

parseAnyPath :: String -> Maybe AnyPath
parseAnyPath s
  = Right <$> parseFilePath s
  <|> Left <$> parseDirPath s

parseFilePath :: String -> Maybe FilePath
parseFilePath s = (rootDir </> _) <$> (P.sandbox rootDir =<< parseAbsFile s)

parseDirPath :: String -> Maybe DirPath
parseDirPath s = (rootDir </> _) <$> (P.sandbox rootDir =<< parseAbsDir s)

changeDirExt :: (String -> String) -> DirName -> DirName
changeDirExt f (DirName name) =
  DirName ((if ext == "" then name else n) <> "." <> f ext)
  where
  mbIdx = S.lastIndexOf (S.Pattern ".") name
  n = maybe name (\idx -> S.take idx name) mbIdx
  ext = maybe "" (\idx -> S.drop (idx + 1) name) mbIdx

dropDirExt :: DirName -> DirName
dropDirExt (DirName d) =
  DirName $ maybe d (\idx -> S.take idx d) $ S.lastIndexOf (S.Pattern ".") d

takeDirExt :: DirName -> String
takeDirExt (DirName d) =
  maybe "" (\idx -> S.drop (idx + 1) d) $ S.lastIndexOf (S.Pattern ".") d

decodeURIPath :: String -> String
decodeURIPath uri =
  G.decodeURIComponent $
    Rgx.replace (unsafePartial fromRight $ Rgx.regex "\\+" RXF.global) " " uri

encodeURIPath :: String -> String
encodeURIPath path =
  S.joinWith "/" $
  S.joinWith "+" <$>
  (G.encodeURIComponent <$> _) <$>
  S.split (S.Pattern " ") <$>
  S.split (S.Pattern "/") path

hidePath :: String -> String -> String
hidePath path input =
  S.trim $
  S.replace (S.Pattern $ "+path:\"" <> path <> "\"") (S.Replacement "") $
  S.replace (S.Pattern $ "+path:" <> path) (S.Replacement "") input

getNameStr :: AnyPath -> String
getNameStr ap = either getNameStr' getNameStr' ap
  where
  getNameStr' :: forall b a s. Path a b s -> String
  getNameStr' p = maybe "" (snd >>> nameOfFileOrDir) $ peel p

nameOfFileOrDir :: Either DirName FileName -> String
nameOfFileOrDir (Left (DirName name)) = name
nameOfFileOrDir (Right (FileName name)) = name

getDir :: AnyPath -> DirPath
getDir ap = either getDir' getDir' ap
  where
  getDir' :: forall b. Path Abs b Sandboxed -> DirPath
  getDir' = maybe rootDir fst <<< peel

rootFile :: FilePath
rootFile = rootDir </> file ""
