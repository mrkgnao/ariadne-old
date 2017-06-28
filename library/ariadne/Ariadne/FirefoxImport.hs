{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Ariadne.FirefoxImport where

import           Data.Char            (ord)
import           Data.Csv

import           Data.Text            (Text)
import qualified Data.Text            as T

import qualified Data.ByteString      as BS
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS

import           Data.Vector          (Vector)
import qualified Data.Vector          as V

import           GHC.Generics

import           Control.Monad
import           System.IO            as S

import qualified Ariadne.AriadneT     as A
import qualified Ariadne.Tables       as A
import           Edible

tsvOpts :: DecodeOptions
tsvOpts = defaultDecodeOptions {decDelimiter = fromIntegral (ord '\t')}

data Bookmark = Bookmark
  { uri   :: !Text
  , title :: !Text
  , added :: Integer
  } deriving (Show, Generic)

instance FromRecord Bookmark
instance ToRecord Bookmark

filename = "/home/mrkgnao/code/haskell/ariadne/bookmarks.tsv"

contents :: IO ByteString
contents = LBS.readFile filename

printBookmarks :: IO ()
printBookmarks = do
  conn <- connect A.connInfo
  withFile filename S.ReadMode (loop conn)
  where
    loop conn h = do
      c <- BS.hGetLine h
      let l :: Either String (Vector Bookmark)
          l = decodeWith tsvOpts NoHeader (LBS.pack (BS.unpack c))
      case l of
        Right (V.toList -> [b]) -> do
          print b
          k <-
            A.escapeLabyrinth
              (A.AriadneState conn)
              (A.createLink (uri b) (title b))
          print k
        _ -> pure ()
      eof <- hIsEOF h
      when (not eof) (loop conn h)
