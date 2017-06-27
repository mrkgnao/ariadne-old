{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ariadne.Logging where

import Control.Monad.Logger

import           Control.Monad.Logger
import           System.Log.FastLogger

import qualified Data.ByteString.Char8        as S8

import           Data.Char                    (toLower, toUpper)
import           Data.Monoid                  (mconcat, (<>))
import qualified Data.Text.Lazy                    as T
import qualified Data.Text.Lazy.Encoding                    as T
import qualified Data.Text.IO                 as T

import           Data.Time

import           System.IO                    (stdout)

import           Data.Text.Prettyprint.Doc hiding ((<>))
import           Data.Text.Prettyprint.Doc.Render.Terminal

import GHC.Stack

logger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logger _ _ lvl msg = logger'
  where
    logger' = do
      datestamp <- getDate
      timestamp <- getTime
      S8.hPutStr stdout . fromLogStr $
        mconcat [timeDate timestamp datestamp, "\n", logLine, "\n\n"]
    timeDate ts ds =
      mconcat
        [ toLogStr
            (renderLazy $
             layoutPretty
               defaultLayoutOptions
               (annotate (color Black) (pretty ts)))
        , " "
        , toLogStr
            (renderLazy $
             layoutPretty
               defaultLayoutOptions
               (annotate (color Black) (pretty ds)))
        ]
    logLine = mconcat [defaultLogLevelStr lvl, " ", msg]
    getDate = do
      now <- getZonedTime
      return $ formatTime' now
      where
        formatTime' = take dateLength . formatTime defaultTimeLocale "%F"
        dateLength =
          length $
          formatTime defaultTimeLocale "%F" (UTCTime (ModifiedJulianDay 0) 0)
    getTime = do
      now <- getZonedTime
      return $ formatTime' now
      where
        formatTime' = take timeLength . formatTime defaultTimeLocale "%T.%q"
        timeLength =
          length $
          formatTime
            defaultTimeLocale
            "%T.000000"
            (UTCTime (ModifiedJulianDay 0) 0)

defaultLogLevelStr :: LogLevel -> LogStr
defaultLogLevelStr (LevelOther t) = toLogStr t

defaultLogLevelStr level = toLogStr (T.encodeUtf8 preCode)
  where
    basename = map toUpper $ drop 5 $ show level
    preCode =
      renderLazy $
      layoutPretty defaultLayoutOptions $
      fill 8 . colorString $ pretty ("[" <> basename <> "]")
      where
        colorString =
          case level of
            LevelError -> annotate (color Red)
            LevelWarn -> annotate (color Yellow)
            LevelDebug -> annotate (color Green)
            LevelInfo -> annotate (color Blue)
            _ -> id

execute :: LoggingT m a -> m a
execute op =
  runLoggingT op logger

logTest :: HasCallStack => LoggingT IO ()
logTest = do
  logInfoN "Starting download"
  logErrorN "404"
  logWarnN "Trying mirror"
  logInfoN "File found"
  logDebugN "Finished in 40s"
  logInfoN "Completed download"
  pure ()
