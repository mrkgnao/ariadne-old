{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ariadne.Logging where

import Control.Monad.Logger

import           Control.Monad.Logger
import           System.Log.FastLogger

import qualified Data.ByteString.Char8        as S8

import           Data.Char                    (toLower, toUpper)
import           Data.Monoid                  (mconcat, (<>))
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T

import           Data.Time

import           System.IO                    (Handle, stderr, stdout)

import           System.Console.ANSI
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))

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
        [ toLogStr (show (black (text ts)))
        , " "
        , toLogStr (show (black (text ds)))
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

defaultLogLevelStr level = toLogStr (S8.pack preCode)
  where
    basename = map toUpper $ drop 5 $ show level
    preCode =  show $ fill 8 . colorString $ text ("[" <> basename <> "]")
      where
        colorString =
          case level of
            LevelError     -> red
            LevelWarn      -> yellow
            LevelDebug     -> green
            LevelInfo      -> blue
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
