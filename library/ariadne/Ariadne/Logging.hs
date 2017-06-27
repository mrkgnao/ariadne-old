{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariadne.Logging where

import           Control.Monad.Logger
import           System.Log.FastLogger

import           Data.Char                                 (toUpper)
import           Data.Monoid                               (mconcat)

import qualified Data.ByteString.Char8                     as S8

import           Data.Text                                 as Strict (Text)
import qualified Data.Text.Encoding                        as Strict

import           Data.Time

import           Data.Text.Prettyprint.Doc                 hiding ((<>))
import           Data.Text.Prettyprint.Doc.Render.Terminal

import           Lib.Prelude                               hiding (group)
import           Prelude                                   (String,show)

layoutAndRender :: Doc AnsiStyle -> Strict.Text
layoutAndRender = renderStrict . layoutPretty defaultLayoutOptions

colorize :: Pretty a => Color -> a -> Doc AnsiStyle
colorize x = annotate (color x) . pretty

logger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logger _ _ lvl msg = logger'
  where
    logger' = do
      datestamp <- getDate
      timestamp <- getTime
      let logStr =
            toLogStr $
            mconcat
              [ toLogStr (timeDate timestamp datestamp)
              , "\n"
              , toLogStr logLine
              , "\n"
              ]
      S8.putStrLn (fromLogStr logStr)
    timeDate ts ds = layoutAndRender (foldMap (colorize Black) [ts, " ", ds])
    logLine =
      mconcat
        [ Strict.encodeUtf8 (logLevelStr lvl)
        , " "
        , msg
       |$ fromLogStr
       |> Strict.decodeUtf8
       |> pretty
       |> hang 9
       |> layoutPretty defaultLayoutOptions
       |> renderStrict
       |> Strict.encodeUtf8
        ]
    getDate = getDateElement "%F" "%F"
    getTime = getDateElement "%T.%q" "%T.000000"

getDateElement :: String -> String -> IO String
getDateElement a b = formatElt <$> getZonedTime
  where
    formatElt = take dateLength . formatTime defaultTimeLocale a
    dateLength =
      length
        (formatTime defaultTimeLocale b (UTCTime (ModifiedJulianDay 0) 0))

logLevelStr :: LogLevel -> Strict.Text
logLevelStr (LevelOther t) = t
logLevelStr level = preCode
  where
    basename = map toUpper $ drop 5 $ show level
    preCode = (layoutAndRender . fill 8 . annotate bold . colorLevel) ("|" <> basename <> "|")
      where
        colorLevel =
          case level of
            LevelError -> colorize Red
            LevelWarn  -> colorize Yellow
            LevelDebug -> colorize Green
            LevelInfo  -> colorize Blue
            _          -> pretty

execute :: LoggingT m a -> m a
execute op =
  runLoggingT op logger

logTest :: LoggingT IO ()
logTest = do
  logInfoN "Starting download"
  logErrorN "404"
  logWarnN "Trying mirror"
  logInfoN "File found"
  logDebugN "Finished in 40s"
  logInfoN "Completed download"
  pure ()
