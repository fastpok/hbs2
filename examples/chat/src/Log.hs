module Log (
  module Log,
  module HBS2.System.Logger.Simple.ANSI,
  module Prettyprinter,
  module Prettyprinter.Render.Terminal,
) where

import Control.Monad.IO.Class
import HBS2.System.Logger.Simple.ANSI
import Prettyprinter
import Prettyprinter.Render.Terminal

tracePrefix :: SetLoggerEntry
tracePrefix = toStderr . logPrefix "[trace] "

debugPrefix :: SetLoggerEntry
debugPrefix = toStderr . logPrefix "[debug] "

errorPrefix :: SetLoggerEntry
errorPrefix = toStderr . logPrefix "[error] "

warnPrefix :: SetLoggerEntry
warnPrefix = toStderr . logPrefix "[warn] "

noticePrefix :: SetLoggerEntry
noticePrefix = toStderr . logPrefix "[notice] "

infoPrefix :: SetLoggerEntry
infoPrefix = toStdout . logPrefix ""

withLogging :: (MonadIO m) => m a -> m ()
withLogging m = do
  setLogging @TRACE tracePrefix
  setLogging @DEBUG debugPrefix
  setLogging @INFO infoPrefix
  setLogging @ERROR errorPrefix
  setLogging @WARN warnPrefix
  setLogging @NOTICE noticePrefix

  m

  setLoggingOff @TRACE
  setLoggingOff @DEBUG
  setLoggingOff @INFO
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE

green :: Doc AnsiStyle -> Doc AnsiStyle
green = annotate (color Green)

yellow :: Doc AnsiStyle -> Doc AnsiStyle
yellow = annotate (color Yellow)

red :: Doc AnsiStyle -> Doc AnsiStyle
red = annotate (color Red)

blue :: Doc AnsiStyle -> Doc AnsiStyle
blue = annotate (color Blue)