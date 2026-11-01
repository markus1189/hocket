{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The TUI's side effects on the desktop: opening URLs and the clipboard.
module System
  ( urlReplacements,
    cleanUrl,
    browseItem,
    copyToClipboard,
    runClipboardCmd,
    systemPort,
    systemNow,
  )
where

import Control.Lens (makeLensesFor, (&), (.~))
import Control.Monad (unless, void)
import Data.Foldable (for_)
#if MIN_VERSION_base(4,20,0)
import Data.List (isPrefixOf)
#else
import Data.List (foldl', isPrefixOf)
#endif
import Data.Maybe (isJust)
import Data.Time (getCurrentTime, getCurrentTimeZone)
import Network.Bookmark.App.Msg (Now (..))
import Network.Bookmark.Port (SystemPort (..))
import Network.Bookmark.Types (URL (..))
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (hClose, hPutStr)
import System.Process (CreateProcess, createProcess, proc, waitForProcess)
import System.Process.Internals (StdStream (CreatePipe))

makeLensesFor [("std_in", "stdIn"), ("std_err", "stdErr"), ("std_out", "stdOut")] ''CreateProcess

urlReplacements :: [(String, String)]
urlReplacements =
  [ ("m.imdb.", "imdb."),
    ("m.aliexpress.", "aliexpress.")
  ]

replace :: String -> String -> String -> String
replace old new = go
  where
    go [] = []
    go s@(x : xs)
      | old `isPrefixOf` s = new ++ go (drop (length old) s)
      | otherwise = x : go xs

cleanUrl :: String -> String
cleanUrl s = foldl' (\acc (old, new) -> replace old new acc) s urlReplacements

systemPort :: SystemPort
systemPort = SystemPort browseItem copyToClipboard

-- | The clock keys and agent writes are stamped with.
systemNow :: IO Now
systemNow = Now <$> getCurrentTime <*> getCurrentTimeZone

-- | 'proc', not 'shell': the URL comes from Raindrop and may contain quotes.
browseItem :: FilePath -> URL -> IO ()
browseItem program (URL url) = do
  let spec = proc program [cleanUrl url]
  (_, _, _, ph) <- createProcess $ spec & stdOut .~ CreatePipe & stdErr .~ CreatePipe
  void . waitForProcess $ ph

-- | Copy text to the system clipboard via the native tool: wl-copy on
-- Wayland, else xclip.
copyToClipboard :: String -> IO ()
copyToClipboard text = do
  wayland <- isJust <$> lookupEnv "WAYLAND_DISPLAY"
  if wayland
    then runClipboardCmd "wl-copy" [] text
    else runClipboardCmd "xclip" ["-selection", "clipboard"] text

-- | Spawn a tool, feed @text@ on stdin, close it for EOF, and wait; fail on
-- a nonzero exit.
runClipboardCmd :: String -> [String] -> String -> IO ()
runClipboardCmd cmd args text = do
  (mIn, _, _, ph) <- createProcess (proc cmd args & stdIn .~ CreatePipe)
  for_ mIn $ \h -> hPutStr h text >> hClose h
  exitCode <- waitForProcess ph
  unless (exitCode == ExitSuccess) $
    ioError (userError (cmd <> " exited with code " <> show exitCode))
