-- | What 'Network.Bookmark.App.Update.update' asks the shell to do; the
-- shell's interpreter is the only place these become IO.
module Network.Bookmark.App.Effect
  ( Effect (..),
    Opener (..),
  )
where

import Data.Text (Text)

-- | Bookmarks open in firefox; Raindrop's edit page with the desktop default.
data Opener = Firefox | XdgOpen
  deriving (Show, Eq)

data Effect
  = Halt
  | OpenUrl !Opener !Text
  | -- | The raw link; the clipboard adapter cleans it, as 'browseItem' does.
    CopyText !Text
  deriving (Show, Eq)
