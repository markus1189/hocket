-- | What 'Network.Bookmark.App.Update.update' asks the shell to do; the
-- shell's interpreter is the only place these become IO.
module Network.Bookmark.App.Effect (Effect (..)) where

data Effect = Halt
  deriving (Show, Eq)
