{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Agent writes as the 'Intent' the keyboard would produce (plan §11 #19).
-- Separate from "Network.Bookmark.Agent.Protocol" because
-- "Network.Bookmark.App.Msg" imports 'FlagAction' from there.
module Network.Bookmark.Agent.Intent (toIntent) where

import Network.Bookmark.Agent.Protocol (WriteCmd (..))
import Network.Bookmark.App.Msg

-- | Mirrors the shell's @toEvent@: the same command reaches the same state
-- change. 'Now' replaces the reminder clock @toEvent@ sampled itself.
toIntent :: Now -> WriteCmd -> Intent
toIntent now = \case
  CmdSetFlag bid act -> SetFlag bid act now
  CmdClearFlags -> ClearAllFlags
  CmdFlagAllArchive -> FlagAllArchive
  CmdExecute -> Execute
  CmdRefresh -> Refresh
  CmdSetFilter q -> SetFilter q
  CmdSetVideoFilter m -> SetVideoFilter m
  CmdSetShowFutureReminders b -> SetShowFutureReminders b
  CmdSelectItem bid -> Select bid
  CmdOpenItem bid -> Open (ById bid)
  CmdOpenAndFlag bid -> OpenAndFlag bid
  CmdSetStatus t -> SetStatus ("agent: " <> t)
