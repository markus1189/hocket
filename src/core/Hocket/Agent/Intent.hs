{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Agent writes as the 'Intent' the keyboard would produce (plan §11 #19).
-- Separate from "Hocket.Agent.Protocol" because
-- "Hocket.App.Msg" imports 'FlagAction' from there.
module Hocket.Agent.Intent (toIntent) where

import Hocket.Agent.Protocol (WriteCmd (..))
import Hocket.App.Msg

-- | The Intent a write stands for, the one its key produces, so an agent
-- reaches no state a key could not. The server's @writeMsg@ samples 'Now'
-- once per write.
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
