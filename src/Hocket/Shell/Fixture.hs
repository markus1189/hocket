{-# LANGUAGE OverloadedStrings #-}

-- | @hocket tui --fixture@: the command-line side of the fixture backend
-- (plan D8). The store itself is "Hocket.Adapter.Fake".
module Hocket.Shell.Fixture
  ( FixtureOpts (..),
    fixtureParser,
    fixtureStore,
    recordingSystemPort,
  )
where

import Control.Lens ((.~))
import Data.Aeson (encode, object, (.=))
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.Text as T
import Hocket.Adapter.Fake (FailRule, FakeRaindrop, frRules, parseFixtureFail, seedFake)
import Hocket.Domain.Bookmark (URL (..))
import Hocket.Port (SystemPort (..))
import Options.Applicative

data FixtureOpts = FixtureOpts
  { foSeed :: !FilePath,
    foFail :: ![FailRule],
    foDump :: !(Maybe FilePath),
    -- | The config's archive collection id, which fixture mode never reads.
    foArchive :: !Int,
    -- | Where the browser and clipboard calls go, as JSON lines.
    foDesktopLog :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

-- | The other fixture flags only parse together with @--fixture@.
fixtureParser :: Parser FixtureOpts
fixtureParser =
  FixtureOpts
    <$> strOption
      ( long "fixture"
          <> metavar "FILE"
          <> help "Serve a Raindrop list response from FILE instead of the real account; reads no config"
      )
    <*> many
      ( option
          (eitherReader (first T.unpack . parseFixtureFail . T.pack))
          ( long "fixture-fail"
              <> metavar "STEP[:N]=STATUS"
              <> help "Fail every (or the N-th) call of fetch | archive | set-reminders | remove-reminders with STATUS (HTTP code or conn); repeatable"
          )
      )
    <*> optional
      ( strOption
          ( long "fixture-dump"
              <> metavar "OUT"
              <> help "On exit, write the fixture store to OUT as a Raindrop list response"
          )
      )
    <*> option
      auto
      ( long "fixture-archive"
          <> metavar "ID"
          <> value 999
          <> showDefault
          <> help "Collection id that archiving moves items to"
      )
    <*> optional
      ( strOption
          ( long "fixture-desktop-log"
              <> metavar "OUT"
              <> help "Append the browser and clipboard calls to OUT as JSON lines; fixture mode never starts them"
          )
      )

-- | Fixture mode's desktop: nothing is opened or copied, so @e@ cannot
-- reach app.raindrop.io. With a log file every call is appended as one JSON
-- line, @{"open":{"program":…,"url":…}}@ or @{"copy":…}@.
recordingSystemPort :: Maybe FilePath -> SystemPort
recordingSystemPort mLog = SystemPort open copy
  where
    open program (URL url) = record (object ["open" .= object ["program" .= program, "url" .= url]])
    copy text = record (object ["copy" .= text])
    record v = mapM_ (\out -> LBS.appendFile out (encode v <> LBSC.pack "\n")) mLog

-- | The store a fixture run starts from: the seed's items and the failure rules.
fixtureStore :: FixtureOpts -> LBS.ByteString -> Either String FakeRaindrop
fixtureStore fo seed = (frRules .~ foFail fo) <$> seedFake (foArchive fo) seed
