-- | @hocket tui --fixture@: the command-line side of the fixture backend
-- (plan D8). The store itself is "Network.Bookmark.Fake".
module Fixture
  ( FixtureOpts (..),
    fixtureParser,
    fixtureStore,
  )
where

import Control.Lens ((.~))
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Network.Bookmark.Fake (FailRule, FakeRaindrop, frRules, parseFixtureFail, seedFake)
import Options.Applicative

data FixtureOpts = FixtureOpts
  { foSeed :: !FilePath,
    foFail :: ![FailRule],
    foDump :: !(Maybe FilePath),
    -- | The config's archive collection id, which fixture mode never reads.
    foArchive :: !Int
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

-- | The store a fixture run starts from: the seed's items and the failure rules.
fixtureStore :: FixtureOpts -> LBS.ByteString -> Either String FakeRaindrop
fixtureStore fo seed = (frRules .~ foFail fo) <$> seedFake (foArchive fo) seed
