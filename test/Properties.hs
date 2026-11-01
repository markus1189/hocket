{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Properties (propertyTests) where

import Control.Lens ((^.))
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Time.Clock (addUTCTime)
import Gen
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.Bookmark.Agent.Protocol
import Network.Bookmark.Agent.Snapshot
import Network.Bookmark.Types
import Network.Bookmark.Ui.State
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

propertyTests :: TestTree
propertyTests =
  testGroup
    "Properties"
    [ -- The CLI builds requests with 'encodeCmd' and the server reads them
      -- with 'decodeCmd'; a disagreement makes `hocket agent` emit requests
      -- the socket rejects.
      testProperty "decodeCmd . encodeCmd round-trips every command" $ property $ do
        cmd <- forAll genAgentCmd
        tripping cmd encodeCmd (\(m, p) -> decodeCmd m (Just p)),
      testProperty "BookmarkItem JSON round-trips" $ property $ do
        item <- forAll genBookmarkItem
        tripping item A.encode (A.eitherDecode @BookmarkItem),
      testProperty "AgentSnapshot JSON round-trips" $ property $ do
        snap <- forAll genAgentSnapshot
        tripping snap A.encode (fmap unWire . A.eitherDecode),
      testProperty "insertItems is independent of insertion order" $ property $ do
        start <- forAllWith (show . (^. hsContents)) genModel
        items <- forAll genItemVersions
        shuffled <- forAll (Gen.shuffle items)
        insertItems shuffled start ^. hsContents === insertItems items start ^. hsContents
    ]

-- | Several versions per id with distinct '_biLastUpdate': Raindrop never
-- sends two different bodies with the same id and timestamp, so ties are not
-- a case 'insertItem' has to order.
genItemVersions :: Gen [BookmarkItem]
genItemVersions = concat <$> Gen.list (Range.linear 0 15) versions
  where
    versions = do
      base <- genBookmarkItem
      n <- Gen.int (Range.linear 1 4)
      titles <- Gen.list (Range.singleton n) (Gen.text (Range.linear 0 10) Gen.alphaNum)
      pure
        [ base {_biLastUpdate = addUTCTime (fromIntegral k) (_biLastUpdate base), _biTitle = t}
        | (k, t) <- zip [0 :: Int ..] titles
        ]

-- | Test-only decoder: the snapshot is write-only in the library, so this
-- inverts its 'ToJSON' to give 'tripping' something to compare.
newtype Wire = Wire {unWire :: AgentSnapshot}

instance FromJSON Wire where
  parseJSON = withObject "snapshot" $ \o ->
    fmap Wire $
      AgentSnapshot
        <$> o .: "version"
        <*> (map unWireItem <$> o .: "items")
        <*> (fmap BookmarkItemId <$> o .: "selected")
        <*> o .: "filter_query"
        <*> (o .: "video_filter" >>= videoFilterP)
        <*> o .: "show_future_reminders"
        <*> o .: "status"
        <*> o .: "last_updated"
        <*> (o .: "async_op" >>= traverse asyncOpP)

newtype WireItem = WireItem {unWireItem :: SnapshotItem}

instance FromJSON WireItem where
  parseJSON = withObject "item" $ \o ->
    fmap WireItem $
      SnapshotItem . BookmarkItemId
        <$> o .: "id"
        <*> o .: "title"
        <*> o .: "link"
        <*> o .: "tags"
        <*> o .: "note"
        <*> o .: "excerpt"
        <*> o .: "created"
        <*> o .: "reminder"
        <*> o .: "favorite"
        <*> (o .: "pending" >>= pendingP)
        <*> o .: "visible"
    where
      pendingP = withObject "pending" $ \p ->
        p .: "action" >>= \a -> case a :: Text of
          "none" -> pure None
          "archive" -> pure ToBeArchived
          "reminder" -> ToBeReminded <$> p .: "at"
          "remove_reminder" -> pure ReminderToBeRemoved
          other -> fail ("unknown pending action: " <> show other)

videoFilterP :: Text -> Parser VideoFilterMode
videoFilterP t = maybe (fail ("unknown video filter: " <> show t)) pure (lookup t [(videoFilterModeToText m, m) | m <- [NoVideoFilter, ShowOnlyVideos, HideVideos]])

asyncOpP :: Text -> Parser AsyncOp
asyncOpP t = maybe (fail ("unknown async op: " <> show t)) pure (lookup t [(asyncOpToText op, op) | op <- [OpFetchItems, OpExecuteBatch]])
