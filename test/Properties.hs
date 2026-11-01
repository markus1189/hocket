{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Properties (propertyTests) where

import Control.Lens ((%~), (&), (.~), (^.))
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time.Clock (addUTCTime)
import qualified Data.Vector as V
import Gen
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hocket.Agent.Protocol
import Hocket.Agent.Snapshot
import Hocket.App.Model
import Hocket.Domain.Bookmark
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
      testProperty "settle keeps the selection visible, or Nothing exactly when nothing is (I4)" $ property $ do
        start <- forAllWith (show . (^. hsContents)) genModel
        -- Any prior selection: visible, hidden, unknown, or none.
        prior <- forAll (Gen.maybe (Gen.choice (genBookmarkItemId : [Gen.element (Map.keys (start ^. hsContents)) | not (Map.null (start ^. hsContents))])))
        let settled = settle (settle start & hsSelected .~ prior)
            visibleIds = V.map (^. biId) (settled ^. hsVisible)
        case settled ^. hsSelected of
          Nothing -> V.null visibleIds === True
          Just bid -> V.elem bid visibleIds === True
        (settle settled ^. hsSelected, settle settled ^. hsVisible) === (settled ^. hsSelected, settled ^. hsVisible),
      testProperty "insertItems is order-independent and keeps existing flags" $ property $ do
        start <- forAllWith (show . (^. hsContents)) genModel
        items <- forAll (genItemVersions (Map.keys (start ^. hsContents)))
        shuffled <- forAll (Gen.shuffle items)
        let result = insertItems items start ^. hsContents
        insertItems shuffled start ^. hsContents === result
        Map.map fst (start ^. hsContents) === Map.map fst (Map.intersection result (start ^. hsContents))
    ]

-- | Several versions per id with distinct 'biLastUpdate': Raindrop never
-- sends two different bodies with the same id and timestamp, so ties are not
-- a case 'insertItem' has to order. About half the ids are drawn from
-- @known@, so updates of flagged items are exercised too.
genItemVersions :: [BookmarkItemId] -> Gen [BookmarkItem]
genItemVersions known = concat <$> Gen.list (Range.linear 0 15) versions
  where
    versions = do
      base <- genBookmarkItem
      bid <- if null known then pure (base ^. biId) else Gen.choice [pure (base ^. biId), Gen.element known]
      n <- Gen.int (Range.linear 1 4)
      titles <- Gen.list (Range.singleton n) (Gen.text (Range.linear 0 10) Gen.alphaNum)
      pure
        [ base & biId .~ bid & biLastUpdate %~ addUTCTime (fromIntegral k) & biTitle .~ t
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
