-- | What the async workers post, as pure functions of what their calls
-- returned, so the posting rules can be tested without IO.
module Worker
  ( fetchResultEvents,
  )
where

import Data.Text (Text)
import Events
import Network.Bookmark.Types (BookmarkItemBatch (..))

-- | The events a finished fetch posts, given the rendered error or the pages.
fetchResultEvents :: Bool -> Either (Maybe Text) [BookmarkItemBatch] -> [HocketEvent]
fetchResultEvents _ (Left err) = [asyncActionFailedEvt err]
fetchResultEvents isUpdateFetch (Right batches) =
  setStatusEvt Nothing : [fetchedItemsEvt ts bis isUpdateFetch | BookmarkItemBatch ts bis _ <- batches]
