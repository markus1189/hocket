{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Pocket.Retrieve ( AsFormParams (..)

                               , RetrieveConfig
                               , retrieveState
                               , retrieveFavorite
                               , retrieveTag
                               , retrieveContentType
                               , retrieveSort
                               , retrieveDetailType
                               , retrieveSearch
                               , retrieveDomain
                               , retrieveSince
                               , retrieveCount

                               , RetrieveState (..)
                               , RetrieveFavorite (..)
                               , RetrieveTag (..)
                               , RetrieveContentType (..)
                               , RetrieveSort (..)
                               , RetrieveDetailType (..)
                               , RetrieveCount (..)
                               ) where

import Control.Lens (view)
import Control.Lens.Operators
import Control.Lens.TH
import Data.Default (Default, def)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Network.Wreq (FormParam((:=)))
import Network.Wreq.Types (FormValue(..))

class AsFormParams a where
  toFormParams :: a -> [FormParam]

s :: String -> String
s = id

data RetrieveState = Unread | Archived | All
instance FormValue RetrieveState where
  renderFormValue Unread = renderFormValue . s $  "unread"
  renderFormValue Archived = renderFormValue . s $ "archive"
  renderFormValue All = renderFormValue . s $ "all"

data RetrieveFavorite = OnlyFavorites | OnlyNonFavorites
instance FormValue RetrieveFavorite where
  renderFormValue OnlyNonFavorites = renderFormValue (0 :: Integer)
  renderFormValue OnlyFavorites = renderFormValue (1 :: Integer)

data RetrieveTag = ExactTag Text | Untagged
instance FormValue RetrieveTag where
  renderFormValue (ExactTag t) = renderFormValue t
  renderFormValue Untagged = renderFormValue . s $ "_untagged_"

data RetrieveContentType = Article | Video | Image
instance FormValue RetrieveContentType where
  renderFormValue Article = renderFormValue . s $ "article"
  renderFormValue Video = renderFormValue . s $ "video"
  renderFormValue Image = renderFormValue . s $ "image"

data RetrieveSort = NewestFirst | OldestFirst | TitleAlpha | SiteAlpha
instance FormValue RetrieveSort where
  renderFormValue NewestFirst = renderFormValue . s $ "newest"
  renderFormValue OldestFirst = renderFormValue . s $ "oldest"
  renderFormValue TitleAlpha = renderFormValue . s $ "title"
  renderFormValue SiteAlpha = renderFormValue . s $ "site"

data RetrieveDetailType = Simple | Complete
instance FormValue RetrieveDetailType where
  renderFormValue Simple = renderFormValue . s $ "simple"
  renderFormValue Complete = renderFormValue . s $ "complete"

data RetrieveCount = Count Int | CountOffset Int Int | NoLimit
instance AsFormParams RetrieveCount where
  toFormParams (Count i) = ["count" := show i]
  toFormParams (CountOffset c o) = ["count" := show c, "offset" := show o]
  toFormParams NoLimit = []

data RetrieveConfig =
  RetrieveConfig { _retrieveState :: RetrieveState
                 , _retrieveFavorite :: Maybe RetrieveFavorite
                 , _retrieveTag :: Maybe RetrieveTag
                 , _retrieveContentType :: Maybe RetrieveContentType
                 , _retrieveSort :: Maybe RetrieveSort
                 , _retrieveDetailType :: Maybe RetrieveDetailType
                 , _retrieveSearch :: Maybe Text
                 , _retrieveDomain :: Maybe Text
                 , _retrieveSince :: Maybe POSIXTime
                 , _retrieveCount :: RetrieveCount
                 }
makeLenses ''RetrieveConfig


instance Default RetrieveConfig where
  def = RetrieveConfig { _retrieveState = Unread
                       , _retrieveFavorite = Nothing
                       , _retrieveTag = Nothing
                       , _retrieveContentType = Nothing
                       , _retrieveSort = Just NewestFirst
                       , _retrieveDetailType = Nothing
                       , _retrieveSearch = Nothing
                       , _retrieveDomain = Nothing
                       , _retrieveSince = Nothing
                       , _retrieveCount = NoLimit
                       }

instance AsFormParams RetrieveConfig where
  toFormParams rc = filter nonEmpty [ "state" := view retrieveState rc
                                    , "favorite" := view retrieveFavorite rc
                                    , "tag" := view retrieveTag rc
                                    , "contentType" := view retrieveContentType rc
                                    , "sort" := view retrieveSort rc
                                    , "detailType" := view retrieveDetailType rc
                                    , "search" := view retrieveSearch rc
                                    , "domain" := view retrieveDomain rc
                                    , "since" := (rc ^. retrieveSince <&> show)
                                    ] ++ toFormParams (view retrieveCount rc)

nonEmpty :: FormParam -> Bool
nonEmpty (_ := x) = renderFormValue x /= ""
