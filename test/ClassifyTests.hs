{-# LANGUAGE OverloadedStrings #-}

module ClassifyTests (classifyTests) where

import Control.Exception (toException)
import Data.Bifunctor (bimap)
import Data.Foldable (for_)
import qualified Data.Sequence as Seq
import Gen (genClientErrorWithScope)
import Hedgehog (cover, forAll, property, verifiedTermination, (===))
import Network.HTTP.Types (http11, mkStatus)
import Network.Raindrop (ErrorScope (..), classify)
import Servant.Client.Core (BaseUrl (..), ClientError (..), Response, ResponseF (..), Scheme (Https), defaultRequest)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog (testProperty)

response :: Int -> Response
response code = Response (mkStatus code "") Seq.empty http11 ""

failure :: Int -> ClientError
failure = FailureResponse (bimap (const ()) (const (BaseUrl Https "api.raindrop.io" 443 "", "")) defaultRequest) . response

-- | One row per D6 case (plan §5, rulings in §11).
classifyTests :: TestTree
classifyTests =
  testGroup
    "classify (D6 error scope)"
    [ testCase "item-scoped: any other 4xx fails only that item" $
        for_ [400, 404, 405, 409, 410, 422, 499] $ \c ->
          (c, classify (failure c)) @?= (c, ItemScoped),
      testCase "global: auth, timeout and rate limit hit every following call" $
        for_ [401, 403, 408, 429] $ \c ->
          (c, classify (failure c)) @?= (c, Global),
      testCase "global: 5xx" $
        for_ [500, 502, 503, 504, 599] $ \c ->
          (c, classify (failure c)) @?= (c, Global),
      testCase "global: 1xx and 3xx are not item errors" $
        for_ [100, 301, 302, 304] $ \c ->
          (c, classify (failure c)) @?= (c, Global),
      testCase "global: connection error" $
        classify (ConnectionError (toException (userError "offline"))) @?= Global,
      testCase "global: decode failure (the API shape changed)" $
        classify (DecodeFailure "no field result" (response 200)) @?= Global,
      testCase "global: unsupported content type" $
        classify (UnsupportedContentType "text/html" (response 200)) @?= Global,
      testCase "global: invalid content-type header" $
        classify (InvalidContentTypeHeader (response 200)) @?= Global,
      -- 1 of 10 generator rows is item-scoped; verified termination judges
      -- coverage statistically instead of failing on a low 100-sample draw.
      testProperty "classify maps every generated ClientError to its labelled scope" $ verifiedTermination $ property $ do
        (err, scope) <- forAll genClientErrorWithScope
        cover 5 "item-scoped" (scope == ItemScoped)
        cover 5 "global" (scope == Global)
        classify err === scope
    ]
