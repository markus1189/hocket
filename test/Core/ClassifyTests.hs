{-# LANGUAGE OverloadedStrings #-}

module Core.ClassifyTests (classifyTests, failure, failureWithXError, response) where

import Control.Exception (toException)
import Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import qualified Data.Sequence as Seq
import Data.String (fromString)
import Gen (clientErrorRows, genClientErrorRow)
import Hedgehog (cover, forAll, property, withTests, (===))
import Hocket.Adapter.Raindrop (ErrorScope (..), classify)
import Network.HTTP.Types (http11, mkStatus)
import Servant.Client.Core (BaseUrl (..), ClientError (..), Response, ResponseF (..), Scheme (Https), defaultRequest)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog (testProperty)

response :: Int -> Response
response code = Response (mkStatus code "") Seq.empty http11 ""

failure :: Int -> ClientError
failure = FailureResponse (bimap (const ()) (const (BaseUrl Https "api.raindrop.io" 443 "", "")) defaultRequest) . response

-- | A failure response carrying Raindrop's @x-error@ header.
failureWithXError :: Int -> BS.ByteString -> ClientError
failureWithXError code msg = case failure code of
  FailureResponse req r -> FailureResponse req r {responseHeaders = Seq.fromList [("x-error", msg)]}
  other -> other

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
      -- Each row is 1 in 10. At 100 tests some row falls under 3% in about
      -- 2% of runs (seen locally); at 1000 a row averages 100 hits
      -- and the 30-hit floor is over 7 standard deviations away.
      testProperty "classify maps every generated ClientError to its labelled scope" $ withTests 1000 $ property $ do
        (row, err, scope) <- forAll genClientErrorRow
        for_ clientErrorRows $ \(name, _, _) -> cover 3 (fromString name) (row == name)
        classify err === scope
    ]
