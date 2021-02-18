{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Cardano.Metadata.Server.Types
  ( tests
  ) where

import           Data.List (delete, find, sort)
import           Data.Functor.Identity (Identity(Identity))
import           Data.Monoid (Sum (Sum), getSum, First(First))
import           Data.Ratio (Ratio, (%))
import           Data.Word (Word8)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import Data.ByteArray.Encoding
    ( Base (Base16, Base64), convertFromBase, convertToBase )
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as Aeson
import           Text.Read (readEither)
import           Text.RawString.QQ
import           qualified Data.HashMap.Strict as HM
import           Hedgehog (Gen, MonadTest, annotate, forAll, property, tripping, (===), footnote, failure)
import qualified Hedgehog as H (Property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Aeson (ToJSON, FromJSON)
import           Test.Tasty (TestTree, testGroup)
import           Network.URI (parseURI, URI(URI))
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit (Assertion, assertEqual, testCase, (@?=))

import qualified Test.Cardano.Metadata.Generators as Gen

import Cardano.Metadata.Server.Types

tests :: TestTree
tests = testGroup "Metadata type tests"
  [ testGroup "Parsers and printers"
      [ testProperty "HashFn/read/show/roundtrips" (prop_read_show_roundtrips Gen.hashFn)
      , testCase     "HashFn/show/matches-spec" unit_hashfn_show_spec
      , testProperty "HashFn/json/roundtrips" (prop_json_roundtrips Gen.hashFn)
      , testProperty "HashFn/json/matches-spec-keys" (prop_json_read_show_align_spec Gen.hashFn)

      , testProperty "AssetLogo/json/roundtrips" (prop_json_roundtrips Gen.assetLogo)
      , testCase "AssetLogo/json/matches-spec" unit_assetLogo_json_spec

      , testProperty "AssetURL/json/roundtrips" (prop_json_roundtrips Gen.assetURL)
      , testCase "AssetURL/json/matches-spec" unit_assetURL_json_spec

      , testProperty "AssetUnit/json/roundtrips" (prop_json_roundtrips Gen.assetUnit)
      , testCase "AssetUnit/json/matches-spec" unit_assetUnit_json_spec

      , testProperty "Subject/json/roundtrips" (prop_json_roundtrips Gen.subject)
      , testCase "Subject/json/matches-spec" unit_subject_json_spec

      , testProperty "AnnotatedSignature/json/roundtrips" (prop_json_roundtrips Gen.annotatedSignature)
      , testProperty "AnnotatedSignature/json/matches-spec-keys" (prop_json_only_has_keys Gen.annotatedSignature ["publicKey", "signature"])

      , testProperty "Property/json/roundtrips" (prop_json_roundtrips (Gen.metadataProperty Gen.metadataValue))

      , testProperty "PreImage/json/roundtrips" (prop_json_roundtrips Gen.preImage)
      , testProperty "PreImage/json/matches-spec-keys" (prop_json_only_has_keys Gen.preImage ["value", "hashFn"])

      , testProperty "Owner/json/roundtrips" (prop_json_roundtrips Gen.owner)
      , testProperty "Owner/json/matches-spec-keys" (prop_json_only_has_keys Gen.owner ["publicKey", "signature"])

      , testProperty "Entry/json/roundtrips" (prop_json_roundtrips Gen.entry)
      , testCase     "Entry/json/matches-spec" unit_entry_json_spec
      , testProperty "PartialEntry/json/roundtrips" (prop_json_roundtrips Gen.partialEntry)

      , testProperty "BatchRequest/json/roundtrips" (prop_json_roundtrips Gen.batchRequest)
      , testProperty "BatchRequest/json/matches-spec-keys" (prop_json_only_has_keys Gen.batchRequest ["subjects", "properties"])
      , testCase     "BatchRequest/json/matches-spec" unit_batchRequest_json_spec

      , testProperty "BatchResponse/json/roundtrips" (prop_json_roundtrips Gen.batchResponse)
      , testCase     "BatchResponse/json/matches-spec" unit_batch_response_json_spec
      ]
  ]

prop_read_show_roundtrips :: (Show a, Eq a, Read a) => Gen a -> H.Property
prop_read_show_roundtrips gen = property $ do
  a <- forAll gen

  tripping a show readEither

prop_json_roundtrips :: (Show a, Eq a, ToJSON a, FromJSON a) => Gen a -> H.Property
prop_json_roundtrips gen = property $ do
  a <- forAll gen

  tripping a Aeson.toJSON Aeson.fromJSON

unit_assetLogo_json_spec :: Assertion
unit_assetLogo_json_spec = do
  let xs = take (assetLogoMaxLength + 1) $ repeat 0
      ys = mempty
      asJSON = Aeson.String . T.decodeUtf8 . convertToBase Base64 . B.pack
      asAssetLogo = AssetLogo . Encoded . convertToBase Base64 . B.pack

  Aeson.fromJSON (asJSON xs) @?= (Aeson.Error "Length must be no more than 65536 bytes, got 65537" :: Aeson.Result AssetLogo)
  Aeson.fromJSON (asJSON ys) @?= Aeson.Success (asAssetLogo ys)

unit_assetURL_json_spec :: Assertion
unit_assetURL_json_spec = do
  let goodURL    = "https://www.google.com"
      badURL     = "http://www.google.com"
      asJSON     = Aeson.String
      asAssetURL = AssetURL . fromJust . parseURI . T.unpack

  Aeson.fromJSON (asJSON badURL) @?= (Aeson.Error "Scheme must be https: but got http:" :: Aeson.Result AssetURL)
  Aeson.fromJSON (asJSON goodURL) @?= Aeson.Success (asAssetURL goodURL)

unit_subject_json_spec :: Assertion
unit_subject_json_spec = do
  let badSubject1 = Aeson.String ""
      badSubject2 = Aeson.String $ T.pack $ take 257 $ repeat 'a'
      goodSubject = Aeson.String $ T.pack $ take 256 $ repeat 'a'

  Aeson.fromJSON badSubject1 @?= (Aeson.Error "Length must be at least 1 characters, got 0" :: Aeson.Result Subject)
  Aeson.fromJSON badSubject2 @?= (Aeson.Error "Length must be no more than 256 characters, got 257" :: Aeson.Result Subject)
  Aeson.fromJSON goodSubject @?= (Aeson.Success $ Subject $ T.pack $ take 256 $ repeat 'a')

unit_assetUnit_json_spec :: Assertion
unit_assetUnit_json_spec = do
  let badDecimals1 = Aeson.Object $ HM.fromList $
        [ ("decimals", Aeson.Number (-1))
        , ("name", Aeson.String "steve")
        ]
      badDecimals2 = Aeson.Object $ HM.fromList $
        [ ("decimals", Aeson.Number 20)
        , ("name", Aeson.String "steve")
        ]
      badDecimals3 = Aeson.Object $ HM.fromList $
        [ ("decimals", Aeson.Number 0)
        , ("name", Aeson.String "steve")
        ]
      badName1 = Aeson.Object $ HM.fromList $
        [ ("name", Aeson.String "")
        , ("decimals", Aeson.Number 1)
        ]
      badName2 = Aeson.Object $ HM.fromList $
        [ ("name", Aeson.String $ T.pack $ take 31 $ repeat '1')
        , ("decimals", Aeson.Number 1)
        ]
      good = Aeson.Object $ HM.fromList $
        [ ("name", Aeson.String "hello")
        , ("decimals", Aeson.Number 1)
        ]

  Aeson.fromJSON badDecimals1 @?= (Aeson.Error "parsing Natural failed, unexpected negative number -1" :: Aeson.Result AssetUnit)
  Aeson.fromJSON badDecimals2 @?= (Aeson.Error "Number of decimals must be less than 19, got 20" :: Aeson.Result AssetUnit)
  Aeson.fromJSON badDecimals3 @?= (Aeson.Error "Number of decimals must be greater than 1, got 0" :: Aeson.Result AssetUnit)
  Aeson.fromJSON badName1     @?= (Aeson.Error "Length must be at least 1 characters, got 0" :: Aeson.Result AssetUnit)
  Aeson.fromJSON badName2     @?= (Aeson.Error "Length must be no more than 30 characters, got 31" :: Aeson.Result AssetUnit)
  Aeson.fromJSON good         @?= (Aeson.Success $ AssetUnit "hello" 1)

unit_hashfn_show_spec :: Assertion
unit_hashfn_show_spec = do
  show Blake2b256 @?= "blake2b-256"
  show Blake2b224 @?= "blake2b-224"
  show SHA256     @?= "sha256"

unit_batchRequest_json_spec :: Assertion
unit_batchRequest_json_spec = do
  let jsonNoProperties = [r|
  {
    "subjects": [
      "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0"
    ]
  }
  |]

  Aeson.eitherDecode jsonNoProperties
    @?= (Right $ BatchRequest ["7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0"] Nothing)

  let jsonProperties = [r|
  {
    "subjects": [
      "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0"
    ],
    "properties": [
      "owner",
      "description",
      "bad"
    ]
  }
  |]

  Aeson.eitherDecode jsonProperties
    @?= (Right $ BatchRequest ["7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0"] (Just ["owner", "description", "bad"]))

  let jsonPropertiesEmpty = [r|
  {
    "subjects": [
      "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0"
    ],
    "properties": []
  }
  |]

  Aeson.eitherDecode jsonPropertiesEmpty
    @?= (Right $ BatchRequest ["7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0"] (Just []))

unit_entry_json_spec :: Assertion
unit_entry_json_spec = do
  let json = [r|
  {
      "subject": "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0",
      "owner": {
          "signature": "62e800b8c540b218396174f9c42fc253ab461961e20a4cc8ed4ba8b3fdff760cf8422e80d2504829a1d84458093880f02629524416f895b802cb9211f5145808",
          "publicKey": "25912b3081c20782aaa576af51ef3b17d7370d9fdf6641fec28012678ac1d179"
      },
      "description": {
          "value": "A sample description",
          "anSignatures": [
              {
                  "signature": "83ef5c04882e43e5f1c8e9bc386bd51cdda163f5cbd1996d1d066238de063d4b79b1648b48aec63dddff05649911ca116579842c8e9a08a3bc7ae1a0ec7ef000",
                  "publicKey": "1446c9d327b0f07aa691014c08578867674f3a88b36f2017a58c37a8a7799058"
              },
              {
                  "signature": "4e29a00feaeb24b25315f0eac28bbfc550dabfb847bf6a06cb8086120201f90c64fab778037d0ef009ab4669121a38fe9b8c0a6aec99c68366c5187c0889520a",
                  "publicKey": "1910312a9a6998c7e4f585dc138f85a90f50a28397b8ea05eb23355fb8ea4fa0"
              },
              {
                  "signature": "ce939acca5677bc6d436bd8f054ed8fb03d143e0a9792c1f58592c43f175e89bb72d4d7114c1474b86e0d8fbf7807f4506325b56fcc6b87b2cb7002872527106",
                  "publicKey": "4c5bbbbe7caaa18372aa8edc1ef2d2a770d18a5c2d142b9d695619c3365dd297"
              },
              {
                  "signature": "5a1d55048234d92057dfd1938f49935a33751ee604b7dbd02a315418ced6f0836a51107512b192eae6133403bb437c6850b1af1c62c3b17a372acce77adf9903",
                  "publicKey": "57fa73123c3b39489c4d6c2ff3cab9952e56e556daab9f8f333bc5ca6984fa5e"
              },
              {
                  "signature": "e13c9ba5b084dc126d34f3f1120fff75495b64a41a98a69071b5c5ed01bb9d273f51d570cf4fdaa42969fa2c775c12ec05c496cd8f61323d343970136781f60e",
                  "publicKey": "8cc8963b65ddd0a49f7ce1acc2915d8baff505bbc4f8727a22bd1d28f8ad6632"
              }
          ]
      },
      "name": {
          "value": "SteveToken",
          "anSignatures": [
              {
                  "signature": "7ef6ed44ba9456737ef8d2e31596fdafb66d5775ac1a254086a553b666516e5895bb0c6b7ba8bef1f6b4d9bd9253b4449d1354de2f9e043ea4eb43fd42f87108",
                  "publicKey": "0ee262f062528667964782777917cd7139e19e8eb2c591767629e4200070c661"
              },
              {
                  "signature": "c95cf87b74d1e4d3b413c927c65de836f0905ba2cd176c7cbff83d8b886b30fe1560c542c1f77bb88280dff55c2d267c9840fe36560fb13ba4a78b6429e51500",
                  "publicKey": "7c3bfe2a11290a9b6ea054b4d0932678f88130511cfbfe3f634ee77d71edebe7"
              },
              {
                  "signature": "f88692b13212bac8121151a99a4de4d5244e5f63566babd2b8ac20950ede74073af0570772b3ce3d11b72e972079199f02306e947cd5fcca688a9d4664eddb04",
                  "publicKey": "8899d0777f399fffd44f72c85a8aa51605123a7ebf20bba42650780a0c81096a"
              },
              {
                  "signature": "c2b30fa5f2c09323d81e5050af681c023089d832d0b85d05f60f4278fba3011ab03e6bd9bd2b8649080a368ecfe51573cd232efe8f1e7ca69ff8334ced7b6801",
                  "publicKey": "d40688a3eeda1f229c64efc56dd53b363ff981f71a7462f78c8cc444117a03db"
              }
          ]
      },
      "acronym": { "value": "Stve" },
      "unit": { "value": { "name": "Steves", "decimals": 1 } },
      "url": { "value": "https://www.google.com" },
      "logo": { "value": "AA==", "anSignatures": [] }
  }
  |]
  (Aeson.eitherDecode json :: Either String Entry')
    @?= Right ( Entry'
                  "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0"
                  $ Entry
                    ( GenericProperty
                        "SteveToken"
                        [ (AnnotatedSignature "7ef6ed44ba9456737ef8d2e31596fdafb66d5775ac1a254086a553b666516e5895bb0c6b7ba8bef1f6b4d9bd9253b4449d1354de2f9e043ea4eb43fd42f87108" "0ee262f062528667964782777917cd7139e19e8eb2c591767629e4200070c661")
                        , (AnnotatedSignature "c95cf87b74d1e4d3b413c927c65de836f0905ba2cd176c7cbff83d8b886b30fe1560c542c1f77bb88280dff55c2d267c9840fe36560fb13ba4a78b6429e51500" "7c3bfe2a11290a9b6ea054b4d0932678f88130511cfbfe3f634ee77d71edebe7")
                        , (AnnotatedSignature "f88692b13212bac8121151a99a4de4d5244e5f63566babd2b8ac20950ede74073af0570772b3ce3d11b72e972079199f02306e947cd5fcca688a9d4664eddb04" "8899d0777f399fffd44f72c85a8aa51605123a7ebf20bba42650780a0c81096a")
                        , (AnnotatedSignature "c2b30fa5f2c09323d81e5050af681c023089d832d0b85d05f60f4278fba3011ab03e6bd9bd2b8649080a368ecfe51573cd232efe8f1e7ca69ff8334ced7b6801" "d40688a3eeda1f229c64efc56dd53b363ff981f71a7462f78c8cc444117a03db")
                        ]
                    )
                    ( GenericProperty
                        "A sample description"
                        [ (AnnotatedSignature "83ef5c04882e43e5f1c8e9bc386bd51cdda163f5cbd1996d1d066238de063d4b79b1648b48aec63dddff05649911ca116579842c8e9a08a3bc7ae1a0ec7ef000" "1446c9d327b0f07aa691014c08578867674f3a88b36f2017a58c37a8a7799058")
                        , (AnnotatedSignature "4e29a00feaeb24b25315f0eac28bbfc550dabfb847bf6a06cb8086120201f90c64fab778037d0ef009ab4669121a38fe9b8c0a6aec99c68366c5187c0889520a" "1910312a9a6998c7e4f585dc138f85a90f50a28397b8ea05eb23355fb8ea4fa0")
                        , (AnnotatedSignature "ce939acca5677bc6d436bd8f054ed8fb03d143e0a9792c1f58592c43f175e89bb72d4d7114c1474b86e0d8fbf7807f4506325b56fcc6b87b2cb7002872527106" "4c5bbbbe7caaa18372aa8edc1ef2d2a770d18a5c2d142b9d695619c3365dd297")
                        , (AnnotatedSignature "5a1d55048234d92057dfd1938f49935a33751ee604b7dbd02a315418ced6f0836a51107512b192eae6133403bb437c6850b1af1c62c3b17a372acce77adf9903" "57fa73123c3b39489c4d6c2ff3cab9952e56e556daab9f8f333bc5ca6984fa5e")
                        , (AnnotatedSignature "e13c9ba5b084dc126d34f3f1120fff75495b64a41a98a69071b5c5ed01bb9d273f51d570cf4fdaa42969fa2c775c12ec05c496cd8f61323d343970136781f60e" "8cc8963b65ddd0a49f7ce1acc2915d8baff505bbc4f8727a22bd1d28f8ad6632")
                        ]
                    )
                    ( Just $ Owner
                       "62e800b8c540b218396174f9c42fc253ab461961e20a4cc8ed4ba8b3fdff760cf8422e80d2504829a1d84458093880f02629524416f895b802cb9211f5145808"
                       "25912b3081c20782aaa576af51ef3b17d7370d9fdf6641fec28012678ac1d179"
                    )
                    ( Just $ GenericProperty "Stve" [])
                    ( Just $ GenericProperty (AssetURL $ fromJust $ parseURI "https://www.google.com") [])
                    ( Just $ GenericProperty (AssetLogo (Encoded "\NUL")) [])
                    ( Just $ GenericProperty (AssetUnit "Steves" 1) [])
              )

  let json2 = [r|
        {
          "subject": "extra fields",
          "name": { "value": "Token2" },
          "description": { "value": "description2" },
          "acronym": { "value": "acr2" },
          "unit": { "value": { "decimals": 14, "name": "unit2" } },
          "url": { "value": "https://iohk.io" },
          "logo": { "value": "QWxtb3N0IGEgbG9nbw==" }
        }|]
  (Aeson.eitherDecode json2 :: Either String Entry')
    @?= Right ( Entry'
                  "extra fields"
                  $ Entry
                    ( GenericProperty "Token2" [])
                    ( GenericProperty
                        "description2" []
                    )
                    Nothing
                    ( Just $ GenericProperty "acr2" [])
                    ( Just $ GenericProperty (AssetURL $ fromJust $ parseURI "https://iohk.io") [])
                    ( Just $ GenericProperty (AssetLogo (Encoded "Almost a logo")) [])
                    ( Just $ GenericProperty (AssetUnit "unit2" 14) [] )
              )

unit_batch_response_json_spec :: Assertion
unit_batch_response_json_spec = do
  let
    expected = [r|
      {
        "subjects": [
            {
                "subject": "44b57ee30cdb55829d0a5d4f046baef078f1e97a7f21b62d75f8e96ea139c35f",
                "name": {
                    "value": "Wallet #6",
                    "anSignatures": [
                        {
                            "signature": "2e27065e365d38bef19b7bec139206f99b00effc8a2ad05bd22259aa939dd5083f25da91c4cb764eb1bfbce243ec32cce112be9762e1da7a38e975ebb0cc0b08",
                            "publicKey": "44b57ee30cdb55829d0a5d4f046baef078f1e97a7f21b62d75f8e96ea139c35f"
                        }
                    ]
                }
            },
            {
                "subject": "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0",
                "description": {
                    "value": "rex",
                    "anSignatures": [
                        {
                            "signature": "7ef6ed44ba9456737ef8d2e31596fdafb66d5775ac1a254086a553b666516e5895bb0c6b7ba8bef1f6b4d9bd9253b4449d1354de2f9e043ea4eb43fd42f87108",
                            "publicKey": "0ee262f062528667964782777917cd7139e19e8eb2c591767629e4200070c661"
                        }
                    ]
                }
            }
        ]
      }
  |]

  (Aeson.eitherDecode expected :: Either String BatchResponse)
    @?=
    (Right $ BatchResponse
      [ ( PartialEntry'
            "44b57ee30cdb55829d0a5d4f046baef078f1e97a7f21b62d75f8e96ea139c35f"
            $ PartialEntry $ EntryF
              (First Nothing)
              (First $ Just $ GenericProperty "Wallet #6" [AnnotatedSignature "2e27065e365d38bef19b7bec139206f99b00effc8a2ad05bd22259aa939dd5083f25da91c4cb764eb1bfbce243ec32cce112be9762e1da7a38e975ebb0cc0b08" "44b57ee30cdb55829d0a5d4f046baef078f1e97a7f21b62d75f8e96ea139c35f"])
              (First Nothing)
              (First Nothing)
              (First Nothing)
              (First Nothing)
              (First Nothing)
        )
      , ( PartialEntry'
            "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0"
            $ PartialEntry $ EntryF
              (First Nothing)
              (First Nothing)
              (First $ Just $ GenericProperty "rex" [AnnotatedSignature "7ef6ed44ba9456737ef8d2e31596fdafb66d5775ac1a254086a553b666516e5895bb0c6b7ba8bef1f6b4d9bd9253b4449d1354de2f9e043ea4eb43fd42f87108" "0ee262f062528667964782777917cd7139e19e8eb2c591767629e4200070c661"])
              (First Nothing)
              (First Nothing)
              (First Nothing)
              (First Nothing)
        )
      ]
    )

-- The from/to JSON instances should simply match the show/read
-- instances.
prop_json_read_show_align_spec :: forall a. (Eq a, Show a, Read a, ToJSON a, FromJSON a) => Gen a -> H.Property
prop_json_read_show_align_spec gen = property $ do
  a <- forAll gen

  (Aeson.String $ T.pack $ show a) === (Aeson.toJSON a)
  (Aeson.eitherDecode $ BLC.pack $ "\"" <> show a <> "\"" :: Either String a) === (readEither $ show a)

-- prop_json_only_has_keys Gen.owner              ["publicKey", "signature"]
-- prop_json_only_has_keys Gen.annotatedSignature ["publicKey", "signature"]
-- prop_json_only_has_keys Gen.entry              ["description", "name", "owner", "preImage", "subject"]
-- prop_json_only_has_keys Gen.metadataProperty   ["value", "anSignatures"]

prop_json_only_has_keys :: (Show a, ToJSON a) => Gen a -> [Text] -> H.Property
prop_json_only_has_keys genA keys = property $ do
  a <- forAll genA

  a `onlyHasKeys` keys

onlyHasKeys :: (MonadTest m, ToJSON a) => a -> [Text] -> m ()
onlyHasKeys a ks =
  case Aeson.toJSON a of
    Aeson.Object obj -> do
      sort (HM.keys obj) === sort ks
    x                -> do
      footnote $ "Expected Aeson Object but got: " <> show x
      failure
