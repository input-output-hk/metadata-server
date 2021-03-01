{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Metadata.Types.Common where

import           Control.DeepSeq              (NFData)
import           Data.Foldable (asum)
import           Data.Maybe (fromMaybe)
import           Data.Aeson                   (FromJSON, FromJSONKey, ToJSON,
                                               ToJSONKey, (.:))
import qualified Data.Aeson                   as Aeson
import           Data.Aeson.TH                (deriveJSON)
import qualified Data.Aeson.Types             as Aeson
import           Data.ByteArray.Encoding      (Base (Base16, Base64),
                                               convertFromBase, convertToBase)
import qualified Data.ByteString.Char8 as     BC
import qualified Data.ByteString.Lazy as BSL
import           Data.Hashable                (Hashable)
import qualified Data.HashMap.Strict          as HM
import           Data.String                  (IsString)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           GHC.Generics                 (Generic)
import           Quiet                        (Quiet (Quiet))
import           Text.Casing                  (fromHumps, toCamel)
import           Text.ParserCombinators.ReadP (choice, string)
import           Text.Read                    (readEither, readPrec)
import qualified Text.Read                    as Read (lift)
import           Web.HttpApiData              (FromHttpApiData)
import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash

-- | The metadata subject, the on-chain identifier
newtype Subject = Subject { unSubject :: Text }
  deriving (Generic, Eq, Ord, FromHttpApiData, ToJSONKey, FromJSONKey, ToJSON, FromJSON, Hashable)
  deriving newtype (IsString)
  deriving (Show) via (Quiet Subject)

newtype PropertyName = PropertyName { unPropertyName :: Text }
  deriving (Generic, Eq, Ord, FromHttpApiData, ToJSONKey, FromJSONKey, ToJSON, FromJSON, Hashable)
  deriving newtype (IsString)
  deriving (Show) via (Quiet PropertyName)

data Property value
  = Property { _propertyValue        :: value
             , _propertyAnSignatures :: Maybe [AnnotatedSignature]
             }
  deriving (Eq, Show)

propertyValue :: Property value -> value
propertyValue = _propertyValue

propertyAnSignatures :: Property value -> [AnnotatedSignature]
propertyAnSignatures = fromMaybe [] . _propertyAnSignatures

-- | A human-readable name for the metadata subject, suitable for use in an interface
type Name        = Property Text

-- | A human-readable description for the metadata subject, suitable for use in an interface
type Description = Property Text

-- | A pair of a public key, and a signature of the metadata entry by
-- that public key.
data AnnotatedSignature =
  AnnotatedSignature { asAttestationSignature :: SigDSIGN Ed25519DSIGN
                     , asPublicKey            :: VerKeyDSIGN Ed25519DSIGN
                     }
  deriving (Eq, Show)

-- | Hash functions supported by 'PreImage'.
data HashFn = Blake2b256
            | Blake2b224
            | SHA256
  deriving (Eq)

-- | A pair of a hash function identifier and a bytestring, such that
-- the bytestring is the preimage of the metadata subject under that hash
-- function.
data PreImage
  = PreImage { piValue  :: Text
             , piHashFn :: HashFn
             }
  deriving (Eq, Show)

-- | Public key and signature attesting to ownership of the metadata
-- entry in this registry.
data Owner
  = Owner { ownSignature :: Text
          , ownPublicKey :: Text
          }
  deriving (Eq, Show)

newtype Encoded (base :: Base) = Encoded
    { rawEncoded :: ByteString }
    deriving (Generic, Show, Eq, Ord, Semigroup, Monoid)

mkAnnotatedSignature :: forall val . ToJSON val => SignKeyDSIGN Ed25519DSIGN -> Subject -> PropertyName -> val -> AnnotatedSignature
mkAnnotatedSignature skey subj propName propVal =
  let
    hashSubj     = hashWith (T.encodeUtf8 . unSubject) subj :: Hash Blake2b_256 Subject
    hashPropName = hashWith (T.encodeUtf8 . unPropertyName) propName :: Hash Blake2b_256 PropertyName
    hashPropVal  = hashWith (BSL.toStrict . Aeson.encode) propVal :: Hash Blake2b_256 val

    h = hashWith id
      (  hashToBytes hashSubj
      <> hashToBytes hashPropName
      <> hashToBytes hashPropVal
      ) :: Hash Blake2b_256 ByteString
    publicKey = deriveVerKeyDSIGN skey
    sig       = signDSIGN () (hashToBytes h) skey
  in
    AnnotatedSignature sig publicKey

deserialiseBase16 :: Text -> Either Text ByteString
deserialiseBase16 t =
  case (convertFromBase Base16 . T.encodeUtf8 $ t) of
    Left err -> Left . T.pack $ "Failed to deserialise Base16 bytestring from text: '" <> T.unpack t <> "', error was: " <> err
    Right x  -> pure x

deserialiseAttestationSignature :: ByteString -> Either Text (SigDSIGN Ed25519DSIGN)
deserialiseAttestationSignature t =
  case rawDeserialiseSigDSIGN t of
    Nothing -> Left . T.pack $ "Failed to parse Ed25519DSIGN signature from '" <> BC.unpack t <> "'."
    Just x  -> pure x

deserialisePublicKey :: ByteString -> Either Text (VerKeyDSIGN Ed25519DSIGN)
deserialisePublicKey t =
  case rawDeserialiseVerKeyDSIGN t of
    Nothing -> Left . T.pack $ "Failed to parse Ed25519DSIGN verification key from '" <> BC.unpack t <> "'."
    Just x  -> pure x


-- Instances

instance FromJSON (Encoded 'Base16) where
  parseJSON = Aeson.withText "base16 bytestring" $
      either fail (pure . Encoded) . convertFromBase Base16 . T.encodeUtf8

instance ToJSON (Encoded 'Base16) where
  toJSON (Encoded raw) = Aeson.String $ T.decodeUtf8 $ convertToBase Base16 raw

instance FromJSON (Encoded 'Base64) where
  parseJSON = Aeson.withText "base64 bytestring" $
      either fail (pure . Encoded) . convertFromBase Base64 . T.encodeUtf8

instance ToJSON (Encoded 'Base64) where
  toJSON (Encoded raw) = Aeson.String $ T.decodeUtf8 $ convertToBase Base64 raw

instance NFData (Encoded 'Base16)
instance NFData (Encoded 'Base64)

instance ToJSON HashFn where
  toJSON = Aeson.String . T.pack . show

instance FromJSON HashFn where
  parseJSON = Aeson.withText "HashFn" (either Aeson.parseFail pure . readEither . T.unpack)

instance Show HashFn where
  show Blake2b256 = "blake2b-256"
  show Blake2b224 = "blake2b-224"
  show SHA256     = "sha256"

instance Read HashFn where
  readPrec = Read.lift $ choice [ Blake2b256 <$ string "blake2b-256"
                                , Blake2b224 <$ string "blake2b-224"
                                , SHA256     <$ string "sha256"
                                ]

instance ToJSON value => ToJSON (Property value) where
  toJSON (Property value Nothing)     = Aeson.toJSON value
  toJSON (Property value (Just sigs)) = Aeson.Object $ HM.fromList $
    [ ("value", Aeson.toJSON value)
    , ("anSignatures", Aeson.toJSON sigs)
    ]

instance FromJSON value => FromJSON (Property value) where
  parseJSON v =
    asum [ Aeson.withObject "Weakly-typed Property" (\obj -> Property <$> obj .: "value" <*> (Just <$> obj .: "anSignatures")) v
         , Property <$> Aeson.parseJSON v <*> pure Nothing
         ]

instance ToJSON AnnotatedSignature where
  toJSON (AnnotatedSignature sig pubKey) = Aeson.Object . HM.fromList $
    [ ("signature", Aeson.String $ T.decodeUtf8 $ convertToBase Base16 $ rawSerialiseSigDSIGN sig)
    , ("publicKey", Aeson.String $ T.decodeUtf8 $ convertToBase Base16 $ rawSerialiseVerKeyDSIGN pubKey)
    ]

instance FromJSON AnnotatedSignature where
  parseJSON = Aeson.withObject "AnnotatedSignature" $ \obj -> do
    AnnotatedSignature
    <$> (deserialiseSigDSIGN' =<< deserialiseBase16' =<< obj .: "signature")
    <*> (deserialiseVerKeyDSIGN' =<< deserialiseBase16' =<< obj .: "publicKey")

    where
      deserialiseBase16' :: Text -> Aeson.Parser ByteString
      deserialiseBase16' t =
        case (convertFromBase Base16 . T.encodeUtf8 $ t) of
          Left err -> fail $ "Failed to deserialise Base16 bytestring from text: '" <> T.unpack t <> "', error was: " <> err
          Right x  -> pure x

      deserialiseSigDSIGN' :: ByteString -> Aeson.Parser (SigDSIGN Ed25519DSIGN)
      deserialiseSigDSIGN' t =
        case rawDeserialiseSigDSIGN t of
          Nothing -> fail $ "Failed to parse Ed25519DSIGN signature from '" <> BC.unpack t <> "'."
          Just x  -> pure x

      deserialiseVerKeyDSIGN' :: ByteString -> Aeson.Parser (VerKeyDSIGN Ed25519DSIGN)
      deserialiseVerKeyDSIGN' t =
        case rawDeserialiseVerKeyDSIGN t of
          Nothing -> fail $ "Failed to parse Ed25519DSIGN verification key from '" <> BC.unpack t <> "'."
          Just x  -> pure x

$(deriveJSON Aeson.defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 3 } ''Owner)
$(deriveJSON Aeson.defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 2 } ''PreImage)