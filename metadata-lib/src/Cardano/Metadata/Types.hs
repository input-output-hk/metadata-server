{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Metadata.Types where

import Data.SOP
import Data.SOP.Dict
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import Data.Aeson (ToJSON, FromJSON, (.:), (.:?))
import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

data Property (name :: Symbol)
  = Property { value        :: PropertyValue name
             , anSignatures :: [Signature]
             }

type family PropertyValue (name :: Symbol) :: Type
type instance PropertyValue "test" = Text

data Metadata (propertyNames :: [Symbol])
  = Metadata { subject    :: Subject
             , properties :: NP Property propertyNames
             }

propertiesToJSONValues :: (All (KnownSymbol `And` (ToJSON `Compose` Property)) xs) => NP Property xs -> [(Text, Aeson.Value)]
propertiesToJSONValues = hcollapse . hcmap (Proxy :: Proxy (KnownSymbol `And` (ToJSON `Compose` Property))) (\(x :: Property name) -> K $ (T.pack $ symbolVal (Proxy @name), Aeson.toJSON x))

propertiesFromJSONValues :: (All (KnownSymbol `And` (FromJSON `Compose` Property)) xs) => Aeson.Object -> NP (Aeson.Result :.: Property) xs
propertiesFromJSONValues obj = hcmap (Proxy :: Proxy (KnownSymbol `And` (FromJSON `Compose` Property))) f (hpure Proxy)
  where
    f :: (KnownSymbol a, (FromJSON (Property a))) => Proxy a -> (:.:) Aeson.Result Property a
    f p =
      let
        key = T.pack $ symbolVal p
      in
        case HM.lookup key obj of
          Nothing      -> Comp (Aeson.Error $ "Missing key " <> show key)
          Just jsonVal -> Comp (Aeson.fromJSON jsonVal)

data Subject = Subject ()
  deriving (Eq, Show)

data Signature = Signature ()
  deriving (Eq, Show)

-- JSON encodings

instance ToJSON Subject where
  toJSON _ = Aeson.String "empty subject"

instance ToJSON Signature where
  toJSON _ = Aeson.String "empty signature"

instance FromJSON Signature where
  parseJSON = Aeson.withText "Signature" $ \_ -> pure $ Signature ()

instance ToJSON (PropertyValue name) => ToJSON (Property name) where
  toJSON (Property value anSignatures) = Aeson.Object $ HM.fromList $
    [ ("value", Aeson.toJSON value)
    , ("anSignatures", Aeson.toJSON anSignatures)
    ]

deriving instance Show (PropertyValue name) => Show (Property name)
deriving instance Eq (PropertyValue name) => Eq (Property name)

instance (All (KnownSymbol `And` (ToJSON `Compose` Property)) xs) => ToJSON (Metadata xs) where
  toJSON (Metadata subject props) = Aeson.Object $ HM.fromList $
    [ ("subject", Aeson.toJSON subject) ]
    <> propertiesToJSONValues props

instance FromJSON (PropertyValue name) => FromJSON (Property name) where
  parseJSON = Aeson.withObject "Property" $ \obj ->
    Property <$> obj .: "value"
             <*> obj .: "anSignatures"
