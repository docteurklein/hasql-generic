--------------------------------------------------------------------------------
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Hasql.Generic.HasParams
    ( HasParams
    , HasEField
    , HasEValue
    , mkParams
    , mkEField
    , mkEValue
    , gParams
    , gEValue
    ) where

--------------------------------------------------------------------------------
import qualified Data.Aeson.Types           as JSON
import           Data.ByteString            (ByteString)
import           Data.Foldable              (foldl')
import           Data.Functor.Contravariant
import           Data.Int                   (Int16, Int32, Int64)
import qualified Data.Map                   as Map
import           Data.Scientific            (Scientific)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Time                  (Day, DiffTime, LocalTime,
                                             TimeOfDay, TimeZone, UTCTime)
import           Data.UUID                  (UUID)
import           Data.Vector                (Vector)
import qualified Data.Vector                as Vector
import           Data.Word                  (Word16, Word32, Word64)
import           Generics.SOP
import qualified GHC.Generics               as GHC
import           Hasql.Encoders

--------------------------------------------------------------------------------
-- |
-- A type that can be encoded into database parameters, using Hasql's `Params` encoder.
--
-- Your data type needs to derive GHC `GHC.Generics.Generic`, and using this derive
-- an instance of SOP `Generics.SOP.Generic`. From here you can derive an instance
-- of `HasParams`. This gives you access to a value `mkParams`, which you can use
-- to get a value of type `Hasql.Encoders.Params`.
--
-- @
-- {-\# DeriveGeneric #-}
--
-- import Data.Text (Text)
-- import Data.UUID (UUID)
-- import qualified GHC.Generics as GHC
-- import           Generics.SOP
-- import           Hasql.Query (statement)
-- import           Hasql.Session (Session, query)
-- import qualified Hasql.Decoders as HD
-- import qualified Hasql.Encoders as HE
--
-- data Person = Person
--   { personId :: UUID
--   , personName :: Text
--   , personAge :: Int
--   } deriving (GHC.Generic)
--
-- instance Generic Person
-- instance HasParams Person
--
-- \-- Search for a 'Person' with a matching UUID
-- createPerson :: Person -> Session ()
-- createPerson person =
--   query person preparedStatement
--     where
--       preparedStatement = statement sql encoder decoder True
--       sql = "INSERT INTO people (id, name, age) VALUES ($1, $2, $3)"
--       encoder = mkParams
--       decoder = HD.unit
-- @
class HasParams a where
  mkParams :: Params a
  default mkParams :: (Generic a, Code a ~ '[ xs ], All HasEField xs) => Params a
  mkParams = gParams

--------------------------------------------------------------------------------
-- | A type representing a value encoder lifted into a composable params encoder.
--   Fields with `HasEValue` instances will be automatically lifted into some
--   common wrappers, including vectors, lists, and maybe.
class HasEField a where
  mkEField :: Params a

--------------------------------------------------------------------------------
-- | A type representing a encoder of an individual value. Instances should be
--   defined manually for each type.
class HasEValue a where
  mkEValue :: Value a

--------------------------------------------------------------------------------
-- | Generate a 'Params a' generically
gParams :: (Generic a, Code a ~ '[ xs ], All HasEField xs) => Params a
gParams =
  contramap (unZ . unSOP . from)
    (mconcat $ hcollapse
      (hcmap (Proxy :: Proxy HasEField)
         (\ (Fn p) -> K (contramap (unI . p . K) mkEField))
         projections
      )
    )

--------------------------------------------------------------------------------
class (a ~ b) => Equal a b
instance (a ~ b) => Equal a b

--------------------------------------------------------------------------------
-- | Derive a 'HasEField' for enumeration types
gEValue :: (Generic a, All (Equal '[]) (Code a)) => NP (K Text) (Code a) -> Params a
gEValue names =
  value (contramap (hcollapse . hzipWith const names . unSOP . from) text)

--------------------------------------------------------------------------------
-- Instances for common data types

instance HasEValue Bool where
  {-# INLINE mkEValue #-}
  mkEValue = bool

instance HasEValue Int16 where
  {-# INLINE mkEValue #-}
  mkEValue = int2

instance HasEValue Int32 where
  {-# INLINE mkEValue #-}
  mkEValue = int4

instance HasEValue Int64 where
  {-# INLINE mkEValue #-}
  mkEValue = int8

instance HasEValue Float where
  {-# INLINE mkEValue #-}
  mkEValue = float4

instance HasEValue Double where
  {-# INLINE mkEValue #-}
  mkEValue = float8

instance HasEValue Scientific where
  {-# INLINE mkEValue #-}
  mkEValue = numeric

instance HasEValue Char where
  {-# INLINE mkEValue #-}
  mkEValue = char

instance HasEValue Text where
  {-# INLINE mkEValue #-}
  mkEValue = text

instance HasEValue ByteString where
  {-# INLINE mkEValue #-}
  mkEValue = bytea

instance HasEValue Day where
  {-# INLINE mkEValue #-}
  mkEValue = date

instance HasEValue LocalTime where
  {-# INLINE mkEValue #-}
  mkEValue = timestamp

instance HasEValue UTCTime where
  {-# INLINE mkEValue #-}
  mkEValue = timestamptz

instance HasEValue TimeOfDay where
  {-# INLINE mkEValue #-}
  mkEValue = time

instance HasEValue (TimeOfDay, TimeZone) where
  {-# INLINE mkEValue #-}
  mkEValue = timetz

instance HasEValue DiffTime where
  {-# INLINE mkEValue #-}
  mkEValue = interval

instance HasEValue UUID where
  {-# INLINE mkEValue #-}
  mkEValue = uuid

instance HasEValue JSON.Value where
  {-# INLINE mkEValue #-}
  mkEValue = jsonb


--------------------------------------------------------------------------------
instance HasEValue a => HasEField [Maybe a] where
  {-# INLINE mkEField #-}
  mkEField = value $ array (arrayDimension foldl' (arrayNullableValue mkEValue))

instance HasEValue a => HasEField [a] where
  {-# INLINE mkEField #-}
  mkEField = value $ array (arrayDimension foldl' (arrayValue mkEValue))

instance HasEValue a => HasEField (Vector (Maybe a)) where
  {-# INLINE mkEField #-}
  mkEField = value $ array (arrayDimension Vector.foldl' (arrayNullableValue mkEValue))

instance HasEValue a => HasEField (Vector a) where
  {-# INLINE mkEField #-}
  mkEField = value $ array (arrayDimension Vector.foldl' (arrayValue mkEValue))

instance HasEValue a => HasEField (Maybe a) where
  {-# INLINE mkEField #-}
  mkEField = nullableValue mkEValue

instance HasEValue a => HasEField a where
  {-# INLINE mkEField #-}
  mkEField = value mkEValue


--------------------------------------------------------------------------------
instance HasEField Int where
  {-# INLINE mkEField #-}
  mkEField = contramap fromIntegral (value int8)

instance HasEField (Maybe Int) where
  {-# INLINE mkEField #-}
  mkEField = contramap (fmap fromIntegral) (nullableValue int8)

instance HasEField Word16 where
  {-# INLINE mkEField #-}
  mkEField = contramap fromIntegral (value int2)

instance HasEField Word32 where
  {-# INLINE mkEField #-}
  mkEField = contramap fromIntegral (value int4)

instance HasEField Word64 where
  {-# INLINE mkEField #-}
  mkEField = contramap fromIntegral (value int8)

instance HasEField (Maybe Word16) where
  {-# INLINE mkEField #-}
  mkEField = contramap (fmap fromIntegral) (nullableValue int2)

instance HasEField (Maybe Word32) where
  {-# INLINE mkEField #-}
  mkEField = contramap (fmap fromIntegral) (nullableValue int4)

instance HasEField (Maybe Word64) where
  {-# INLINE mkEField #-}
  mkEField = contramap (fmap fromIntegral) (nullableValue int8)
