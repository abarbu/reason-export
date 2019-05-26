{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Reason.Type where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap
import Data.Map
import Data.Proxy
import Data.Text hiding (all)
import Data.Time
import GHC.Generics
import Prelude
import Data.Hashable(hash)

data ReasonDatatype
  = ReasonDatatype Text ReasonConstructor
  | ReasonPrimitive ReasonPrimitive
  deriving (Show, Eq)

data ReasonPrimitive
  = RInt
  | RInt64
  | RFloat
  | RBool
  | RChar
  | RString
  | RTimePosix
  | RUnit
  | RList ReasonDatatype
  | ROption ReasonDatatype
  | RTuple2 ReasonDatatype ReasonDatatype
  | RTuple3 ReasonDatatype ReasonDatatype ReasonDatatype
  | RMap ReasonPrimitive ReasonDatatype
  deriving (Show, Eq)

data ReasonConstructor
  = NamedConstructor Text ReasonValue
  | RecordConstructor Text ReasonValue
  | MultipleConstructors [ReasonConstructor]
  deriving (Show, Eq)

data ReasonValue
  = ReasonRef Text
  | ReasonEmpty
  | ReasonPrimitiveRef ReasonPrimitive
  | Values ReasonValue ReasonValue
  | ReasonField Text ReasonValue
  deriving (Show, Eq)

------------------------------------------------------------
class ReasonType a where
  toReasonType :: a -> ReasonDatatype
  toReasonType = genericToReasonDatatype . from
  default toReasonType :: (Generic a, GenericReasonDatatype (Rep a)) =>
    a -> ReasonDatatype

------------------------------------------------------------
class GenericReasonDatatype f where
  genericToReasonDatatype :: f a -> ReasonDatatype

instance (Datatype d, GenericReasonConstructor f) =>
         GenericReasonDatatype (D1 d f) where
  genericToReasonDatatype datatype =
    ReasonDatatype
      (pack (datatypeName datatype))
      (genericToReasonConstructor (unM1 datatype))

------------------------------------------------------------
class GenericReasonConstructor f where
  genericToReasonConstructor :: f a -> ReasonConstructor

instance (Constructor c, GenericReasonValue f) =>
         GenericReasonConstructor (C1 c f) where
  genericToReasonConstructor constructor =
    if conIsRecord constructor
      then RecordConstructor name (genericToReasonValue (unM1 constructor))
      else NamedConstructor name (genericToReasonValue (unM1 constructor))
    where
      name = pack $ conName constructor

instance (GenericReasonConstructor f, GenericReasonConstructor g) =>
         GenericReasonConstructor (f :+: g) where
  genericToReasonConstructor _ =
    MultipleConstructors
      [ genericToReasonConstructor (undefined :: f p)
      , genericToReasonConstructor (undefined :: g p)
      ]

------------------------------------------------------------
class GenericReasonValue f where
  genericToReasonValue :: f a -> ReasonValue

instance (Selector s, GenericReasonValue a) =>
         GenericReasonValue (S1 s a) where
  genericToReasonValue selector =
    case selName selector of
      "" -> genericToReasonValue (undefined :: a p)
      name -> ReasonField (pack name) (genericToReasonValue (undefined :: a p))

instance (GenericReasonValue f, GenericReasonValue g) =>
         GenericReasonValue (f :*: g) where
  genericToReasonValue _ =
    Values
      (genericToReasonValue (undefined :: f p))
      (genericToReasonValue (undefined :: g p))

instance GenericReasonValue U1 where
  genericToReasonValue _ = ReasonEmpty

instance ReasonType a =>
         GenericReasonValue (Rec0 a) where
  genericToReasonValue _ =
    case toReasonType (Proxy :: Proxy a) of
      ReasonPrimitive primitive -> ReasonPrimitiveRef primitive
      ReasonDatatype name _ -> ReasonRef name

instance ReasonType a =>
         ReasonType [a] where
  toReasonType _ = ReasonPrimitive (RList (toReasonType (Proxy :: Proxy a)))

instance ReasonType a =>
         ReasonType (Maybe a) where
  toReasonType _ = ReasonPrimitive (ROption (toReasonType (Proxy :: Proxy a)))

instance ReasonType () where
  toReasonType _ = ReasonPrimitive RUnit

instance ReasonType Text where
  toReasonType _ = ReasonPrimitive RString

instance ReasonType Day where
  toReasonType _ = ReasonPrimitive RTimePosix

instance ReasonType UTCTime where
  toReasonType _ = ReasonPrimitive RTimePosix

instance ReasonType Float where
  toReasonType _ = ReasonPrimitive RFloat

instance ReasonType Double where
  toReasonType _ = ReasonPrimitive RFloat

instance ReasonType Int8 where
  toReasonType _ = ReasonPrimitive RInt

instance ReasonType Int16 where
  toReasonType _ = ReasonPrimitive RInt

instance ReasonType Int32 where
  toReasonType _ = ReasonPrimitive RInt

instance ReasonType Int64 where
  toReasonType _ = ReasonPrimitive RInt64

instance (ReasonType a, ReasonType b) =>
         ReasonType (a, b) where
  toReasonType _ =
    ReasonPrimitive $
    RTuple2 (toReasonType (Proxy :: Proxy a)) (toReasonType (Proxy :: Proxy b))

instance (ReasonType a, ReasonType b, ReasonType c) =>
         ReasonType (a, b, c) where
  toReasonType _ =
    ReasonPrimitive $
    RTuple3 (toReasonType (Proxy :: Proxy a)) (toReasonType (Proxy :: Proxy b)) (toReasonType (Proxy :: Proxy c))

instance (ReasonType a) =>
         ReasonType (Proxy a) where
  toReasonType _ = toReasonType (undefined :: a)

instance (HasReasonComparable k, ReasonType v) =>
         ReasonType (Map k v) where
  toReasonType _ =
    ReasonPrimitive $
    RMap (toReasonComparable (undefined :: k)) (toReasonType (Proxy :: Proxy v))

instance (ReasonType v) =>
         ReasonType (IntMap v) where
  toReasonType _ = ReasonPrimitive $ RMap RInt (toReasonType (Proxy :: Proxy v))

class HasReasonComparable a where
  toReasonComparable :: a -> ReasonPrimitive

instance HasReasonComparable String where
  toReasonComparable _ = RString

instance HasReasonComparable Text where
  toReasonComparable _ = RString

instance ReasonType Int where
  toReasonType _ = ReasonPrimitive RInt

instance ReasonType Char where
  toReasonType _ = ReasonPrimitive RChar

instance ReasonType Bool where
  toReasonType _ = ReasonPrimitive RBool

-- | Whether a set of constructors is an enumeration, i.e. whether they lack
-- values. data A = A | B | C would be simple data A = A Int | B | C would not
-- be simple.
isEnumeration :: ReasonConstructor -> Bool
isEnumeration (NamedConstructor _ ReasonEmpty) = True
isEnumeration (MultipleConstructors cs) = all isEnumeration cs
isEnumeration _ = False

primitiveName :: ReasonPrimitive -> Text
primitiveName k = pack (show (abs (hash (show k))))
