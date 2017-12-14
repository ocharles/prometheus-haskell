{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prometheus.Label
  ( Label (..)
  , LabelValue(..)
  , LabelPairs
  , toLabelPairs
  ) where

import Control.Arrow (second)
import Data.Proxy
import Data.Text (Text, pack)
import GHC.Generics
import GHC.TypeLits

type LabelPairs = [(Text, Text)]
  
-- | Labels are used to construct multi-dimensional metrics. The 'Label' type
-- class defines the class of types that can be used to add dimensions to
-- metrics.
--
-- This class specifies a way to (statically) transform a label into the list
-- of dimensions, and a way to extract a value for each dimension.
--
-- To use labels, first define a data type that represents the labels you
-- wish to use and derive an instance of the 'Label' class. Here we're
-- using the @DeriveAnyClass@ and @DeriveGeneric@ extensions:
--
--     data HttpMeta = HttpMeta { httpMethod :: String , httpStatus :: Int }
--       deriving (Generic, Label, Ord, Eq)
--
-- Now you can construct a vector of metrics:
--
--    httpRequests <- register "http_requests" $ vector (counter (Info ...))
--
-- To access the underlying counter, we index into this vector by providing labels:
--
--    withLabel httpRequests (HttpMeta { httpMethod = "GET", httpStatus = 200 }) incCounter
class (Eq l, Ord l) => Label l where
  labelMap :: [(Text, l -> Text)]
  default labelMap :: (Generic l, GLabel (Rep l)) => [(Text, l -> Text)]
  labelMap = map (second (. from)) glabelMap

class GLabel f where
  glabelMap :: [(Text, f a -> Text)]

instance GLabel f => GLabel (M1 D meta f) where
  glabelMap = map (second (. unM1)) glabelMap

instance GLabel f => GLabel (M1 C meta f) where
  glabelMap = map (second (. unM1)) glabelMap

instance (GLabel l, GLabel r) => GLabel (l :*: r) where
  glabelMap = map (second (. getLeft)) glabelMap ++ map (second (. getRight)) glabelMap
    where getLeft (l :*: _) = l
          getRight (_ :*: r) = r

instance (KnownSymbol name, LabelValue a) => GLabel (M1 S ('MetaSel ('Just name) su ss ds) (K1 i a)) where
  glabelMap =
    [( pack (symbolVal (Proxy :: Proxy name))
     , \(M1 (K1 a)) -> labelValue a)
    ]

class LabelValue a where
  labelValue :: a -> Text
  default labelValue :: Show a => a -> Text
  labelValue = pack . show

instance LabelValue Bool
instance LabelValue Char
instance LabelValue Int
instance LabelValue Float
instance LabelValue Double

instance LabelValue String where labelValue = pack
instance LabelValue Text where labelValue = id

toLabelPairs :: Label l => l -> LabelPairs
toLabelPairs l = map (second (\extract -> extract l)) labelMap
