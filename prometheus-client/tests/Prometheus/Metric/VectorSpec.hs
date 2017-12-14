{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}

module Prometheus.Metric.VectorSpec (
    spec
) where

import Prometheus

import GHC.Generics
import Control.Monad
import Test.Hspec

data TestLabels = TestLabels { a :: String, b :: String }
  deriving (Generic, Ord, Eq, Show)
instance Label TestLabels

newtype ALabel = ALabel String
  deriving (Eq, Ord, Show)

instance Label ALabel where
  labelMap = [ ("a", \(ALabel a) -> labelValue a) ]

spec :: Spec
spec = describe "Prometheus.Metric.Vector" $ do
      it "starts with no labels" $ do
            m <- register $ vector @TestLabels $ counter (Info "name" "help")
            value <- getVectorWith m getCounter
            value `shouldBe` []
      it "maintains state for a single label" $ do
            m <- register $ vector $ counter (Info "name" "help")
            replicateM_ 47 $ withLabel m (TestLabels "foo" "bar") incCounter
            value <- getVectorWith m getCounter
            value `shouldBe` [((TestLabels "foo" "bar"), 47)]
      it "maintains state for multiple labels" $ do
            m <- register $ vector $ counter (Info "name" "help")
            replicateM_ 47 $ withLabel m (ALabel "foo") incCounter
            replicateM_ 42 $ withLabel m (ALabel "bar") incCounter
            value <- getVectorWith m getCounter
            value `shouldMatchList` [(ALabel "bar", 42), (ALabel "foo", 47)]
