module Main where

import Test.DocTest

main :: IO ()
main = doctest [
        "-isrc"
    ,   "Prometheus.Metric.GHC"
    ]
