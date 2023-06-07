module Main where

import Test.Hspec
import IHP.Prelude

import Test.HtmlSanitizerSpec

main :: IO ()
main = hspec do
    Test.HtmlSanitizerSpec.tests