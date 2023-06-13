module Main where

import Test.Hspec
import IHP.Prelude

import Test.Controller.ParagraphCtaSpec

main :: IO ()
main = hspec do
    Test.Controller.ParagraphCtaSpec.tests