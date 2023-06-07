module Main where

import Test.Hspec
import IHP.Prelude

import Test.HtmlSanitizerSpec
import Test.Controller.ParagraphCtaSpec

main :: IO ()
main = hspec do
    Test.HtmlSanitizerSpec.tests
    Test.Controller.ParagraphCtaSpec.tests