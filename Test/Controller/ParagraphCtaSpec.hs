module Test.Controller.ParagraphCtaSpec where

import Network.HTTP.Types.Status

import IHP.Prelude
import IHP.QueryBuilder (query)
import IHP.Test.Mocking
import IHP.Fetch

import IHP.FrameworkConfig
import IHP.HaskellSupport
import Test.Hspec
import Config

import Generated.Types
import Web.Routes
import Web.Types
import Web.Controller.ParagraphCtas ()
import Web.FrontController ()
import Network.Wai
import IHP.ControllerPrelude
import Data.Text.Encoding (encodeUtf8)

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
        describe "ParagraphCtasController" $ do 

            it "creates a new paragraph Cta" $ withContext do

                page <- newRecord @LandingPage 
                            |> set #title "Lorem Ipsum"
                            |> createRecord

                response <- callActionWithParams CreateParagraphCtaAction [ ("title", "Hello")
                                                                          , ("body", "<p onclick=\"javascript:alert('XSS')\"> eval </p>")
                                                                          , ("landingPageId",  encodeUtf8 . show $ page.id)
                                                                          , ("weight", "0")
                                                                          , ("refLandingPageId", encodeUtf8 . show $ page.id)
                                                                          ]

                paragraph <- query @ParagraphCta |> fetchOne
                paragraph.body `shouldBe` "<p> eval </p>"