module Test.HtmlSanitizerSpec (tests) where

import IHP.Prelude
import Test.Hspec (describe, it, shouldBe, Spec)
import Test.QuickCheck (elements, property, forAll, Gen, Arbitrary(..), suchThat,resize)
import Text.HTML.TagSoup.Tree (parseTree, renderTree, TagTree(..))
import Application.Htmlsanitizer (sanitizeHtml, forbiddenTagNames, AttributeName , AttributeValue, urlAttributes)
import Data.Foldable (toList)
import Control.Monad (replicateM)
import Data.String   (IsString (fromString))



forbiddenTags :: Gen Text
forbiddenTags = elements $ toList forbiddenTagNames

genText :: Gen Text
genText = do
    l <- resize 10 $ arbitrary `suchThat` (> 0)
    let g = elements $ ['A'.. 'Z'] ++ ['a' .. 'z'] ++ ['0'..'9']
    fromString <$> replicateM l g


callBacks :: Gen Text
callBacks =  ("on"<>) <$> genText


forbiddenUrlsWithAttributes :: Gen (AttributeName , AttributeValue)
forbiddenUrlsWithAttributes = do
  n <- elements . toList $ urlAttributes
  v <- elements [ "javascript:alert('XSS')"
                , "&Tab;javascript:prompt(1)&Tab;"
                , "javascript:&#13; javascript:prompt(1)"
                , "data:text/javascript,alert(1)"
                , "javascript:confirm(1)"
                ]
  return (n,v)



tests :: Spec
tests = do
  describe "Parsing Tests" $ do   
    it "totally remove forbiddenTags" $ do
      property $ forAll forbiddenTags $ \t -> 
        let html = "<" <> t <> ">doSomethingEvil();</" <> t <> ">" 
            r = html |> sanitizeHtml |> parseTree |> renderTree 
        in r `shouldBe` mempty
    
    it "totally remove callBacks" $ do
      property $ forAll callBacks $ \t -> 
        let html = "<p " <> t <> "=doSomethingEvil()>SomeThing</p>" 
            r = html |> sanitizeHtml |> parseTree |> renderTree 
        in r `shouldBe` "<p>SomeThing</p>"
   
    it "totally remove bad url link atrributes" $ do
      property $ forAll forbiddenUrlsWithAttributes $ \(n,v) -> 
        let html = "<img style=\"display:block;\" " <> n <> "=" <> v <> ">" 
            r = html |> sanitizeHtml |> parseTree |> renderTree 
        in r `shouldBe` "<img style=\"display:block;\">"