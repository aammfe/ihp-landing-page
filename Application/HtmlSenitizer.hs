module Application.HtmlSenitizer (senitizeHtml, forbiddenTagNames,urlAttributes, AttributeName, AttributeValue) where


import IHP.ViewPrelude
import Text.HTML.TagSoup.Tree (parseTree, renderTree, TagTree(..))
import Text.HTML.TagSoup (Tag(..))  
import Data.Set (Set(), member, fromList)
import Data.Text (unpack, isPrefixOf, toLower, pack)
import Network.URI (uriScheme, parseURIReference, URI (..), isAllowedInURI, escapeURIString)
import Codec.Binary.UTF8.String (encodeString)


senitizeHtml :: Text -> Text
senitizeHtml = renderTree . senitizeTags . parseTree



senitizeTags :: [TagTree Text] -> [TagTree Text]
senitizeTags = map senitizeTag . filter isSafeTag


senitizeTag :: TagTree Text -> TagTree Text
senitizeTag (TagBranch n atrs tgs) = TagBranch n (filter isAttributeSafe atrs) (senitizeTags tgs)
senitizeTag (TagLeaf (TagOpen n att)) = TagLeaf (TagOpen n (filter isAttributeSafe att))
senitizeTag x = x


isSafeTag :: TagTree Text -> Bool
isSafeTag (TagBranch n _ _) = not . isForbiddenTagName $ n
isSafeTag _                 = True


isForbiddenTagName :: Text -> Bool
isForbiddenTagName x = member (toLower x) forbiddenTagNames


forbiddenTagNames :: Set Text
forbiddenTagNames = fromList ["script", "iframe", "embed", "object"]


type AttributeName = Text
type AttributeValue = Text

isAttributeSafe :: (AttributeName, AttributeValue) -> Bool
isAttributeSafe attr@(n,v)
  | isCallBackAttribute n = False
  | isJsAttribute attr    = False
  | isUrlAttributes n     = isSafeURI v 
  | otherwise             = True


isCallBackAttribute :: AttributeName -> Bool
isCallBackAttribute x = "on" `isPrefixOf` toLower x


isJsAttribute :: (AttributeName, AttributeValue) -> Bool
isJsAttribute (n,v) = "javascript:" `isPrefixOf` toLower n || "javascript:" `isPrefixOf` toLower v


isUrlAttributes :: AttributeName -> Bool
isUrlAttributes n = member (toLower n) urlAttributes


urlAttributes :: Set AttributeName
urlAttributes = fromList ["url", "href", "action", "background", "dynsrc", "lowsrc", "src", "formaction"]


isSafeURI :: AttributeValue -> Bool
isSafeURI v =
  case parseURIReference . escapeURI . unpack $ v of
     Just p  -> null (uriScheme p) ||
                member (toLower . pack . fromMaybe "" . init . uriScheme $ p) safeURISchemes
     Nothing -> False


safeURISchemes :: Set Text
safeURISchemes = fromList [ "ftp", "http", "https"]


escapeURI :: String -> String
escapeURI = escapeURIString isAllowedInURI . encodeString

