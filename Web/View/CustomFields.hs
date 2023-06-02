module Web.View.CustomFields where

import IHP.ViewPrelude

textareaWysiwygField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
textareaWysiwygField field = (textField field) { fieldType = TextareaInput,  fieldClass = "wysiwyg" }
{-# INLINE textareaWysiwygField #-}
