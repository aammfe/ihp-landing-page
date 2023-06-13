module Web.Controller.Static where

import Web.Controller.Prelude
import Web.View.Static.StyleGuide

instance Controller StaticController where
  action StyleGuideAction = render StyleGuideView