module Web.View.Static.StyleGuide where

import Web.View.Prelude

data StyleGuideView = StyleGuideView

instance View StyleGuideView where
  html StyleGuideView =
    [hsx|
      <header>
        <h1>Contribution</h1> 
      </header>
      <p>I would love for you to contribute to the CO<sub>2</sub> database. Either by contributing of emitters or by improving the database. You can find the project on Github.</p> 
    |]
