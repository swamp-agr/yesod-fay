{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module Yesod.Fay.Data where

import Yesod.Core

-- | The Fay subsite.
data FaySite = FaySite

mkYesodSubData "FaySite" [parseRoutes|
/ FayCommandR POST
|]
