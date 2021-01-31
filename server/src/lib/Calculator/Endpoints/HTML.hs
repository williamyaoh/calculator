{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- |
-- Adaptor instances to serve HTML using Servant.

module Calculator.Endpoints.HTML
  ( HTML )
where

import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import           Text.Blaze.Html5              as Blaze ( Html )

import Network.HTTP.Media       ( (//), (/:) )
import Servant.API.ContentTypes

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Blaze.Html where
  mimeRender _ = Blaze.renderHtml
