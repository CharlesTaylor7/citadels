module Lucid.Extra where

import Relude (Text, ByteString)

import Lucid qualified 
import Lucid.Html5

scriptSrc_ :: Text -> Lucid.Html ()
scriptSrc_ src = script_ [ src_ src ] ("" :: ByteString)

text_ :: Text -> Lucid.Html ()
text_ = Lucid.toHtml


