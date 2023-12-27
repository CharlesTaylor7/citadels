module Lucid.Htmx where

import Relude

import Lucid.Base


hxExt_ :: Text -> Attributes
hxExt_ = makeAttributes "hx-ext"

hxGet_ :: Text -> Attributes
hxGet_ = makeAttributes "hx-get"

hxPost_ :: Text -> Attributes
hxPost_ = makeAttributes "hx-post"


hxSwapOob_ :: Text -> Attributes
hxSwapOob_ = makeAttributes "hx-swap-oob"

hxSwap_ :: Text -> Attributes
hxSwap_ = makeAttributes "hx-swap"

hxTarget_ :: Text -> Attributes
hxTarget_ = makeAttributes "hx-target"

hxTrigger_ :: Text -> Attributes
hxTrigger_ = makeAttributes "hx-trigger"


wsConnect_ :: Text -> Attributes
wsConnect_ = makeAttributes "ws-connect"

wsSend_ :: Text -> Attributes
wsSend_ = makeAttributes "ws-send"
