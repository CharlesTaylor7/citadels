module Lucid.Htmx where
module Lucid.Htmx where

import Lucid.Base
import Data.Text (Text)

-- | <https://htmx.org/attributes/hx-boost/>
hxBoost_ :: Text -> Attributes
hxBoost_ = makeAttributes "hx-boost"

-- | <https://htmx.org/attributes/hx-confirm/>
hxConfirm_ :: Text -> Attributes
hxConfirm_ = makeAttributes "hx-confirm"

-- | <https://htmx.org/attributes/hx-disable/>
hxDisable_ :: Attributes
hxDisable_ = makeAttributes "hx-disable" mempty

-- | <https://htmx.org/attributes/hx-ext/>
hxExt_ :: Text -> Attributes
hxExt_ = makeAttributes "hx-ext"

-- | <https://htmx.org/attributes/hx-get/>
hxGet_ :: Text -> Attributes
hxGet_ = makeAttributes "hx-get"

-- | <https://htmx.org/attributes/hx-params/>
hxParams_ :: Text -> Attributes
hxParams_ = makeAttributes "hx-params"

-- | <https://htmx.org/attributes/hx-post/>
hxPost_ :: Text -> Attributes
hxPost_ = makeAttributes "hx-post"

-- | <https://htmx.org/attributes/hx-preserve/>
hxPreserve_ :: Text -> Attributes
hxPreserve_ = makeAttributes "hx-preserve"

-- | <https://htmx.org/attributes/hx-prompt/>
hxPrompt_ :: Text -> Attributes
hxPrompt_ = makeAttributes "hx-prompt"

-- | <https://htmx.org/attributes/hx-push-url/>
hxPushUrl_ :: Text -> Attributes
hxPushUrl_ = makeAttributes "hx-push-url"

-- | <https://htmx.org/attributes/hx-put/>
hxPut_ :: Text -> Attributes
hxPut_ = makeAttributes "hx-put"

-- | <https://htmx.org/attributes/hx-request/>
hxRequest_ :: Text -> Attributes
hxRequest_ = makeAttributes "hx-request"

-- | <https://htmx.org/attributes/hx-select/>
hxSelect_ :: Text -> Attributes
hxSelect_ = makeAttributes "hx-select"

-- | <https://htmx.org/attributes/hx-swap-oob/>
hxSwapOob_ :: Text -> Attributes
hxSwapOob_ = makeAttributes "hx-swap-oob"

-- | <https://htmx.org/attributes/hx-swap/>
hxSwap_ :: Text -> Attributes
hxSwap_ = makeAttributes "hx-swap"

-- | <https://htmx.org/attributes/hx-target/>
hxTarget_ :: Text -> Attributes
hxTarget_ = makeAttributes "hx-target"

-- | <https://htmx.org/attributes/hx-trigger/>
hxTrigger_ :: Text -> Attributes
hxTrigger_ = makeAttributes "hx-trigger"

-- | <https://htmx.org/attributes/hx-vals/>
hxVals_ :: Text -> Attributes
hxVals_ = makeAttributes "hx-vals"

-- | <https://htmx.org/attributes/hx-ws/>
hxWs_ :: Text -> Attributes
hxWs_ = makeAttributes "hx-ws"
