module PrestoDOM.Events where

import Prelude

import DOM (DOM)
import Control.Monad.Eff (Eff)
import FRP (FRP)
import Halogen.VDom.DOM.Prop (Prop)
import PrestoDOM.Properties (prop, PropName(..))
import Unsafe.Coerce (unsafeCoerce)

generateProp :: forall a eff. (a -> Eff eff Unit) -> String
generateProp = unsafeCoerce

type EventHandler e a = (a -> Eff (frp :: FRP, dom :: DOM | e) Unit) -> Prop Unit

onClick :: forall e a. EventHandler e a
onClick push = prop (PropName "onClick") (generateProp push)

onChange :: forall e a. EventHandler e a
onChange push = prop (PropName "onChange") (generateProp push)
