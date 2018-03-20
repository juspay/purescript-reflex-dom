# purescript-reflex-dom
Reflex.DOM style UI building library on top of halogen-vdom and PrestoUI

Checkout src/Main.purs for usage demo.

The FRP is based on purescript-behaviors and there's Dynamic as well as helper functions in PrestDOM.FRP module.

All UI elements are built in the FWidget monad. There're common UI elements from PrestoUI defined in PrestoDOM.FRP.Elements module already.

In order to handle the case that a widget refers to Event produced by another widget that's added later, there's the fixEvent function, which has type:

fixEvent :: forall a b eff m. MonadEff (frp :: FRP | eff) m => (Event a -> m (Tuple (Event a) b)) -> m b
