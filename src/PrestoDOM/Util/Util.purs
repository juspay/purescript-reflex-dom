module PrestoDOM.Util where

import Prelude
import Control.Monad.Eff (Eff)
import DOM.Node.Types (Element, Document)
import Halogen.VDom (Step(..), VDomMachine, VDomSpec(..))
import Halogen.VDom.Machine (never)
import Halogen.VDom.DOM.Prop (Prop)

foreign import logNode :: forall eff a . a  -> Eff eff Unit
foreign import applyAttributes ∷ forall i eff. Element → (Array (Prop i)) → Eff eff (Array (Prop i))
foreign import done :: forall eff. Eff eff Unit
foreign import patchAttributes ∷ forall i eff. Element → (Array (Prop i)) → (Array (Prop i)) → Eff eff (Array (Prop i))
foreign import cleanupAttributes ∷ forall i eff. Element → (Array (Prop i)) → Eff eff Unit
foreign import getLatestMachine :: forall m a b eff. Eff eff (Step m a b)
foreign import storeMachine :: forall eff m a b. Step m a b -> Eff eff Unit
foreign import getRootNode :: forall eff. Eff eff Document
foreign import insertDom :: forall a b eff. a -> b -> Eff eff Unit
-- foreign import attachSignalEvents :: forall a b eff.  String -> String -> (b ->  Eff (frp::F.FRP | eff) Unit) -> Unit
-- foreign import initializeState :: forall eff t . Eff eff Unit
-- foreign import updateState :: forall eff a b t. a  -> b -> Eff eff (Rec t)
-- foreign import getState :: forall eff t. Eff eff (Rec t)

buildAttributes
  ∷ ∀ eff a
  . Element
  → VDomMachine eff (Array (Prop a)) Unit
buildAttributes elem = apply
  where
  apply ∷ forall e. VDomMachine e (Array (Prop a)) Unit
  apply attrs = do
    x <- applyAttributes elem attrs
    pure
      (Step unit
        (patch x)
        (done x))

  patch ∷ forall e. (Array (Prop a)) → VDomMachine e (Array (Prop a)) Unit
  patch attrs1 attrs2 = do
    x <- patchAttributes elem attrs1 attrs2
    pure
      (Step unit
        (patch x)
        (done x))

  done ∷ forall e. (Array (Prop a)) → Eff e Unit
  done attrs = cleanupAttributes elem attrs

spec :: forall i e. Document -> VDomSpec e (Array (Prop i)) Void
spec document =  VDomSpec {
      buildWidget: const never
    , buildAttributes: buildAttributes
    , document : document
    }
