module PrestoDOM.FRP.Elements where

import Prelude

import FRP (FRP)
import FRP.Event (Event, create, subscribe)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.State.Class (get, put)
import Control.Monad.Trans.Class (lift)

import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.List (List(..), toUnfoldable, reverse)
import Data.Array (snoc)
import Data.Traversable (sequence)
import PrestoDOM.FRP
import PrestoDOM.Util (getRootNode, insertDom, storeMachine, getLatestMachine, spec)
import PrestoDOM.Properties (width, height)
import PrestoDOM.Types.DomAttributes (Length(..))
import PrestoDOM.Events (EventHandler, onClick, onChange)
import DOM (DOM)

import Halogen.VDom.DOM (buildVDom)
import Halogen.VDom.Machine (never, extract, step, Step(..))
import Halogen.VDom.Types (VDom(..), ElemSpec(..), ElemName(..))
import Halogen.VDom.DOM.Prop (Prop(..))

type WidgetState i = List (Dynamic (VDom (Array (Prop i)) Void))
type FWidget i m a = StateT (WidgetState i) m a

runFWidget :: forall i e a. FWidget i (Eff (dom :: DOM, frp :: FRP | e)) a -> Eff (dom :: DOM, frp :: FRP | e) a
runFWidget w = do
  root <- liftEff getRootNode
  Tuple res vdomDynL <- runStateT w Nil
  let vdomLDyn = sequence vdomDynL
      mkElem cl = Elem (ElemSpec Nothing (ElemName "root") [width Match_Parent, height Match_Parent]) (toUnfoldable $ reverse cl)
  machine <- buildVDom (spec root) (mkElem (vdomLDyn ^. init))
  storeMachine machine
  _ <- liftEff $ subscribe (vdomLDyn ^. ev) (patchAndRun <<< mkElem)
  liftEff $ insertDom root (extract machine)
  pure res

patchAndRun :: forall eff i w. VDom i w -> Eff eff Unit
patchAndRun vdom = do
  machine <- getLatestMachine
  newMachine <- step machine vdom
  storeMachine newMachine

type FrpDomEffect e = (dom :: DOM, frp :: FRP | e)

dynText :: forall i m e. MonadEff (FrpDomEffect e) m => Dynamic String -> FWidget i m Unit
dynText textD = do
  oldL <- get
  let domDyn = Text <$> textD
  put $ Cons domDyn oldL

el :: forall m i a e. MonadEff (FrpDomEffect e) m => String -> Array (Prop i) -> FWidget i m a -> FWidget i m a
el tagName props child = do
  let elemSpec = ElemSpec Nothing (ElemName tagName) props
      mkElem = Elem elemSpec <<< toUnfoldable <<< reverse
  Tuple res cl <- lift $ runStateT child Nil
  let domDyn = mkElem <$> (sequence cl)
  oldL <- get
  put $ Cons domDyn oldL
  pure res

el' :: forall m i a e. MonadEff (FrpDomEffect e) m => String -> Dynamic (Array (Prop i)) -> FWidget i m a -> FWidget i m a
el' tagName propsDyn child = do
  let elemSpec props = ElemSpec Nothing (ElemName tagName) props
      mkElem props cl = Elem (elemSpec props) $ toUnfoldable $ reverse cl
  Tuple res cl <- lift $ runStateT child Nil
  domDyn <- liftEff $ combineDyn mkElem propsDyn (sequence cl)
  oldL <- get
  put $ Cons domDyn oldL
  pure res

-- | Wrap the child element with event processing. NOTE: if the child is a sequence of elements, it will put event listener
--   on all of them.
withEvent :: forall i e m a evt. MonadEff (FrpDomEffect e) m => EventHandler e i evt -> FWidget i m a -> FWidget i m (Tuple a (Event evt))
withEvent handler child = do
  { event, push } <- liftEff create
  Tuple res cl <- lift $ runStateT child Nil
  let addProp p (Elem (ElemSpec domain name props) childArr) = Elem (ElemSpec domain name (snoc props p)) childArr
      addProp _ v = v
      newCL = map (addProp (handler push)) <$> cl
  oldL <- get
  put $ newCL <> oldL
  pure $ Tuple res event

withClickEvt :: forall i e m a. MonadEff (FrpDomEffect e) m => FWidget i m a -> FWidget i m (Tuple a (Event Unit))
withClickEvt = withEvent onClick

withChangeEvt :: forall i e m a evt. MonadEff (FrpDomEffect e) m => FWidget i m a -> FWidget i m (Tuple a (Event evt))
withChangeEvt = withEvent onChange

blank :: forall i m. Monad m => FWidget i m Unit
blank = pure unit


linearLayout :: forall i m a e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m a -> FWidget i m a
linearLayout = el "linearLayout"

linearLayout' :: forall i m a e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m a -> FWidget i m a
linearLayout' = el' "linearLayout"

relativeLayout :: forall i m a e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m a -> FWidget i m a
relativeLayout = el "relativeLayout"

relativeLayout' :: forall i m a e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m a -> FWidget i m a
relativeLayout' = el' "relativeLayout"

horizontalScrollView :: forall i m a e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m a -> FWidget i m a
horizontalScrollView = el "horizontalScrollView"

horizontalScrollView' :: forall i m a e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m a -> FWidget i m a
horizontalScrollView' = el' "horizontalScrollView"

scrollView :: forall i m a e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m a -> FWidget i m a
scrollView = el "scrollView"

scrollView' :: forall i m a e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m a -> FWidget i m a
scrollView' = el' "scrollView"

frameLayout :: forall i m a e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m a -> FWidget i m a
frameLayout = el "frameLayout"

frameLayout' :: forall i m a e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m a -> FWidget i m a
frameLayout' = el' "frameLayout"

shimmerFrameLayout :: forall i m a e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m a -> FWidget i m a
shimmerFrameLayout = el "shimmerFrameLayout"

shimmerFrameLayout' :: forall i m a e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m a -> FWidget i m a
shimmerFrameLayout' = el' "shimmerFrameLayout"

tabLayout :: forall i m a e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m a -> FWidget i m a
tabLayout = el "tabLayout"

tabLayout' :: forall i m a e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m a -> FWidget i m a
tabLayout' = el' "tabLayout"

imageView :: forall i m e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m Unit
imageView props = el "imageView" props blank

imageView' :: forall i m e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m Unit
imageView' propsDyn = el' "imageView" propsDyn blank

editText :: forall i m e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m (Dynamic String)
editText props = do
  Tuple _ event <- withChangeEvt $ el "editText" props blank
  pure $ stepDyn "" event

editText' :: forall i m e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m (Dynamic String)
editText' propsDyn = do
  Tuple _ event <- withChangeEvt $ el' "editText" propsDyn blank
  pure $ stepDyn "" event

listView :: forall i m e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m Unit
listView props = el "listView" props blank

listView' :: forall i m e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m Unit
listView' propsDyn = el' "listView" propsDyn blank

progressBar :: forall i m e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m Unit
progressBar props = el "progressBar" props blank

progressBar' :: forall i m e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m Unit
progressBar' propsDyn = el' "progressBar" propsDyn blank

textView :: forall i m e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m Unit
textView props = el "textView" props blank

textView' :: forall i m e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m Unit
textView' propsDyn = el' "textView" propsDyn blank

viewPager :: forall i m e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m Unit
viewPager props = el "viewPager" props blank

viewPager' :: forall i m e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m Unit
viewPager' propsDyn = el' "viewPager" propsDyn blank

button :: forall i m e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m Unit
button props = el "button" props blank

button' :: forall i m e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m Unit
button' propsDyn = el' "button" propsDyn blank

calendar :: forall i m e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m Unit
calendar props = el "calendar" props blank

calendar' :: forall i m e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m Unit
calendar' propsDyn = el' "calendar" propsDyn blank

checkBox :: forall i m e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m Unit
checkBox props = el "checkBox" props blank

checkBox' :: forall i m e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m Unit
checkBox' propsDyn = el' "checkBox" propsDyn blank

switch :: forall i m e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m Unit
switch props = el "switch" props blank

switch' :: forall i m e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m Unit
switch' propsDyn = el' "switch" propsDyn blank

viewWidget :: forall i m e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m Unit
viewWidget props = el "viewWidget" props blank

viewWidget' :: forall i m e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m Unit
viewWidget' propsDyn = el' "viewWidget" propsDyn blank

webView :: forall i m e. MonadEff (FrpDomEffect e) m => Array (Prop i) -> FWidget i m Unit
webView props = el "webView" props blank

webView' :: forall i m e. MonadEff (FrpDomEffect e) m => Dynamic (Array (Prop i)) -> FWidget i m Unit
webView' propsDyn = el' "webView" propsDyn blank
