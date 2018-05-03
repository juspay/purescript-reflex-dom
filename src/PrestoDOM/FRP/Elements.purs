module PrestoDOM.FRP.Elements where

import Prelude
import PrestoDOM.FRP

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.State.Class (get, put)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import Data.Array (snoc)
import Data.Lens ((^.))
import Data.List (List(..), toUnfoldable, reverse)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import FRP (FRP)
import FRP.Event (Event, create, subscribe)
import Halogen.VDom.DOM (buildVDom)
import Halogen.VDom.DOM.Prop (Prop)
import Halogen.VDom.Machine (extract, step)
import Halogen.VDom.Types (VDom(..), ElemSpec(..), ElemName(..))
import PrestoDOM.Events (EventHandler, onClick, onChange)
import PrestoDOM.Properties (width, height)
import PrestoDOM.Types.DomAttributes (Length(..))
import PrestoDOM.Util (getRootNode, insertDom, storeMachine, getLatestMachine, spec)
import Unsafe.Coerce (unsafeCoerce)

type FWidgetEff e = (dom :: DOM, frp :: FRP | e)
type FWidgetProp = Prop Unit
type FWidgetPropArr = Array FWidgetProp

type WidgetState = List (Dynamic (VDom FWidgetPropArr Void))
type FWidget m a = StateT WidgetState m a

runFWidget :: forall e m a. MonadEff (FWidgetEff e) m => FWidget m a -> m a
runFWidget w = do
  root <- liftEff getRootNode
  Tuple res vdomDynL <- runStateT w Nil
  let vdomLDyn = sequence vdomDynL
      mkElem cl = Elem (ElemSpec Nothing (ElemName "root") [width Match_Parent, height Match_Parent]) (toUnfoldable $ reverse cl)
  machine <- liftEff $ buildVDom (spec root) (mkElem (vdomLDyn ^. init))
  liftEff $ storeMachine machine
  _ <- liftEff $ subscribe (vdomLDyn ^. ev) (patchAndRun <<< mkElem)
  liftEff $ insertDom root (extract machine)
  pure res

patchAndRun :: forall eff i w. VDom i w -> Eff eff Unit
patchAndRun vdom = do
  machine <- getLatestMachine
  newMachine <- step machine vdom
  storeMachine newMachine

dynText :: forall e m. MonadEff (FWidgetEff e) m => Dynamic String -> FWidget m Unit
dynText textD = do
  oldL <- get
  let domDyn = Text <$> textD
  put $ Cons domDyn oldL

el :: forall m a e. MonadEff (FWidgetEff e) m => String -> Array (Prop Unit) -> FWidget m a -> FWidget m a
el tagName props child = do
  let elemSpec = ElemSpec Nothing (ElemName tagName) props
      mkElem = Elem elemSpec <<< toUnfoldable <<< reverse
  Tuple res cl <- lift $ runStateT child Nil
  let domDyn = mkElem <$> (sequence cl)
  oldL <- get
  put $ Cons domDyn oldL
  pure res

el' :: forall m a e. MonadEff (FWidgetEff e) m => String -> Dynamic (FWidgetPropArr) -> FWidget m a -> FWidget m a
el' tagName propsDyn child = do
  let elemSpec props = ElemSpec Nothing (ElemName tagName) props
      mkElem props cl = Elem (elemSpec props) $ toUnfoldable $ reverse cl
  Tuple res cl <- lift $ runStateT child Nil
  domDyn <- liftEff $ combineDyn mkElem propsDyn (sequence cl)
  oldL <- get
  put $ Cons domDyn oldL
  pure res

widgetHoldInternal :: forall a b m e. MonadEff (FWidgetEff e) m => String -> FWidgetPropArr -> FWidget m a -> Event (FWidget m b) -> FWidget m (Tuple a (Event b))
widgetHoldInternal tagName props w e = do
  let elemSpec = ElemSpec Nothing (ElemName tagName) props
      mkElem = Elem elemSpec <<< toUnfoldable <<< reverse
  Tuple res0 cl0 <- lift $ runStateT w Nil
  { event: resEvt, push: resPush } <- liftEff create
  { event: clEvt, push: clPush } <- liftEff create

  let f ch = do
        Tuple res1 cl1 <- runStateT ch Nil
        resPush res1
        clPush $ sequence cl1
  
  _ <- liftEff $ subscribe e $ unsafeCoerce f

  let domDyn = mkElem <$> join (stepDyn (sequence cl0) clEvt)
  oldL <- get
  put $ Cons domDyn oldL
  pure $ Tuple res0 resEvt

widgetHold :: forall m a e. MonadEff (FWidgetEff e) m => String -> FWidgetPropArr -> FWidget m a -> Event (FWidget m a) -> FWidget m (Dynamic a)
widgetHold tagName props w e = do
  Tuple a ea <- widgetHoldInternal tagName props w e
  pure $ stepDyn a ea

dyn :: forall m a e. MonadEff (FWidgetEff e) m => String -> FWidgetPropArr -> Dynamic (FWidget m a) -> FWidget m (Event a)
dyn tagName props d = do
  let initW = d ^. init
      evtW = d ^. ev
  snd <$> widgetHoldInternal tagName props initW evtW

-- | Wrap the child element with event processing. NOTE: if the child is a sequence of elements, it will put event listener
--   on all of them.
withEvent :: forall e m a evt. MonadEff (FWidgetEff e) m => EventHandler e evt -> FWidget m a -> FWidget m (Tuple a (Event evt))
withEvent handler child = do
  { event, push } <- liftEff create
  Tuple res cl <- lift $ runStateT child Nil
  let addProp p (Elem (ElemSpec domain name props) childArr) = Elem (ElemSpec domain name (snoc props p)) childArr
      addProp _ v = v
      newCL = map (addProp (handler push)) <$> cl
  oldL <- get
  put $ newCL <> oldL
  pure $ Tuple res event

withClickEvt :: forall e m a. MonadEff (FWidgetEff e) m => FWidget m a -> FWidget m (Tuple a (Event Unit))
withClickEvt = withEvent onClick

withChangeEvt :: forall e m a evt. MonadEff (FWidgetEff e) m => FWidget m a -> FWidget m (Tuple a (Event evt))
withChangeEvt = withEvent onChange

blank :: forall m e. MonadEff (FWidgetEff e) m => FWidget m Unit
blank = pure unit


linearLayout :: forall m a e. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m a -> FWidget m a
linearLayout = el "linearLayout"

linearLayout' :: forall m e a. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m a -> FWidget m a
linearLayout' = el' "linearLayout"

relativeLayout :: forall m e a. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m a -> FWidget m a
relativeLayout = el "relativeLayout"

relativeLayout' :: forall m e a. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m a -> FWidget m a
relativeLayout' = el' "relativeLayout"

horizontalScrollView :: forall m e a. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m a -> FWidget m a
horizontalScrollView = el "horizontalScrollView"

horizontalScrollView' :: forall m e a. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m a -> FWidget m a
horizontalScrollView' = el' "horizontalScrollView"

scrollView :: forall m e a. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m a -> FWidget m a
scrollView = el "scrollView"

scrollView' :: forall m e a. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m a -> FWidget m a
scrollView' = el' "scrollView"

frameLayout :: forall m e a. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m a -> FWidget m a
frameLayout = el "frameLayout"

frameLayout' :: forall m e a. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m a -> FWidget m a
frameLayout' = el' "frameLayout"

shimmerFrameLayout :: forall m e a. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m a -> FWidget m a
shimmerFrameLayout = el "shimmerFrameLayout"

shimmerFrameLayout' :: forall m e a. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m a -> FWidget m a
shimmerFrameLayout' = el' "shimmerFrameLayout"

tabLayout :: forall m e a. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m a -> FWidget m a
tabLayout = el "tabLayout"

tabLayout' :: forall m e a. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m a -> FWidget m a
tabLayout' = el' "tabLayout"

imageView :: forall m e. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m Unit
imageView props = el "imageView" props blank

imageView' :: forall m e. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m Unit
imageView' propsDyn = el' "imageView" propsDyn blank

editText :: forall m e. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m (Dynamic String)
editText props = do
  Tuple _ event <- withChangeEvt $ el "editText" props blank
  pure $ stepDyn "" event

editText' :: forall m e. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m (Dynamic String)
editText' propsDyn = do
  Tuple _ event <- withChangeEvt $ el' "editText" propsDyn blank
  pure $ stepDyn "" event

listView :: forall m e. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m Unit
listView props = el "listView" props blank

listView' :: forall m e. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m Unit
listView' propsDyn = el' "listView" propsDyn blank

progressBar :: forall m e. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m Unit
progressBar props = el "progressBar" props blank

progressBar' :: forall m e. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m Unit
progressBar' propsDyn = el' "progressBar" propsDyn blank

textView :: forall m e. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m Unit
textView props = el "textView" props blank

textView' :: forall m e. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m Unit
textView' propsDyn = el' "textView" propsDyn blank

viewPager :: forall m e. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m Unit
viewPager props = el "viewPager" props blank

viewPager' :: forall m e. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m Unit
viewPager' propsDyn = el' "viewPager" propsDyn blank

button :: forall m e. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m Unit
button props = el "button" props blank

button' :: forall m e. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m Unit
button' propsDyn = el' "button" propsDyn blank

calendar :: forall m e. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m Unit
calendar props = el "calendar" props blank

calendar' :: forall m e. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m Unit
calendar' propsDyn = el' "calendar" propsDyn blank

checkBox :: forall m e. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m Unit
checkBox props = el "checkBox" props blank

checkBox' :: forall m e. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m Unit
checkBox' propsDyn = el' "checkBox" propsDyn blank

switch :: forall m e. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m Unit
switch props = el "switch" props blank

switch' :: forall m e. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m Unit
switch' propsDyn = el' "switch" propsDyn blank

viewWidget :: forall m e. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m Unit
viewWidget props = el "viewWidget" props blank

viewWidget' :: forall m e. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m Unit
viewWidget' propsDyn = el' "viewWidget" propsDyn blank

webView :: forall m e. MonadEff (FWidgetEff e) m => FWidgetPropArr -> FWidget m Unit
webView props = el "webView" props blank

webView' :: forall m e. MonadEff (FWidgetEff e) m => Dynamic FWidgetPropArr -> FWidget m Unit
webView' propsDyn = el' "webView" propsDyn blank
