module PrestoDOM.FRP.Elements where

import Prelude
import PrestoDOM.FRP

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
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

type WidgetState i = List (Dynamic (VDom (Array (Prop i)) Void))
type FWidget i e a = StateT (WidgetState i) (Eff (dom :: DOM, frp :: FRP | e)) a

runFWidget :: forall i e a. FWidget i e a -> Eff (dom :: DOM, frp :: FRP | e) a
runFWidget w = do
  root <- getRootNode
  Tuple res vdomDynL <- runStateT w Nil
  let vdomLDyn = sequence vdomDynL
      mkElem cl = Elem (ElemSpec Nothing (ElemName "root") [width Match_Parent, height Match_Parent]) (toUnfoldable $ reverse cl)
  machine <- buildVDom (spec root) (mkElem (vdomLDyn ^. init))
  storeMachine machine
  _ <- subscribe (vdomLDyn ^. ev) (patchAndRun <<< mkElem)
  insertDom root (extract machine)
  pure res

patchAndRun :: forall eff i w. VDom i w -> Eff eff Unit
patchAndRun vdom = do
  machine <- getLatestMachine
  newMachine <- step machine vdom
  storeMachine newMachine

dynText :: forall i e. Dynamic String -> FWidget i e Unit
dynText textD = do
  oldL <- get
  let domDyn = Text <$> textD
  put $ Cons domDyn oldL

el :: forall i a e. String -> Array (Prop i) -> FWidget i e a -> FWidget i e a
el tagName props child = do
  let elemSpec = ElemSpec Nothing (ElemName tagName) props
      mkElem = Elem elemSpec <<< toUnfoldable <<< reverse
  Tuple res cl <- lift $ runStateT child Nil
  let domDyn = mkElem <$> (sequence cl)
  oldL <- get
  put $ Cons domDyn oldL
  pure res

el' :: forall i a e. String -> Dynamic (Array (Prop i)) -> FWidget i e a -> FWidget i e a
el' tagName propsDyn child = do
  let elemSpec props = ElemSpec Nothing (ElemName tagName) props
      mkElem props cl = Elem (elemSpec props) $ toUnfoldable $ reverse cl
  Tuple res cl <- lift $ runStateT child Nil
  domDyn <- liftEff $ combineDyn mkElem propsDyn (sequence cl)
  oldL <- get
  put $ Cons domDyn oldL
  pure res

widgetHoldInternal :: forall i a b e. String -> Array (Prop i) -> FWidget i e a -> Event (FWidget i e b) -> FWidget i e (Tuple a (Event b))
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
  
  _ <- liftEff $ subscribe e f

  let domDyn = mkElem <$> join (stepDyn (sequence cl0) clEvt)
  oldL <- get
  put $ Cons domDyn oldL
  pure $ Tuple res0 resEvt

widgetHold :: forall i a e. String -> Array (Prop i) -> FWidget i e a -> Event (FWidget i e a) -> FWidget i e (Dynamic a)
widgetHold tagName props w e = do
  Tuple a ea <- widgetHoldInternal tagName props w e
  pure $ stepDyn a ea

dyn :: forall i a e. String -> Array (Prop i) -> Dynamic (FWidget i e a) -> FWidget i e (Event a)
dyn tagName props d = do
  let initW = d ^. init
      evtW = d ^. ev
  snd <$> widgetHoldInternal tagName props initW evtW

-- | Wrap the child element with event processing. NOTE: if the child is a sequence of elements, it will put event listener
--   on all of them.
withEvent :: forall i e a evt. EventHandler e i evt -> FWidget i e a -> FWidget i e (Tuple a (Event evt))
withEvent handler child = do
  { event, push } <- lift create
  Tuple res cl <- lift $ runStateT child Nil
  let addProp p (Elem (ElemSpec domain name props) childArr) = Elem (ElemSpec domain name (snoc props p)) childArr
      addProp _ v = v
      newCL = map (addProp (handler push)) <$> cl
  oldL <- get
  put $ newCL <> oldL
  pure $ Tuple res event

withClickEvt :: forall i e a. FWidget i e a -> FWidget i e (Tuple a (Event Unit))
withClickEvt = withEvent onClick

withChangeEvt :: forall i e a evt. FWidget i e a -> FWidget i e (Tuple a (Event evt))
withChangeEvt = withEvent onChange

blank :: forall i e. FWidget i e Unit
blank = pure unit


linearLayout :: forall i a e. Array (Prop i) -> FWidget i e a -> FWidget i e a
linearLayout = el "linearLayout"

linearLayout' :: forall i e a. Dynamic (Array (Prop i)) -> FWidget i e a -> FWidget i e a
linearLayout' = el' "linearLayout"

relativeLayout :: forall i e a. Array (Prop i) -> FWidget i e a -> FWidget i e a
relativeLayout = el "relativeLayout"

relativeLayout' :: forall i e a. Dynamic (Array (Prop i)) -> FWidget i e a -> FWidget i e a
relativeLayout' = el' "relativeLayout"

horizontalScrollView :: forall i e a. Array (Prop i) -> FWidget i e a -> FWidget i e a
horizontalScrollView = el "horizontalScrollView"

horizontalScrollView' :: forall i e a. Dynamic (Array (Prop i)) -> FWidget i e a -> FWidget i e a
horizontalScrollView' = el' "horizontalScrollView"

scrollView :: forall i e a. Array (Prop i) -> FWidget i e a -> FWidget i e a
scrollView = el "scrollView"

scrollView' :: forall i e a. Dynamic (Array (Prop i)) -> FWidget i e a -> FWidget i e a
scrollView' = el' "scrollView"

frameLayout :: forall i e a. Array (Prop i) -> FWidget i e a -> FWidget i e a
frameLayout = el "frameLayout"

frameLayout' :: forall i e a. Dynamic (Array (Prop i)) -> FWidget i e a -> FWidget i e a
frameLayout' = el' "frameLayout"

shimmerFrameLayout :: forall i e a. Array (Prop i) -> FWidget i e a -> FWidget i e a
shimmerFrameLayout = el "shimmerFrameLayout"

shimmerFrameLayout' :: forall i e a. Dynamic (Array (Prop i)) -> FWidget i e a -> FWidget i e a
shimmerFrameLayout' = el' "shimmerFrameLayout"

tabLayout :: forall i e a. Array (Prop i) -> FWidget i e a -> FWidget i e a
tabLayout = el "tabLayout"

tabLayout' :: forall i e a. Dynamic (Array (Prop i)) -> FWidget i e a -> FWidget i e a
tabLayout' = el' "tabLayout"

imageView :: forall i e. Array (Prop i) -> FWidget i e Unit
imageView props = el "imageView" props blank

imageView' :: forall i e. Dynamic (Array (Prop i)) -> FWidget i e Unit
imageView' propsDyn = el' "imageView" propsDyn blank

editText :: forall i e. Array (Prop i) -> FWidget i e (Dynamic String)
editText props = do
  Tuple _ event <- withChangeEvt $ el "editText" props blank
  pure $ stepDyn "" event

editText' :: forall i e. Dynamic (Array (Prop i)) -> FWidget i e (Dynamic String)
editText' propsDyn = do
  Tuple _ event <- withChangeEvt $ el' "editText" propsDyn blank
  pure $ stepDyn "" event

listView :: forall i e. Array (Prop i) -> FWidget i e Unit
listView props = el "listView" props blank

listView' :: forall i e. Dynamic (Array (Prop i)) -> FWidget i e Unit
listView' propsDyn = el' "listView" propsDyn blank

progressBar :: forall i e. Array (Prop i) -> FWidget i e Unit
progressBar props = el "progressBar" props blank

progressBar' :: forall i e. Dynamic (Array (Prop i)) -> FWidget i e Unit
progressBar' propsDyn = el' "progressBar" propsDyn blank

textView :: forall i e. Array (Prop i) -> FWidget i e Unit
textView props = el "textView" props blank

textView' :: forall i e. Dynamic (Array (Prop i)) -> FWidget i e Unit
textView' propsDyn = el' "textView" propsDyn blank

viewPager :: forall i e. Array (Prop i) -> FWidget i e Unit
viewPager props = el "viewPager" props blank

viewPager' :: forall i e. Dynamic (Array (Prop i)) -> FWidget i e Unit
viewPager' propsDyn = el' "viewPager" propsDyn blank

button :: forall i e. Array (Prop i) -> FWidget i e Unit
button props = el "button" props blank

button' :: forall i e. Dynamic (Array (Prop i)) -> FWidget i e Unit
button' propsDyn = el' "button" propsDyn blank

calendar :: forall i e. Array (Prop i) -> FWidget i e Unit
calendar props = el "calendar" props blank

calendar' :: forall i e. Dynamic (Array (Prop i)) -> FWidget i e Unit
calendar' propsDyn = el' "calendar" propsDyn blank

checkBox :: forall i e. Array (Prop i) -> FWidget i e Unit
checkBox props = el "checkBox" props blank

checkBox' :: forall i e. Dynamic (Array (Prop i)) -> FWidget i e Unit
checkBox' propsDyn = el' "checkBox" propsDyn blank

switch :: forall i e. Array (Prop i) -> FWidget i e Unit
switch props = el "switch" props blank

switch' :: forall i e. Dynamic (Array (Prop i)) -> FWidget i e Unit
switch' propsDyn = el' "switch" propsDyn blank

viewWidget :: forall i e. Array (Prop i) -> FWidget i e Unit
viewWidget props = el "viewWidget" props blank

viewWidget' :: forall i e. Dynamic (Array (Prop i)) -> FWidget i e Unit
viewWidget' propsDyn = el' "viewWidget" propsDyn blank

webView :: forall i e. Array (Prop i) -> FWidget i e Unit
webView props = el "webView" props blank

webView' :: forall i e. Dynamic (Array (Prop i)) -> FWidget i e Unit
webView' propsDyn = el' "webView" propsDyn blank
