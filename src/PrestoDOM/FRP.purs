module PrestoDOM.FRP (
    Dynamic(..)
    , stepDyn
    , combineDyn
    , nubDyn
    , count
    , toggle
    , tagDyn
    , attachDyn
    , switchDyn
    , joinDyn
    , init
    , beh
    , ev
    , fixEvent
	) where

import Prelude

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Lens (Getter', Lens', lens, (^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Filterable (filter)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import FRP (FRP)
import FRP.Behavior (Behavior, step)
import FRP.Event (Event, create, subscribe, sampleOn, withLast, keepLatest, sampleOn_)

data Dynamic a = Dynamic
  {
    init :: a
  , beh :: Behavior a
  , event :: Event a
  }

instance functorDynamic :: Functor Dynamic where
  map f (Dynamic d) = Dynamic { init: f d.init, beh: map f d.beh, event: map f d.event }

instance applyDynamic :: Apply Dynamic where
  apply (Dynamic f) (Dynamic d) = Dynamic {
    init: f.init d.init,
    beh: apply f.beh d.beh,
    event: apply f.event d.event
    }

instance applicativeDynamic :: Applicative Dynamic where
  pure a = let ev = pure a
               b = step a ev
           in Dynamic { init: a,  beh: b, event: ev }

stepDyn :: forall a. a -> Event a -> Dynamic a
stepDyn v e = Dynamic { init: v, beh: step v e, event: e }

combineDyn :: forall a b c e m. MonadEff (frp :: FRP | e) m => (a -> b -> c) -> Dynamic a -> Dynamic b -> m (Dynamic c)
combineDyn f d1 d2 = do
  let initVal = f (d1 ^. init) (d2 ^. init)
      e1 = sampleOn (d2 ^. ev) (f <$> (d1 ^. ev))
      e2 = sampleOn (d1 ^. ev) (flip f <$> (d2 ^. ev))
  { event, push } <- liftEff create
  _ <- liftEff $ subscribe e1 push
  _ <- liftEff $ subscribe e2 push
  pure $ stepDyn initVal event

-- | Create a Dynamic that only signals changes if the values actually changed
nubDyn :: forall a. Eq a => Dynamic a -> Dynamic a
nubDyn d = Dynamic { init: dv, beh: step dv e, event: e }
  where dv = d ^. init
        e = map _.now $ filter notSame $ withLast $ d ^. ev
        notSame { now: n, last: ml } = not $ Just n == ml

-- | Create a Dynamic that counts the occurences of the Event
count :: forall e m a. MonadEff (frp :: FRP | e) m => Event a -> m (Dynamic Int)
count ev = fixEvent (\numEvt -> do
                        let numDyn = stepDyn 0 numEvt
                            newNumEvt = sampleOn numEvt $ const ((+) 1) <$> ev
                        pure $ Tuple newNumEvt numDyn
                    )

-- | create a new Dynamic using the initial value that flips its value every time the event occurs
toggle :: forall a e m. MonadEff (frp :: FRP | e) m => Boolean -> Event a -> m (Dynamic Boolean)
toggle init ev = fixEvent (\valE -> do
                              let valD = stepDyn init valE
                                  newValE = sampleOn valE $ const not <$> ev
                              pure $ Tuple newValE valD
                              )

-- | Replace the value of the Event with the current value of the Dynamic
tagDyn :: forall a b. Dynamic a -> Event b -> Event a
tagDyn d e = sampleOn_ (d ^. ev) e

-- | Combine the current value of the Dynamic and Event each time the Event occurs
attachDyn :: forall a b c. (a -> b -> c) -> Dynamic a -> Event b -> Event c
attachDyn f d e = sampleOn (d ^. ev) $ flip f <$> e

-- | switches to the new Event whenever it receives one
switchDyn :: forall a. Dynamic (Event a) -> Event a
switchDyn d = keepLatest (d ^. ev)
  
-- | join a nested Dynamic into a new Dynamic that has the value of the inner Dynamic
joinDyn :: forall a. Dynamic (Dynamic a) -> Dynamic a
joinDyn d = stepDyn initVal evt
  where initVal = (d ^. init) ^. init
        evt = keepLatest (getEv <$> d ^. ev)
        getEv dyn = dyn ^. ev
  
_Dynamic :: forall a . Lens' (Dynamic a) { init :: a, beh :: Behavior a, event :: Event a }
_Dynamic = lens (\(Dynamic rec) -> rec) (\_ -> Dynamic)

init :: forall a. Getter' (Dynamic a) a
init = _Dynamic <<< (prop (SProxy :: SProxy "init"))

beh :: forall a. Getter' (Dynamic a) (Behavior a)
beh = _Dynamic <<< (prop (SProxy :: SProxy "beh"))

ev :: forall a. Getter' (Dynamic a) (Event a)
ev = _Dynamic <<< (prop (SProxy :: SProxy "event"))


fixEvent :: forall a b eff m. MonadEff (frp :: FRP | eff) m => (Event a -> m (Tuple (Event a) b)) -> m b
fixEvent f = do
  { event, push } <- liftEff create
  Tuple event' result <- f event
  _ <- liftEff $ subscribe event' push
  pure result
