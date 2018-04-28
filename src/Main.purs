module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.String (length)
import FRP (FRP)
import Data.Tuple (Tuple(..))

import PrestoDOM.FRP
import PrestoDOM.FRP.Elements
import PrestoDOM.Properties
import PrestoDOM.Types.DomAttributes

type State =
  { errorMsg :: String
  , showLoading :: String
  , success :: Boolean
  , visibility :: String
  }

main :: forall eff. Eff ( console :: CONSOLE, frp :: FRP, dom :: DOM | eff ) Unit
main = runFWidget demoWidget

demoWidget :: forall i e. FWidget i e Unit
demoWidget = do
  let rootProp = [ height Match_Parent, width Match_Parent, background "#323232", gravity "center", name "rootNode"]
      centerProp = [ height $ V 600, width $ V 400, background "#000000", orientation "vertical", gravity "center"]
      vertProp = [ height $ V 150, width Match_Parent, orientation "vertical", margin "20,20,20,20"]
      loginProp = [ height $ V 30, width Match_Parent, margin "10,20,20,20", text "Login", textSize "28"]
      vert2Prop = [ height $ V 150, width Match_Parent, orientation "vertical", margin "20,0,20,0"]
      passProp = [ height $ V 30, width Match_Parent, margin "10,20,20,20", text "Password", textSize "28"]
      vert3Prop = [ height $ V 150, width Match_Parent, orientation "vertical", margin "20,20,20,20", gravity "center"]
      msgProp = [ height $ V 50, width Match_Parent, margin "20,0,20,0"]
      loginTextProp = [ height (V 40), width Match_Parent, margin "10,10,10,10", textSize "20", name "name"]
      passTextProp = [ height (V 40), width Match_Parent, margin "10,10,10,10", textSize "20", name "name", color "#00000"]
      submitAreaProp = [ height $ V 50, width Match_Parent, margin "20,20,20,20", background "#969696", gravity "center", visibility "not", name "name"]
      submitProp = [ width (V 80), height (V 25), text "Submit", textSize "28"]

      mkResProp true _ = [ width Match_Parent, height Match_Parent, text "", visibility "not", margin "20,20,20,20", textSize "20"]
      mkResProp false state = [ width Match_Parent, height Match_Parent, text $ state.errorMsg, visibility state.visibility, margin "20,20,20,20", textSize "20"]

      initialState = { errorMsg : "WRONG" , showLoading : "", success : false, visibility : "gone" }

      validate user pass oldState = if ((length user) > 0) && ((length pass) >0) && (pass == "blueberry")
                                    then oldState { errorMsg = "Welcome " <> user <> "", success = true, visibility = "visible" }
                                    else oldState { errorMsg = ":( Adda Pavi " <> user <> " !!", success = false, visibility = "visible" }
  linearLayout rootProp $ linearLayout centerProp $ do
    loginTextDyn <- linearLayout vertProp $ do
      linearLayout loginProp blank
      linearLayout [] $ editText loginTextProp

    passTextDyn <- linearLayout vert2Prop $ do
      linearLayout passProp blank       
      linearLayout [] $ editText passTextProp

    linearLayout vert3Prop $ fixEvent (\stEvt -> do
                                          let stDyn = stepDyn initialState stEvt
                                              isInitDyn = stepDyn true $ const false <$> stEvt
                                              newStDyn = validate <$> loginTextDyn <*> passTextDyn <*> stDyn
                                          propDyn <- combineDyn mkResProp isInitDyn newStDyn
                                          
                                          linearLayout msgProp $ linearLayout' propDyn blank
                                          Tuple _ clickEvt <- withClickEvt $ linearLayout submitAreaProp $ textView submitProp
                                          
                                          let newStEvt = tagDyn newStDyn clickEvt
                                          pure $ Tuple newStEvt unit)
    pure unit
