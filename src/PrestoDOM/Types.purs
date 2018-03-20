module PrestoDOM.Types
    ( Rec
    , EventValue(..)
    , Props(..)
    , EventResp
    , module VDom
    , module Types
    ) where

import Prelude

import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode, encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import FRP.Event (Event, subscribe)

import Halogen.VDom.Types (VDom(..), ElemSpec(..), ElemName(..), Namespace(..)) as VDom

import PrestoDOM.Types.DomAttributes as Types

type Rec t = {| t }

data EventValue = ValueS String | ValueI Int
data Props t =  Props { id :: String | t }
type EventResp t u = { value :: EventValue, props :: (Props u) | t}
