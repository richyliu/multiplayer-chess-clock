module Component.HTML.Timer where

import AppPrelude
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Util (class_)
import Data.Timer (Timer)

renderTimer :: forall p a. a -> Timer -> HH.HTML p a
renderTimer timerClicked timer =
  HH.div
    [ class_ [ "m-2" ] ]
    [ HH.p_ [ HH.text "hello" ]
    , HH.button
        [ HE.onClick \_ -> timerClicked ]
        [ HH.text "click to switch time" ]
    ]
