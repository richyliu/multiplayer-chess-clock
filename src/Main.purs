module Main where

import Prelude
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

class_ :: forall r t. Array String -> HH.IProp ( "class" :: String | r ) t
class_ = HP.classes <<< map ClassName

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State
  = Int

data Action
  = Increment
  | Decrement

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ = 0

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ class_ [ "m-2" ] ]
    [ HH.button
        [ HE.onClick \_ -> Decrement
        , class_ [ "mx-2" ]
        ]
        [ HH.text "-" ]
    , HH.text (show state)
    , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Decrement -> H.modify_ \state -> state - 1
  Increment -> H.modify_ \state -> state + 1
