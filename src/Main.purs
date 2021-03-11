module Main where

import AppPrelude
import Data.Array.NonEmpty (NonEmptyArray, singleton)
import Data.Player (Player)
import Data.Time.Duration (Milliseconds(..))
import Data.Timer (Timer)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Util (class_)
import Component.Settings as Settings

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State
  = { timer :: Maybe Timer
    }

data Action
  = UpdateTimer Timer

type Slots
  = ( settings :: forall query. H.Slot query Timer Int )

_settings = Proxy :: Proxy "settings"

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: input -> State
  initialState _ =
    { timer: Nothing
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state = case state.timer of
    Just timer ->
      HH.div
        [ class_ [ "m-2" ] ]
        [ HH.p_ [ HH.text "hello" ]
        , HH.p_ [ HH.text $ show $ state.timer ]
        ]
    Nothing -> HH.slot _settings 0 Settings.component unit UpdateTimer

  handleAction :: forall o. Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    UpdateTimer t ->
      H.modify_ \state ->
        state { timer = Just t }
