module Main where

import AppPrelude
import Component.Settings as Settings
import Component.Timer as TimerComponent
import Data.Timer (Timer)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State
  = { timer :: Maybe Timer
    }

data Action
  = NewTimer Timer

type Slots
  = ( settings :: forall query. H.Slot query Timer Int
    , timer :: forall query. H.Slot query Void Int
    )

_settings = Proxy :: Proxy "settings"

_timer = Proxy :: Proxy "timer"

component ::
  forall query input output m.
  MonadEffect m =>
  MonadAff m =>
  H.Component query input output m
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
    Just timer -> HH.slot _timer 0 TimerComponent.component timer absurd
    Nothing -> HH.slot _settings 0 Settings.component unit NewTimer

  handleAction :: forall o. Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    NewTimer t -> H.modify_ \state -> state { timer = Just t }
