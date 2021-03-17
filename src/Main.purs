module Main where

import AppPrelude
import Component.Settings as Settings
import Component.Timer as TimerComponent
import Data.Player (Player)
import Data.Time.Duration (Milliseconds(..))
import Data.Timer (Timer)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Util (class_)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State
  = { timer :: Maybe Timer
    , lostOnTime :: Maybe Player
    }

data Action
  = NewTimer Timer
  | PlayerTimeout Player

type Slots
  = ( settings :: forall query. H.Slot query Timer Int
    , timer :: forall query. H.Slot query Player Int
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
    { timer:
        Just
          { cur:
              { player: { name: "one" }, timeRemaining: (Milliseconds 3146.0) }
          , next:
              [ { player: { name: "two" }, timeRemaining: (Milliseconds 30000.0) }
              , { player: { name: "three" }, timeRemaining: (Milliseconds 92080400.0) }
              ]
          , prev: []
          }
    , lostOnTime: Nothing
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ class_ [ "m-2" ] ]
      [ case state.timer of
          Just timer -> HH.slot _timer 0 TimerComponent.component timer PlayerTimeout
          Nothing -> HH.slot _settings 0 Settings.component unit NewTimer
      , case state.lostOnTime of
          Just player -> HH.text $ "Player " <> player.name <> " lost on time"
          Nothing -> HH.text ""
      ]

  handleAction :: forall o. Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    NewTimer t -> H.modify_ \state -> state { timer = Just t }
    PlayerTimeout p -> H.modify_ \state -> state { lostOnTime = Just p }
