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
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Util (class_)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type TimingState
  = { timer :: Timer
    , lostOnTime :: Maybe Player
    , isPaused :: Boolean
    }

data State
  = Settings
  | Timing TimingState

data Action
  = NewTimer Timer
  | PlayerTimeout Player
  | PauseTimer

type Slots
  = ( settings :: forall query. H.Slot query Timer Unit
    , timer :: H.Slot TimerComponent.Query Player Unit
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
    Timing
      { timer:
          { cur:
              { player: { name: "one" }, timeRemaining: (Milliseconds 3146.0) }
          , next:
              [ { player: { name: "two" }, timeRemaining: (Milliseconds 30000.0) }
              , { player: { name: "three" }, timeRemaining: (Milliseconds 92080400.0) }
              ]
          , prev: []
          }
      , lostOnTime: Nothing
      , isPaused: false
      }

  render :: State -> H.ComponentHTML Action Slots m
  render = case _ of
    Timing st ->
      HH.div
        [ class_ [ "m-2" ] ]
        [ HH.button
            [ class_
                [ "my-2"
                , "p-3"
                , "border"
                , "border-gray-300"
                , "rounded-lg"
                , "hover:bg-gray-100"
                , "transition"
                , "duration-100"
                ]
            , HE.onClick \_ -> PauseTimer
            ]
            [ HH.text "pause" ]
        , HH.slot _timer unit TimerComponent.component st.timer PlayerTimeout
        , HH.p_
            [ if st.isPaused then
                HH.text "game is paused"
              else
                HH.text ""
            ]
        , case st.lostOnTime of
            Just player -> HH.text $ "Player " <> player.name <> " lost on time"
            Nothing -> HH.text ""
        ]
    Settings -> HH.slot _settings unit Settings.component unit NewTimer

  handleAction :: forall o. Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    NewTimer t ->
      H.put
        $ Timing
            { timer: t
            , lostOnTime: Nothing
            , isPaused: false
            }
    PlayerTimeout p ->
      H.modify_ \state -> case state of
        Timing st -> Timing $ st { lostOnTime = Just p }
        _ -> state
    PauseTimer -> do
      H.tell _timer unit TimerComponent.ToggleTimer
      H.modify_ \state -> case state of
        Timing st -> Timing $ st { isPaused = not st.isPaused }
        _ -> state
