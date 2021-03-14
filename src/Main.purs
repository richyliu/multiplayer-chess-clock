module Main where

import AppPrelude
import Component.HTML.Timer (renderTimer)
import Component.Settings as Settings
import Control.Monad.Rec.Class (forever)
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff as Aff
import Data.Timer (Timer, nextPlayer, tick)
import Effect.Now (now)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Util (class_)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State
  = { timer :: Maybe Timer
    , timerSubId :: Maybe H.SubscriptionId
    , timerTickInterval :: Milliseconds
    , lastTime :: Maybe Milliseconds
    }

data Action
  = NewTimer Timer
  | SwitchPlayer
  | Tick

type Slots
  = ( settings :: forall query. H.Slot query Timer Int )

_settings = Proxy :: Proxy "settings"

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
    , timerSubId: Nothing
    , timerTickInterval: Milliseconds 100.0
    , lastTime: Nothing
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state = case state.timer of
    Just timer ->
      HH.div
        [ class_ [ "m-2" ] ]
        [ HH.p_ [ HH.text "hello" ]
        , HH.p_ [ HH.text $ show $ state.timer ]
        , renderTimer SwitchPlayer timer
        ]
    Nothing -> HH.slot _settings 0 Settings.component unit NewTimer

  handleAction :: forall o. Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    NewTimer t -> do
      state <- H.get
      case state.timerSubId of
        Just oldId -> H.unsubscribe oldId
        Nothing -> pure unit
      id <- H.subscribe =<< timerSub state.timerTickInterval Tick
      nowInstant <- H.liftEffect now
      let
        nowMs = unInstant nowInstant
      H.put
        $ state
            { timer = Just t
            , timerSubId = Just id
            , lastTime = Just nowMs
            }
      pure unit
    SwitchPlayer ->
      H.modify_ \state ->
        state { timer = nextPlayer <$> state.timer }
    Tick -> do
      state@{ lastTime: maybeLastTime, timer: maybeTimer } <- H.get
      -- update time of the current player with the time difference
      case maybeTimer of
        Just timer -> do
          nowInstant <- H.liftEffect now
          let
            nowMs = unInstant nowInstant
          case maybeLastTime of
            Just (Milliseconds lastTime) ->
              let
                (Milliseconds nowTime) = nowMs

                diff = nowTime - lastTime
              in
                when (diff > 0.0)
                  $ H.put
                  $ state
                      { timer = Just $ timer { cur = tick (Milliseconds diff) timer.cur }
                      , lastTime = Just nowMs
                      }
            Nothing -> pure unit
        -- how can you tick when there's no timer?
        Nothing -> pure unit

timerSub :: forall m a. MonadAff m => Milliseconds -> a -> m (HS.Emitter a)
timerSub duration val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <-
    H.liftAff $ Aff.forkAff
      $ forever do
          Aff.delay duration
          H.liftEffect $ HS.notify listener val
  pure emitter
