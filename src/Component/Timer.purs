module Component.Timer where

import AppPrelude
import Component.HTML.Timer (renderTimer)
import Control.Monad.Rec.Class (forever)
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff as Aff
import Data.Timer (Timer, nextPlayer, tick)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Util (class_)

type State
  = { timer :: Timer
    , timerTickInterval :: Milliseconds
    , timerSubId :: Maybe H.SubscriptionId
    , lastTime :: Maybe Milliseconds
    }

data Action
  = StartTimer
  | SwitchPlayer
  | Tick

component ::
  forall query output m.
  MonadEffect m =>
  MonadAff m =>
  H.Component query Timer output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , initialize = Just StartTimer
            }
    }
  where
  initialState :: Timer -> State
  initialState initialTimer =
    { timer: initialTimer
    , timerSubId: Nothing
    , timerTickInterval: Milliseconds 100.0
    , lastTime: Nothing
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ class_ [ "m-2" ] ]
      [ HH.p_ [ HH.text "hello" ]
      , HH.p_ [ HH.text $ show $ state.timer ]
      , renderTimer SwitchPlayer state.timer
      ]

  handleAction :: forall o. Action -> H.HalogenM State Action () o m Unit
  handleAction = case _ of
    StartTimer -> do
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
            { timer = state.timer
            , timerSubId = Just id
            , lastTime = Just nowMs
            }
      pure unit
    SwitchPlayer ->
      H.modify_ \state ->
        state { timer = nextPlayer state.timer }
    Tick -> do
      state@{ lastTime: maybeLastTime, timer } <- H.get
      -- update time of the current player with the time difference
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
                  { timer = timer { cur = tick (Milliseconds diff) timer.cur }
                  , lastTime = Just nowMs
                  }
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
