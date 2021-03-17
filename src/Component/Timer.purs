module Component.Timer where

import AppPrelude
import Control.Monad.Rec.Class (forever)
import Data.DateTime.Instant (unInstant)
import Data.Player (Player)
import Data.Time.Duration (Milliseconds(..), negateDuration)
import Data.Timer (TimeControl, Timer, nextPlayer, tickCur)
import Data.ChessTime (formatMilliseconds)
import Effect.Aff as Aff
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS
import Util (class_)

type State
  = { timer :: Timer
    , timerTickInterval :: Milliseconds
    , lastTime :: Maybe Milliseconds
    }

data Action
  = StartTimer
  | SwitchPlayer
  | Tick

type Output
  = Player

component ::
  forall query m.
  MonadEffect m =>
  MonadAff m =>
  H.Component query Timer Output m
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
    , timerTickInterval: Milliseconds 20.0
    , lastTime: Nothing
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ class_ [ "m-2" ] ]
      [ HH.h1
          [ class_ [ "mt-4", "mb-2", "text-2xl" ] ]
          [ HH.text "Timer" ]
      , HH.div_ $ (renderPlayer false <$> state.timer.prev)
          <> [ renderPlayer true state.timer.cur ]
          <> (renderPlayer false <$> state.timer.next)
      , HH.button
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
          , HE.onClick \_ -> SwitchPlayer
          ]
          [ HH.text "click to switch time" ]
      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    StartTimer -> do
      state <- H.get
      _ <- H.subscribe =<< timerSub state.timerTickInterval Tick
      nowInstant <- H.liftEffect now
      let
        nowMs = unInstant nowInstant
      H.modify_ \s ->
        s
          { timer = state.timer
          , lastTime = Just nowMs
          }
    SwitchPlayer -> H.modify_ \s -> s { timer = nextPlayer s.timer }
    Tick -> do
      state <- H.get
      -- update time of the current player with the time difference between now and lastTime
      nowMs <- unInstant <$> H.liftEffect now
      case state.lastTime of
        Just lastMs -> do
          H.modify_ \s -> s { lastTime = Just nowMs }
          case tickCur (nowMs <> negateDuration lastMs) state.timer of
            Just newTimer -> H.modify_ \s -> s { timer = newTimer }
            -- output current player if they run out of time
            Nothing -> H.raise state.timer.cur.player
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

renderPlayer :: forall p a. Boolean -> TimeControl -> HH.HTML p a
renderPlayer isActive { player, timeRemaining } =
  HH.div
    [ class_ [ "my-1" ] ]
    [ HH.h3
        [ class_ [ if isActive then "text-red-500" else "" ]
        ]
        [ HH.text $ "Player: " <> player.name ]
    , HH.p [ class_ [ "font-mono" ] ] [ HH.text $ formatMilliseconds timeRemaining ]
    ]
