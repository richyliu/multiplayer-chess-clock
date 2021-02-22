module Main where

import Prelude
import Helpers (classes)
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource as EventSource
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI parent unit body

type Slots
  = ( button :: forall q. H.Slot q Output Unit )

_button = SProxy :: SProxy "button"

type ParentState
  = { count :: Int }

data ParentAction
  = Initialize
  | Increment

parent :: forall query input output m. MonadAff m => H.Component HH.HTML query input output m
parent =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }
  where
  initialState :: input -> ParentState
  initialState _ = { count: 0 }

  render :: ParentState -> H.ComponentHTML ParentAction Slots m
  render { count } = HH.div_ [ HH.slot _button unit button { label: show count } \_ -> Just Increment ]

  handleAction :: ParentAction -> H.HalogenM ParentState ParentAction Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.subscribe
        $ EventSource.affEventSource \emitter -> do
            fiber <-
              Aff.forkAff
                $ forever do
                    Aff.delay $ Milliseconds 1000.0
                    EventSource.emit emitter Increment
            pure
              $ EventSource.Finalizer do
                  Aff.killFiber (error "Event source finalized") fiber
    Increment -> H.modify_ \st -> st { count = st.count + 1 }

-- Now we turn to our child component, the button.
data ButtonAction
  = Receive ButtonInput
  | Click

type ButtonInput
  = { label :: String }

data Output
  = Clicked

type ButtonState
  = { label :: String }

button :: forall query m. H.Component HH.HTML query ButtonInput Output m
button =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = Just <<< Receive
              }
    }
  where
  initialState :: ButtonInput -> ButtonState
  initialState { label } = { label }

  render :: ButtonState -> H.ComponentHTML ButtonAction () m
  render { label } =
    HH.button
      [ HE.onClick \_ -> Just Click
      , classes
          [ "rounded"
          , "bg-green-500"
          , "text-white"
          , "w-12"
          , "m-2"
          ]
      ]
      [ HH.text label ]

  handleAction :: ButtonAction -> H.HalogenM ButtonState ButtonAction () Output m Unit
  handleAction = case _ of
    -- When we receive new input we update our `label` field in state.
    Receive input -> H.modify_ _ { label = input.label }
    Click -> H.raise Clicked
