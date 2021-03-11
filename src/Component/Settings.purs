module Component.Settings where

import AppPrelude
import Data.Array.NonEmpty (NonEmptyArray, singleton)
import Data.Player (Player)
import Data.Time.Duration (Milliseconds(..))
import Data.Timer (Timer, mkTimer)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Util (class_)

type State
  = { players :: NonEmptyArray Player
    , timeLimit :: Milliseconds
    }

data Action
  = TestInput String
  | Submit

type Output
  = Timer

component :: forall query input m. H.Component query input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: input -> State
  initialState _ =
    { players: singleton { name: "" }
    , timeLimit: Milliseconds 1000.0
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ class_ [ "m-2", "p-1", "border", "border-gray-400" ] ]
      [ HH.h2
          [ class_ [ "text-2xl", "font-light" ] ]
          [ HH.text "Settings component" ]
      , HH.p_ [ HH.text $ fold $ show <$> state.players ]
      , HH.input
          [ HP.placeholder "hello"
          , HE.onValueInput TestInput
          , class_ [ "px-1", "my-2", "border", "border-gray-300" ]
          ]
      , HH.button
          [ class_
              [ "mx-2"
              , "px-3"
              , "py-1"
              , "border"
              , "border-gray-300"
              , "text-sm"
              , "font-light"
              , "rounded-md"
              , "bg-gray-50"
              , "hover:bg-gray-100"
              ]
          , HE.onClick \_ -> Submit
          ]
          [ HH.text "Submit" ]
      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    TestInput s -> H.modify_ \state -> state { players = singleton { name: s } }
    Submit -> do
      state <- H.get
      H.raise $ mkTimer state.timeLimit state.players
