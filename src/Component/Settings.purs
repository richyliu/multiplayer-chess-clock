module Component.Settings where

import AppPrelude
import Data.Array.NonEmpty (NonEmptyArray, singleton, fromArray)
import Data.Number (fromString)
import Data.Player (Player)
import Data.String (Pattern(..), split)
import Data.Time.Duration (Milliseconds(..))
import Data.Timer (Timer, mkTimer)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Util (class_)

type State
  = { players :: NonEmptyArray Player
    , timeLimit :: Either TimeLimitError Milliseconds
    }

data TimeLimitError
  = TimeLimitNotANumber
  | TimeLimitNonPositive
  | TimeLimitEmpty

data Action
  = ChangePlayerNames String
  | ChangeTimeLimit String
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
    , timeLimit: Left TimeLimitEmpty
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ class_ [ "m-2", "p-1", "border", "border-gray-400" ] ]
      [ HH.h2
          [ class_ [ "text-2xl", "font-light" ] ]
          [ HH.text "Settings component" ]
      , HH.p_ [ HH.text $ fold $ show <$> state.players ]
      , HH.div []
          [ HH.input
              [ HP.placeholder "Time per person (sec)"
              , HE.onValueInput ChangeTimeLimit
              , class_ [ "px-1", "my-2", "border", "border-gray-300" ]
              ]
          , HH.span
              [ class_
                  [ "ml-2"
                  , "text-sm"
                  , "text-red-700"
                  ]
              ]
              [ HH.text
                  $ case state.timeLimit of
                      Left TimeLimitNonPositive -> "Time limit must be positive"
                      Left TimeLimitNotANumber -> "Time limit must be a number"
                      _ -> ""
              ]
          ]
      , HH.div []
          [ HH.input
              [ HP.placeholder "Names (separated by comma)"
              , HE.onValueInput ChangePlayerNames
              , class_ [ "px-1", "my-2", "border", "border-gray-300" ]
              ]
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
    ChangePlayerNames s ->
      H.modify_ \state ->
        state
          { players =
            case fromArray (split (Pattern ",") s) of
              Just names -> { name: _ } <$> names
              Nothing -> singleton { name: s }
          }
    ChangeTimeLimit s -> H.modify_ \state -> state { timeLimit = newLimit }
      where
      newLimit = case fromString s of
        Just timeLimit ->
          if timeLimit > 0.0 then
            Right $ Milliseconds $ timeLimit * 1000.0
          else
            Left TimeLimitNonPositive
        Nothing -> Left TimeLimitNotANumber
    Submit -> do
      state <- H.get
      case state of
        { players, timeLimit: (Right tL) } -> H.raise $ mkTimer tL state.players
        _ -> pure unit
