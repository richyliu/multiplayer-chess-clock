module Data.Timer where

import AppPrelude
import Data.Player (Player)
import Data.Time.Duration (Milliseconds(..))
import Data.Array.NonEmpty (NonEmptyArray, head, tail)

type Timer
  = { cur :: TimeControl
    , rest :: Array TimeControl
    }

type TimeControl
  = { player :: Player
    , timeRemaining :: Milliseconds
    }

tick :: Milliseconds -> TimeControl -> TimeControl
tick (Milliseconds delta) tc =
  let
    (Milliseconds ms) = tc.timeRemaining
  in
    tc { timeRemaining = Milliseconds $ max 0.0 (ms - delta) }

mkTimer :: Milliseconds -> NonEmptyArray Player -> Timer
mkTimer timeRemaining players =
  { cur: ({ player: _, timeRemaining }) $ head players
  , rest: ({ player: _, timeRemaining }) <$> tail players
  }
