module Data.Timer where

import AppPrelude
import Data.Player (Player)
import Data.Time.Duration (Milliseconds(..))
import Data.Array (uncons, snoc)
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

-- Change cur to the next player and put the old cur to the end of player list (rest)
nextPlayer :: Timer -> Timer
nextPlayer { cur, rest } = case uncons rest of
  Just { head, tail } ->
    { cur: head
    , rest: snoc tail cur
    }
  Nothing -> { cur, rest }

mkTimer :: Milliseconds -> NonEmptyArray Player -> Timer
mkTimer timeRemaining players =
  { cur: ({ player: _, timeRemaining }) $ head players
  , rest: ({ player: _, timeRemaining }) <$> tail players
  }
