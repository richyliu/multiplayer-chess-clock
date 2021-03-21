module Data.Timer where

import AppPrelude
import Data.Player (Player)
import Data.Time.Duration (Milliseconds(..))
import Data.Array (uncons, snoc)
import Data.Array.NonEmpty (NonEmptyArray, head, tail)

type Timer
  = { prev :: Array TimeControl
    , cur :: TimeControl
    , next :: Array TimeControl
    }

type TimeControl
  = { player :: Player
    , timeRemaining :: Milliseconds
    }

tick :: Milliseconds -> TimeControl -> Maybe TimeControl
tick (Milliseconds delta) tc =
  let
    (Milliseconds ms) = tc.timeRemaining

    newTime = ms - delta
  in
    if newTime > 0.0 then
      Just $ tc { timeRemaining = Milliseconds newTime }
    else
      Nothing

tickCur :: Milliseconds -> Timer -> Maybe Timer
tickCur ms timer = (timer { cur = _ }) <$> tick ms timer.cur

incrementTime :: Milliseconds -> Timer -> Timer
incrementTime (Milliseconds ms) timer =
  timer
    { cur =
      timer.cur
        { timeRemaining = Milliseconds $ oldTime + ms
        }
    }
  where
  (Milliseconds oldTime) = timer.cur.timeRemaining

-- Change cur to the next player and put the old cur into prev
nextPlayer :: Timer -> Timer
nextPlayer { prev, cur, next } = case uncons next of
  Just { head, tail } ->
    { prev: snoc prev cur
    , cur: head
    , next: tail
    }
  -- wrap around to the beginning
  Nothing -> case uncons prev of
    Just { head, tail } ->
      { prev: []
      , cur: head
      , next: snoc tail cur
      }
    Nothing -> { prev: [], cur: cur, next: [] }

mkTimer :: Milliseconds -> NonEmptyArray Player -> Timer
mkTimer timeRemaining players =
  { prev: []
  , cur: ({ player: _, timeRemaining }) $ head players
  , next: ({ player: _, timeRemaining }) <$> tail players
  }
