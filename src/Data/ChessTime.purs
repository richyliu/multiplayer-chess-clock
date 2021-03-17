module Data.ChessTime where

import AppPrelude
import Data.Array (replicate)
import Data.Int (floor)
import Data.String (length)
import Data.Time.Duration (Milliseconds(..))

formatMilliseconds :: Milliseconds -> String
formatMilliseconds (Milliseconds ms) =
  let
    hours = (floor $ ms / 1000.0 / 60.0 / 60.0) `mod` 60

    hourStr =
      if hours == 0 then
        ""
      else
        padStartZero 2 hours <> ":"

    minutes = (floor $ ms / 1000.0 / 60.0) `mod` 60

    seconds = (floor $ ms / 1000.0) `mod` 60

    subSec = (floor $ ms / 100.0) `mod` 10
  in
    hourStr
      <> padStartZero 2 minutes
      <> ":"
      <> padStartZero 2 seconds
      <> "."
      <> padStartZero 1 subSec

-- Adds "0"s so that the string representation of `num` has length `digit`
padStartZero :: Int -> Int -> String
padStartZero digits num =
  let
    str = show num

    addZeros = digits - length str
  in
    if addZeros > 0 then
      (fold $ replicate addZeros "0") <> str
    else
      str
