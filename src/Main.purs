module Main where

import Prelude
import Control.Monad.Reader.Class (class MonadAsk, ask, asks)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Array (reverse)
import Data.Array.NonEmpty as NE
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Data.Number.Format (toString)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_, makeAff, delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Math (round)
import Node.ReadLine as NR
import Type.Equality (class TypeEquals, from)

-- Layer 4
newtype Timer
  = Timer
  { active :: TimeControl
  , rest :: Array TimeControl
  }

instance timerShow :: Show Timer where
  show (Timer { active, rest }) =
    "Active: " <> show active <> "\n"
      <> fold ((_ <> "\n") <<< show <$> rest)

tickActive :: Milliseconds -> Timer -> Timer
tickActive delta (Timer timer) = Timer timer { active = updated timer.active }
  where
  updated (TimeControl tc) = TimeControl tc { timeRemaining = tick delta tc.timeRemaining }

nextPlayer :: Timer -> Timer
nextPlayer (Timer { active, rest }) =
  let
    -- add to active back to end of timer array
    timeControls = NE.reverse $ NE.appendArray (NE.singleton active) (reverse rest)
  in
    Timer { active: NE.head timeControls, rest: NE.tail timeControls }

newtype TimeControl
  = TimeControl
  { player :: Player
  , timeRemaining :: TimeRemaining
  }

instance timeControlShow :: Show TimeControl where
  show (TimeControl { player, timeRemaining }) = show player <> ": " <> show timeRemaining

mkTimer :: NE.NonEmptyArray Player -> TimeRemaining -> Timer
mkTimer players initialTime =
  let
    timeControls = TimeControl <<< { player: _, timeRemaining: initialTime } <$> players
  in
    Timer { active: NE.head timeControls, rest: NE.tail timeControls }

-- Nonzero number type representing player time remaining
newtype TimeRemaining
  = TimeRemaining Milliseconds

instance timeShow :: Show TimeRemaining where
  show (TimeRemaining (Milliseconds ms)) = toString (round ms / 1000.0) <> "s"

-- Subtracts delta time from the time remaining. Time cannot be less than 0
tick :: Milliseconds -> TimeRemaining -> TimeRemaining
tick (Milliseconds delta) (TimeRemaining (Milliseconds ms)) = TimeRemaining $ Milliseconds $ max 0.0 $ ms - delta

data TimeRemainingError
  = NegativeTimeRemaining

mkTimeRemaining :: Number -> Either TimeRemainingError TimeRemaining
mkTimeRemaining sec
  | sec < 0.0 = Left NegativeTimeRemaining
  | otherwise = Right <<< TimeRemaining <<< Milliseconds $ sec

newtype Player
  = Player
  { name :: String
  }

instance showPlayer :: Show Player where
  show (Player { name }) = name

mkPlayer :: String -> Player
mkPlayer name = Player { name }

-- Layer 3
-- Capability type classes:
class
  (Monad m) <= LogToScreen m where
  log :: String -> m Unit

class
  (Monad m) <= GetSettings m where
  getPlayers :: m (NE.NonEmptyArray Player)
  getTimeLimit :: m TimeRemaining

class
  (Monad m) <= GetTime m where
  wait :: Milliseconds -> m Unit

-- Business logic that uses these capabilities
-- which makes it easier to test
program ::
  forall m.
  LogToScreen m =>
  GetSettings m =>
  GetTime m =>
  m Unit
program = do
  names <- getPlayers
  timeLimit <- getTimeLimit
  log $ "Names: " <> (fold $ show <$> names)
  log $ "Time limit: " <> show timeLimit
  let
    delta = Milliseconds 1000.0

    update timer = do
      wait delta
      log $ show timer
      update $ tickActive delta timer
  update $ mkTimer names timeLimit

-- Layer 2 (Production)
-- Environment type
type Environment
  = { getInput :: String -> Aff String
    }

-- newtyped ReaderT that implements the capabilities
newtype AppM a
  = AppM (ReaderT Environment Aff a)

derive newtype instance functorTestM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Environment => MonadAsk e AppM where
  ask = AppM $ asks from

runAppM :: Environment -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

-- Layer 1 (the implementations of each instance)
instance logToScreenAppM :: LogToScreen AppM where
  log = liftEffect <<< Console.log

instance getSettingsAppM :: GetSettings AppM where
  getPlayers = do
    env <- ask
    namesRaw <- liftAff $ env.getInput "Enter player names separated by a comma: "
    let
      players = Player <<< { name: _ } <$> split (Pattern ",") namesRaw
    case NE.fromArray players of
      Just p -> pure p
      Nothing -> getPlayers
  getTimeLimit = do
    env <- ask
    timeRaw <- liftAff $ env.getInput "Enter time limit in seconds per player: "
    case fromString timeRaw of
      Just sec -> pure $ TimeRemaining $ Milliseconds (sec * 1000.0)
      Nothing -> getTimeLimit

instance getTimeAppM :: GetTime AppM where
  wait = liftAff <<< delay

-- Layer 0 (production)
main :: Effect Unit
main = do
  interface <- NR.createConsoleInterface NR.noCompletion
  let
    env =
      { getInput: prompt interface
      }

    cleanup _ = NR.close interface
  runAff_
    cleanup
    (runAppM env program)

prompt :: NR.Interface -> String -> Aff String
prompt interface message = do
  makeAff \callback -> NR.question message (callback <<< Right) interface $> mempty
