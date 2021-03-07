module Main where

import Prelude

import Control.Monad.Reader.Class (class MonadAsk, ask, asks)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Array (reverse)
import Data.Array.NonEmpty as NE
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Number.Format (toString)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_, makeAff)
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

mkTimer :: NE.NonEmptyArray Player -> TimeRemaining -> Timer
mkTimer players initialTime =
  let
    timeControls = TimeControl <<< { player: _, timeRemaining: initialTime } <$> players
  in
    Timer { active: NE.head timeControls, rest: NE.tail timeControls }

-- Nonzero number type representing player time remaining
newtype TimeRemaining
  = TimeRemaining Milliseconds

-- Subtracts delta time from the time remaining. Returns Nothing if time reaches 0
tick :: Milliseconds -> TimeRemaining -> Maybe TimeRemaining
tick (Milliseconds delta) (TimeRemaining (Milliseconds ms)) =
  let
    newTime = ms - delta
  in
    if newTime <= 0.0 then
      Nothing
    else
      Just $ TimeRemaining $ Milliseconds newTime

data TimeRemainingError
  = NegativeTimeRemaining

mkTimeRemaining :: Number -> Either TimeRemainingError TimeRemaining
mkTimeRemaining sec
  | sec < 0.0 = Left NegativeTimeRemaining
  | otherwise = Right <<< TimeRemaining <<< Milliseconds $ sec

instance timeShow :: Show TimeRemaining where
  show (TimeRemaining (Milliseconds ms)) = toString (round ms / 1000.0) <> "s"

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

-- Business logic that uses these capabilities
-- which makes it easier to test
program ::
  forall m.
  LogToScreen m =>
  GetSettings m =>
  m Unit
program = do
  names <- getPlayers
  timeLimit <- getTimeLimit
  log $ "Names: " <> (fold $ show <$> names)
  log $ "Time limit: " <> show timeLimit
  pure unit

-- Layer 2 (Production)
-- Environment type
type Environment
  = { someValue :: Int
    , getInput :: String -> Aff String
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
      namesSplit = Player <<< { name: _ } <$> split (Pattern ",") namesRaw

      defaultPlayer = NE.singleton $ Player { name: "Unnamed" }
    pure $ fromMaybe defaultPlayer (NE.fromArray namesSplit)
  getTimeLimit = do
    env <- ask
    timeRaw <- liftAff $ env.getInput "Enter time limit per player: "
    let
      time = fromMaybe 5000.0 $ fromString timeRaw
    pure $ TimeRemaining $ Milliseconds time

-- Layer 0 (production)
main :: Effect Unit
main = do
  interface <- NR.createConsoleInterface NR.noCompletion
  let
    env =
      { someValue: 1
      , getInput: prompt interface
      }

    cleanup _ = NR.close interface
  runAff_
    cleanup
    (runAppM env program)

prompt :: NR.Interface -> String -> Aff String
prompt interface message = do
  makeAff \callback -> NR.question message (callback <<< Right) interface $> mempty
