module AppPrelude (module Exports) where

-- Export the things we care about
import Effect (Effect) as Exports
import Effect.Class (class MonadEffect) as Exports
import Effect.Aff.Class (class MonadAff) as Exports
import Data.Maybe (Maybe(..)) as Exports
import Data.Foldable (fold) as Exports
import Data.Either (Either(..)) as Exports
import Type.Proxy (Proxy(..)) as Exports

-- Re-export what the Prelude module (as of prelude v5.0.0) rexports:
import Control.Applicative (class Applicative, pure, liftA1, unless, when) as Exports
import Control.Apply (class Apply, apply, (*>), (<*), (<*>)) as Exports
import Control.Bind (class Bind, bind, class Discard, discard, ifM, join, (<=<), (=<<), (>=>), (>>=)) as Exports
import Control.Category (class Category, identity) as Exports
import Control.Monad (class Monad, ap, liftM1, unlessM, whenM) as Exports
import Control.Semigroupoid (class Semigroupoid, compose, (<<<), (>>>)) as Exports

import Data.Boolean (otherwise) as Exports
import Data.BooleanAlgebra (class BooleanAlgebra) as Exports
import Data.Bounded (class Bounded, bottom, top) as Exports
import Data.CommutativeRing (class CommutativeRing) as Exports
import Data.DivisionRing (class DivisionRing, recip) as Exports
import Data.Eq (class Eq, eq, notEq, (/=), (==)) as Exports
import Data.EuclideanRing (class EuclideanRing, degree, div, mod, (/), gcd, lcm) as Exports
import Data.Field (class Field) as Exports
import Data.Function (const, flip, ($), (#)) as Exports
import Data.Functor (class Functor, flap, map, void, ($>), (<#>), (<$), (<$>), (<@>)) as Exports
import Data.HeytingAlgebra (class HeytingAlgebra, conj, disj, not, (&&), (||)) as Exports
import Data.Monoid (class Monoid, mempty) as Exports
import Data.NaturalTransformation (type (~>)) as Exports
import Data.Ord (class Ord, compare, (<), (<=), (>), (>=), comparing, min, max, clamp, between) as Exports
import Data.Ordering (Ordering(..)) as Exports
import Data.Ring (class Ring, negate, sub, (-)) as Exports
import Data.Semigroup (class Semigroup, append, (<>)) as Exports
import Data.Semiring (class Semiring, add, mul, one, zero, (*), (+)) as Exports
import Data.Show (class Show, show) as Exports
import Data.Unit (Unit, unit) as Exports
import Data.Void (Void, absurd) as Exports
