module GraphQL.OptionallyParallel where

import Prelude

import Control.Monad.Reader (ReaderT, mapReaderT)
import Control.Parallel (class Parallel, parallel, sequential)
import Data.Either (Either)
import Effect (Effect)

class OptionallyParallel :: (Type -> Type) -> (Type -> Type) -> Constraint
class (Monad m, Applicative f) <= OptionallyParallel f m | m -> f, f -> m where
  optionallyParallel :: m ~> f
  optionallySequential :: f ~> m


instance OptionallyParallel (Either a) (Either a) where
  optionallyParallel = identity
  optionallySequential = identity

else instance (OptionallyParallel f m) => OptionallyParallel (ReaderT a f) (ReaderT a m) where
  optionallyParallel = mapReaderT optionallyParallel
  optionallySequential = mapReaderT optionallySequential

else instance OptionallyParallel Effect Effect where
  optionallyParallel = identity
  optionallySequential = identity

else instance (Parallel f m) => OptionallyParallel f m where
  optionallyParallel = parallel
  optionallySequential = sequential
