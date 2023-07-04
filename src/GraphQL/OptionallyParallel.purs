module GraphQL.OptionallyParallel where

import Prelude

import Control.Parallel (class Parallel, parallel, sequential)

class OptionallyParallel :: (Type -> Type) -> (Type -> Type) -> Constraint
class (Monad m, Applicative f) <= OptionallyParallel f m | m -> f, f -> m where
  optionallyParallel :: m ~> f
  optionallySequential :: f ~> m

instance (Parallel f m) => OptionallyParallel f m where
  optionallyParallel = parallel
  optionallySequential = sequential

instance (Monad m) => OptionallyParallel m m where
  optionallyParallel = identity
  optionallySequential = identity
