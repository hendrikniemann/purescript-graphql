module Test.StackObserverT where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Tuple (Tuple(..))

data Operation
  = OperationPure
  | OperationLift
  | OperationMap Operation
  | OperationApply Operation Operation
  | OperationBind Operation

instance Show Operation where
  show OperationPure = "pure"
  show OperationLift= "lift"
  show (OperationMap operation) = "map (" <> show operation <> ")"
  show (OperationApply operation1 operation2) =
    "apply (" <> show operation1 <> ") (" <> show operation2 <> ")"
  show (OperationBind operation) = "bind (" <> show operation <> ")"

instance Eq Operation where
  eq OperationPure OperationPure = true
  eq OperationLift OperationLift = true
  eq (OperationMap operationA) (OperationMap operationB) = operationA == operationB
  eq (OperationApply operationA1 operationA2) (OperationApply operationB1 operationB2) =
    operationA1 == operationB1 && operationA2 == operationB2
  eq (OperationBind operationA) (OperationBind operationB) = operationA == operationB
  eq _ _ = false

data StackObserverT :: forall k. (k -> Type) -> k -> Type
data StackObserverT m a = StackObserverT Operation (m a)

unStackObserverT :: forall m a. StackObserverT m a -> m a
unStackObserverT (StackObserverT _ inner) = inner

runStackObserverT :: forall m a. StackObserverT m a -> Tuple Operation (m a)
runStackObserverT (StackObserverT operation inner) = Tuple operation inner

instance Functor m => Functor (StackObserverT m) where
  map f (StackObserverT operation inner) = StackObserverT (OperationMap operation) (map f inner)

instance Monad m => Apply (StackObserverT m) where
  apply (StackObserverT operation1 m) (StackObserverT operation2 inner) =
    StackObserverT (OperationApply operation1 operation2) (apply m inner)

instance Monad m => Applicative (StackObserverT m) where
  pure val = StackObserverT OperationPure (pure val)

instance Monad m => Bind (StackObserverT m) where
  bind (StackObserverT operation inner) f =
    StackObserverT (OperationBind operation) (bind inner (f >>> unStackObserverT))

instance Monad m => Monad (StackObserverT m)

instance MonadTrans (StackObserverT) where
  lift m = StackObserverT OperationLift m

instance MonadThrow e m => MonadThrow e (StackObserverT m) where
  throwError err = StackObserverT OperationPure (throwError err)

instance MonadError e m => MonadError e (StackObserverT m) where
  catchError (StackObserverT operation inner) f =
    StackObserverT operation (catchError inner (f >>> unStackObserverT))
