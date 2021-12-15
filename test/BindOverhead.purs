module Test.BindOverhead (bindOverheadSpec) where

import Prelude

import Data.Argonaut as Json
import Data.Either (Either)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class.Console as Console
import Effect.Exception (Error)
import GraphQL ((:>), (!>))
import GraphQL as GQL
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.StackObserverT (runStackObserverT, StackObserverT, Operation(..))

type Context = StackObserverT (Either Error)

testSchema :: GQL.Schema (StackObserverT (Either Error)) Unit
testSchema = GQL.Schema { query: queryType, mutation: Nothing }

queryType :: GQL.ObjectType (StackObserverT (Either Error)) Unit
queryType = GQL.objectType "Query"
  :> GQL.field "standard" GQL.string
    !> (\_ _ -> pure "")
  :> GQL.field "mapping" GQL.string
    !> (\_ _ -> pure "")

bindOverheadSpec :: Spec Unit
bindOverheadSpec =
  describe "bind overhead" do
    it "calls bind for standard resolver" do
      let result = GQL.graphql testSchema "{ standard }" Map.empty Nothing unit
      let (Tuple operation _) = runStackObserverT (result)
      operation `shouldEqual` (OperationBind OperationPure)

    it "calls map for mapping resolver" do
      let result = GQL.graphql testSchema "{ mapping }" Map.empty Nothing unit
      let (Tuple operation eith) = runStackObserverT (result)
      Console.log $ show (eith <#> Json.stringify)
      operation `shouldEqual` (OperationMap OperationPure)
