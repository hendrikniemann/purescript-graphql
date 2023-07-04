module GraphQL.Execution.Result where

import Prelude

import Data.Argonaut.Core (Json, fromNumber, fromString, jsonEmptyObject, jsonNull)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.List (List(..), foldl, null, singleton, (:))
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(..), fst, snd)


data Result
  = ResultLeaf Json
  | ResultError String
  | ResultObject (List (Tuple String Result))
  | ResultList (List Result)
  | ResultNullable (Maybe Result)


data Path = PathField String | PathIndex Int


instance encodeJsonPath :: EncodeJson Path where
  encodeJson (PathField s) = fromString s
  encodeJson (PathIndex i) = fromNumber $ Int.toNumber i


newtype LocatedError = LocatedError { path :: (List Path), message :: String }


instance encodeJsonLocatedError :: EncodeJson LocatedError where
  encodeJson (LocatedError { path, message }) =
    "path" := path ~>
    "message" := message ~>
    jsonEmptyObject


pushPath :: Path -> LocatedError -> LocatedError
pushPath path (LocatedError locerr) = LocatedError $ locerr { path = path : locerr.path}


-- TODO: Follow spec on how to handle errors and null values around them:
--       Null should propagate to the next nullable field in the chain.
-- TODO: Make the path property of located errors add up to form a path (currently always Nil).
serializeResult :: Result -> Json
serializeResult result =
  let Tuple errors json = serializeWithErrors result
      dataJson = "data" := json ~> jsonEmptyObject
  in if null errors then dataJson else "errors" := errors ~> dataJson


serializeWithErrors :: Result -> Tuple (List LocatedError) Json
serializeWithErrors (ResultError message) =
  Tuple (singleton (LocatedError { path: Nil, message })) jsonNull

serializeWithErrors (ResultLeaf json) = Tuple Nil json

serializeWithErrors (ResultList list) =
  let results = map serializeWithErrors list
      errors = join $ mapWithIndex (\index -> map (pushPath $ PathIndex index) <<< fst) results
      jsons = map snd results
  in Tuple errors (encodeJson jsons)

serializeWithErrors (ResultObject list) = foldl f (Tuple Nil jsonEmptyObject) list
  where
    f :: Tuple (List LocatedError) Json -> Tuple String Result -> Tuple (List LocatedError) Json
    f (Tuple errorsAcc jsonAcc) (Tuple key res) =
      let Tuple errors json = serializeWithErrors res
      in Tuple (map (pushPath $ PathField key) errors <> errorsAcc) (key := json ~> jsonAcc)

serializeWithErrors (ResultNullable res) =
  maybe (Tuple Nil jsonNull) serializeWithErrors res
