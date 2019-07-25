module GraphQL.Execution.Result where

import Prelude

import Data.Argonaut.Core (Json, fromNumber, fromString, jsonEmptyObject, jsonNull)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Int as Int
import Data.List (List(..), foldl, null, singleton)
import Data.Tuple (Tuple(..), fst, snd)

data Result
  = ResultLeaf Json
  | ResultError String
  | ResultObject (List (Tuple String Result))
  | ResultList (List Result)

data Path = PathField String | PathIndex Int

instance encodeJsonPath :: EncodeJson Path where
  encodeJson (PathField s) = fromString s
  encodeJson (PathIndex i) = fromNumber $ Int.toNumber i

newtype LocatedError = LocatedError { path :: (List Path), message :: String }

instance encodeJsonLocatedError :: EncodeJson LocatedError where
  encodeJson (LocatedError { path, message }) = "path" := path ~> "message" := message

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
      errors = results >>= fst
      jsons = map snd results
  in Tuple errors (encodeJson jsons)
serializeWithErrors (ResultObject list) = foldl f (Tuple Nil jsonEmptyObject) list
  where
    f :: Tuple (List LocatedError) Json -> Tuple String Result -> Tuple (List LocatedError) Json
    f (Tuple errorsAcc jsonAcc) (Tuple key res) =
      let Tuple errors json = serializeWithErrors res
      in Tuple (errors <> errorsAcc) (key := json ~> jsonAcc)
