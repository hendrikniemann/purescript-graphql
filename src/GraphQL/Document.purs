module GraphQL.Document (Document) where

import Prelude

foreign import data Document :: Type

foreign import nativeShowDocument :: Document -> String

instance documentShow :: Show Document where
  show = nativeShowDocument
