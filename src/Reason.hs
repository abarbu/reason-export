{-| Generate Reason types, JSON decoders & JSON encoders from Haskell datatypes.
-}
module Reason
  ( module X
  ) where

import Reason.Common as X (Options(..), defaultOptions, require)
import Reason.Decoder as X
import Reason.Encoder as X
import Reason.File as X
import Reason.Record as X
import Reason.Type as X
