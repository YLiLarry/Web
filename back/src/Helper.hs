module Helper (
      module Debug.Trace
    , module Helper.ByteString
    , _p
) where

import Debug.Trace 
import Helper.ByteString

_p a = trace (show a) a

