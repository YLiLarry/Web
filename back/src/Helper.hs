module Helper (
      module Debug.Trace
    , _p
) where

import Debug.Trace 

_p a = trace (show a) a
