module Type.Data.RowList where

import Prim.RowList (RowList)

-- | A proxy to carry information about a rowlist.
data RLProxy (rowlist :: RowList Type)
  = RLProxy
