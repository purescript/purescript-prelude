module Type.Data.RowList where

import Prim.RowList (kind RowList)

-- | A proxy to carry information about a rowlist.
data RLProxy (rowlist :: RowList)
  = RLProxy
