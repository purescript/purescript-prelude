module Data.Debug
  ( module Data.Debug.Type
  , module Data.Debug.Class
  , module Data.Debug.Generic
  ) where

import Data.Debug.Type (Repr, array, assoc, boolean, char, collection, constructor, int, number, opaque, opaqueLiteral, opaque_, record, string)
import Data.Debug.Class (class Debug, class DebugRowList, debug, debugRowList)
import Data.Debug.Generic (class GenericDebug, class GenericDebugArgs, genericDebug, genericDebug', genericDebugArgs)
