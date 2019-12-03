module System.Mmap.Exception where

import Data.Typeable (Typeable)
import Control.Exception (Exception)

-- | A simple way to represent an exception occurring when performing
-- a @mmap@ computation.
data MmapException = MmapException
  deriving (Eq, Ord, Show, Typeable)

instance Exception MmapException