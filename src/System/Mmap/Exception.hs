module System.Mmap.Exception where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

-- | A simple way to represent an exception occurring when performing
-- a @mmap@ computation.
data MmapException = MmapException
  deriving (Eq, Ord, Show, Typeable)

instance Exception MmapException
