{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Mmap.Flags
( ProtOption(..), MmapOption(..)
, protExec, protRead, protWrite
, mmapAnon, mmapPrivate, mmapNone ) where

import Data.Bits (Bits, (.|.))
import Foreign.C.Types (CInt)

-- | The flags used in the module
mapAnon, mapPrivate, mapNone :: MmapFlags
mapAnon    = 0x20
mapPrivate = 0x02
mapNone    = 0x0

-- | A type representing flags for the 'mmap' function.
newtype MmapFlags = MmapFlags { unMmapFlags :: CInt }
  deriving (Eq, Show, Ord, Num, Bits)

--------------------------------------------------------------

-- | A type holding the page protection options.
newtype ProtOption = ProtOption CInt
  deriving (Eq, Show, Ord, Num, Bits)

-- | A type holding the mapping options.
newtype MmapOption = MmapOption CInt
  deriving (Eq, Show, Ord, Num, Bits)

-- | Pages can be executed.
protExec :: ProtOption
protExec = ProtOption 0x01

-- | Pages can be read.
protRead :: ProtOption
protRead = ProtOption 0x04

-- | Pages can be written.
protWrite :: ProtOption
protWrite = ProtOption 0x02

-- | No page protection.
protNone :: ProtOption
protNone = ProtOption 0x0

-- | No mapping.
mmapNone :: MmapOption
mmapNone = MmapOption (unMmapFlags mapNone)

-- | Anonymous mapping.
mmapAnon :: MmapOption
mmapAnon = MmapOption (unMmapFlags mapAnon)

-- | Private mapping.
mmapPrivate :: MmapOption
mmapPrivate = MmapOption (unMmapFlags mapPrivate)

instance Monoid ProtOption where
    mempty = protNone

instance Semigroup ProtOption where
    (<>) (ProtOption a) (ProtOption b) = ProtOption (a .|. b)

instance Monoid MmapOption where
    mempty = mmapNone

instance Semigroup MmapOption where
  (<>) (MmapOption a) (MmapOption b) = MmapOption (a .|. b)