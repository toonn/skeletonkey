module MPAlgorithm
  ( masterKey
  , sitePassword
  ) where

import Types
import MAC
import KDF
import MPWTemplates
import Data.Vector.Unboxed as U (Vector, length, (++), fromList)
import Data.Word
import Data.Bits (shiftR)
import System.Endian (toBE32)

sitePassword :: MasterKey -> SiteName -> SitePassword
sitePassword mk (SName sn) = pin (mpwMAC mk ss)
  where
    ss = SSeed $ mkVec "com.lyndir.masterpassword"
          U.++ bigEndian (U.length sn) U.++ sn U.++ bigEndian 1 -- ++ counter 32Bit BE


masterKey :: MasterPassword -> UserName -> MasterKey
masterKey mp (UName un) = mpwKDF mp us
  where
    us = USeed $ mkVec "com.lyndir.masterpassword"
          U.++ bigEndian (U.length un) U.++ un

bigEndian :: Int -> U.Vector Word8
bigEndian w = fromList [ w32Byte w i | i <- [24,16..0] ]
  where
    w32Byte :: Int -> Int -> Word8
    w32Byte w i = fromIntegral (toBE32 (fromIntegral w) `shiftR` i)

-- To ensure cross-platform compatibility, we define all data as byte streams
-- using the following encodings for other types:
--   - Strings are encoded as UTF-8
--   - Numbers are encoded as 32-bit unsigned integers in network byte order

test = mkVec "test"
