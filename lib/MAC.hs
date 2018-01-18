module MAC where

import Types

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteArray as BA (unpack)

import qualified Crypto.Hash.BLAKE2.BLAKE2b as Blake2 (hash)

import qualified Crypto.MAC.HMAC as CMH (hmac, HMAC)
import qualified Crypto.Hash.Algorithms as CHA (SHA256)

blake2 :: MasterKey -> SiteSeed -> TemplateSeed
blake2 = (TSeed . bs2V) ... ((Blake2.hash 64 `on` v2BS) `on2` unMK) unSS

hmacSHA256 :: MasterKey -> SiteSeed -> TemplateSeed
hmacSHA256 = (TSeed . digest2V) ... ((hash `on` v2BS) `on2` unMK) unSS
  where
    digest2V = U.fromList . BA.unpack
    hash = CMH.hmac :: B.ByteString -> B.ByteString -> CMH.HMAC CHA.SHA256


mpwMAC :: MasterKey -> SiteSeed -> TemplateSeed
mpwMAC = hmacSHA256
