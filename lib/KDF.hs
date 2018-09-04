module KDF where
import Types

import qualified Crypto.Argon2 as Argon2
import qualified Crypto.KDF.Scrypt as CKS

argon2 :: Argon2.HashOptions -> MasterPassword -> UserSeed -> MasterKey
argon2 = (MKey . bs2V . either (const undefined) id) ..... flip (`on2` unMP) unUS . (`on` v2BS) . Argon2.hash

skKDF :: MasterPassword -> UserSeed -> MasterKey
skKDF = argon2 hashOpts
  where
    hashOpts :: Argon2.HashOptions
    hashOpts = Argon2.HashOptions
      { Argon2.hashIterations  = 6
      , Argon2.hashMemory      = 2 ^ 17
      , Argon2.hashParallelism = 4
      , Argon2.hashVariant     = Argon2.Argon2i
      , Argon2.hashVersion     = Argon2.Argon2Version13
      , Argon2.hashLength      = 2 ^ 6
      }

scrypt :: CKS.Parameters -> MasterPassword -> UserSeed -> MasterKey
scrypt = (MKey . bs2V) ..... flip (`on2` unMP) unUS . (`on` v2BS) . CKS.generate

mpwKDF :: MasterPassword -> UserSeed -> MasterKey
mpwKDF = scrypt hashOpts
  where
    hashOpts :: CKS.Parameters
    hashOpts = CKS.Parameters  { CKS.n            = 32768
                               , CKS.r            = 8
                               , CKS.p            = 2
                               , CKS.outputLength = 64
                               }
