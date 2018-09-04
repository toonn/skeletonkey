module MPW.Test where

import Test.Tasty
import Test.Tasty.HUnit

import Text.XML.Light
import Data.Maybe (mapMaybe, catMaybes)
import Data.Map as M (empty, insertWith, lookup)
import Data.List (find, intercalate)
import qualified Data.ByteString as B (ByteString, append, length)
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base16 (encode, decode)
import Crypto.Hash (hash, Digest, SHA256)
import Crypto.KDF.Scrypt as CKS
import Data.ByteArray (convert)
import Data.Char (toUpper)

import MPW.Path (mpwTestsXml)

import Types
import MPAlgorithm
import KDF

class Understandable a where
  understand :: String -> Maybe a

instance Understandable Algorithm where
  understand "0" = Just V0
  understand "1" = Just V1
  understand "2" = Just V2
  understand "3" = Just V3
  understand _    = Nothing

instance Understandable UserName where
  understand = Just . UName . mkVec

instance Understandable MasterPassword where
  understand = Just . MPass . mkVec

instance Understandable KeyID where
  understand = Just . KeyHash . pack

instance Understandable SiteName where
  understand = Just . SName . mkVec

instance Understandable Integer where
  understand = Just . read

instance Understandable ResultType where
  understand "Basic"   = Just Basic
  understand "Short"   = Just Short
  understand "Medium"  = Just Medium
  understand "Long"    = Just Long
  understand "Maximum" = Just Maximum
  understand "PIN"     = Just PIN
  understand "Name"    = Just Name
  understand "Phrase"  = Just Phrase
  understand _         = Nothing

instance Understandable KeyPurpose where
  understand "Authentication" = Just Authentication
  understand "Identification" = Just Identification
  understand "Recovery"       = Just Recovery
  understand _                = Nothing

instance Understandable SitePassword where
  understand = Just . SPass . mkVec

data Algorithm = V0 | V1 | V2 | V3

newtype KeyID = KeyHash B.ByteString deriving (Eq, Show)

data ResultType = Basic | Short | Medium | Long | Maximum | PIN | Name | Phrase
  deriving (Show)

data KeyPurpose = Authentication | Identification | Recovery
  deriving (Show)

data Case = Case { test_id        :: String
                 , algorithm      :: Algorithm
                 , fullName       :: UserName
                 , masterPassword :: MasterPassword
                 , keyID          :: KeyID
                 , siteName       :: SiteName
                 , siteCounter    :: Integer
                 , resultType     :: ResultType
                 , keyPurpose     :: KeyPurpose
                 , result         :: SitePassword
                 }

data IncompleteCase = IncompleteCase
  { mb_test_id        :: Maybe String
  , mb_parent         :: Maybe String
  , mb_algorithm      :: Maybe Algorithm
  , mb_fullName       :: Maybe UserName
  , mb_masterPassword :: Maybe MasterPassword
  , mb_keyID          :: Maybe KeyID
  , mb_siteName       :: Maybe SiteName
  , mb_siteCounter    :: Maybe Integer
  , mb_resultType     :: Maybe ResultType
  , mb_keyPurpose     :: Maybe KeyPurpose
  , mb_result         :: Maybe SitePassword
  }

testsX          = unqual "tests"
caseX           = unqual "case"
idX             = unqual "id"
parentX         = unqual "parent"
algorithmX      = unqual "algorithm"
fullNameX       = unqual "fullName"
masterPasswordX = unqual "masterPassword"
keyIDX          = unqual "keyID"
siteNameX       = unqual "siteName"
siteCounterX    = unqual "siteCounter"
resultTypeX     = unqual "resultType"
keyPurposeX     = unqual "keyPurpose"
resultX         = unqual "result"

mpwTests :: IO [TestTree]
mpwTests = map mkTest . catMaybes . (map =<< complete) . map caseify . cases
           . parseXML <$> mpw_tests_xml
  where
    mpw_tests_xml :: IO String
    mpw_tests_xml = readFile mpwTestsXml

    cases :: [Content] -> [Element]
    cases = concatMap (findElements caseX)
            . mapMaybe (findElement testsX)
            . onlyElems

    caseify :: Element -> IncompleteCase
    caseify x = IncompleteCase
      { mb_test_id        = findAttr idX x
      , mb_parent         = findAttr parentX x
      , mb_algorithm      = strContent <$> findElement algorithmX x
                            >>= understand
      , mb_fullName       = strContent <$> findElement fullNameX x
                            >>= understand
      , mb_masterPassword = strContent <$> findElement masterPasswordX x
                            >>= understand
      , mb_keyID          = strContent <$> findElement keyIDX x
                            >>= understand
      , mb_siteName       = strContent <$> findElement siteNameX x
                            >>= understand
      , mb_siteCounter    = strContent <$> findElement siteCounterX x
                            >>= understand
      , mb_resultType     = strContent <$> findElement resultTypeX x
                            >>= understand
      , mb_keyPurpose     = strContent <$> findElement keyPurposeX x
                            >>= understand
      , mb_result         = strContent <$> findElement resultX x
                            >>= understand
      }

    inherit :: [IncompleteCase]
            -> IncompleteCase
            -> (IncompleteCase -> Maybe a)
            -> Maybe a
    inherit iCs x proj =
      case proj x of
        Just e -> Just e
        Nothing -> case mb_parent x of
                     Just p -> find ((== Just p) . mb_test_id) iCs
                               >>= (\a -> inherit iCs a proj)
                     Nothing -> Nothing

    complete :: [IncompleteCase] -> IncompleteCase -> Maybe Case
    complete iCs x = do
      let inherit' = inherit iCs x
      test_id        <- mb_test_id x
      algorithm      <- inherit' mb_algorithm
      fullName       <- inherit' mb_fullName
      masterPassword <- inherit' mb_masterPassword
      keyID          <- inherit' mb_keyID
      siteName       <- inherit' mb_siteName
      siteCounter    <- inherit' mb_siteCounter
      resultType     <- inherit' mb_resultType
      keyPurpose     <- inherit' mb_keyPurpose
      result         <- inherit' mb_result
      return $ Case test_id
               algorithm
               fullName
               masterPassword
               keyID
               siteName
               siteCounter
               resultType
               keyPurpose
               result

    mkTest :: Case -> TestTree
    mkTest (Case t_id al fN mP kID sN sC rT kP res) =
      testCaseSteps t_id $ \step -> do
        step "Verifying Master Key"
        let mK = masterKey mP fN
        kID @=? (KeyHash . pack . map toUpper . unpack . encode . convert
                . (hash :: B.ByteString -> Digest SHA256) . v2BS . unMK) mK
        step $ unwords ["Verifying Site Password:"
                       , "purpose", show kP, ", type", show rT]
        res @=? sitePassword mK sN

beTests :: [TestTree]
beTests = [ testCaseSteps "Big Endian representation" $ \step -> do
              step "42"
              mkVec "\0\0\0*" @=? bigEndian 42
              step "31337"
              mkVec "\0\0zi" @=? bigEndian 31337
              step "0x6f3c2d3c"
              mkVec "o<-<" @=? bigEndian 0x6f3c2d3c
          ]

kdfTests :: [TestTree]
kdfTests = [ testCase "Scrypt test vector 1" $
               canonize
                 "77 d6 57 62 38 65 7b 20 3b 19 ca 42 c1 8a 04 97\
                 \f1 6b 48 44 e3 07 4a e8 df df fa 3f ed e2 14 42\
                 \fc d0 06 9d ed 09 48 f8 32 6a 75 3a 0f c8 1f 17\
                 \e8 d3 e0 fb 2e 0d 36 28 cf 35 e2 0c 38 d1 89 06"
               @=? hexScrypt hashOpts1 "" ""
           , testCase "Scrypt test vector 2" $
               canonize
                 "fd ba be 1c 9d 34 72 00 78 56 e7 19 0d 01 e9 fe\
                 \7c 6a d7 cb c8 23 78 30 e7 73 76 63 4b 37 31 62\
                 \2e af 30 d9 2e 22 a3 88 6f f1 09 27 9d 98 30 da\
                 \c7 27 af b9 4a 83 ee 6d 83 60 cb df a2 cc 06 40"
               @=? hexScrypt hashOpts2 "password" "NaCl"
           , testCase "Scrypt test vector 3" $
               canonize
                 "70 23 bd cb 3a fd 73 48 46 1c 06 cd 81 fd 38 eb\
                 \fd a8 fb ba 90 4f 8e 3e a9 b5 43 f6 54 5d a1 f2\
                 \d5 43 29 55 61 3f 0f cf 62 d4 97 05 24 2a 9a f9\
                 \e6 1e 85 dc 0d 65 1e 40 df cf 01 7b 45 57 58 87"
               @=? hexScrypt hashOpts3 "pleaseletmein" "SodiumChloride"
           , testCase "Scrypt test vector 4" $
               canonize
                 "21 01 cb 9b 6a 51 1a ae ad db be 09 cf 70 f8 81\
                 \ec 56 8d 57 4a 2f fd 4d ab e5 ee 98 20 ad aa 47\
                 \8e 56 fd 8f 4b a5 d0 9f fa 1c 6d 92 7c 40 f4 c3\
                 \37 30 40 49 e8 a9 52 fb cb f4 5c 6f a7 7a 41 a4"
               @=? hexScrypt hashOpts4 "pleaseletmein" "SodiumChloride"
           ] where
             canonize = filter (/= ' ')
             hexSHA256 = encode . v2BS . unMK
             hexScrypt hOpts pass salt = unpack . hexSHA256 $ scrypt
               hOpts
               (MPass $ mkVec pass)
               (USeed $ mkVec salt)
             hashOpts1 = CKS.Parameters { CKS.n            = 16
                                        , CKS.r            = 1
                                        , CKS.p            = 1
                                        , CKS.outputLength = 64
                                        }
             hashOpts2 = CKS.Parameters { CKS.n            = 1024
                                        , CKS.r            = 8
                                        , CKS.p            = 16
                                        , CKS.outputLength = 64
                                        }
             hashOpts3 = CKS.Parameters { CKS.n            = 16384
                                        , CKS.r            = 8
                                        , CKS.p            = 1
                                        , CKS.outputLength = 64
                                        }
             hashOpts4 = CKS.Parameters { CKS.n            = 1048576
                                        , CKS.r            = 8
                                        , CKS.p            = 1
                                        , CKS.outputLength = 64
                                        }


properties :: TestTree
properties = testGroup "Properties" []

unitTests :: IO TestTree
unitTests = testGroup "Unit tests"
  . ((beTests ++ take 1 kdfTests) ++) . take 1 <$> mpwTests

tests :: IO TestTree
tests = do
  uTs <- unitTests
  return $ testGroup "MPW" [ properties, uTs ]
