module MPWTemplates where

import Types

import Prelude (rem, ($), fromIntegral, String, (.), head, map)
import Data.Word
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Map.Strict as M
import qualified Codec.Binary.UTF8.String as U8 (encodeChar)
import Data.Bifunctor (first)


type Template = U.Vector Word8
type Templates = V.Vector Template

charGroup :: M.Map Word8 Template
charGroup = M.fromAscList $ map (first $ head . U8.encodeChar)
              [ ('A', aA), ('C', cC), ('V', vV), ('a', a)
              , ('c', c), ('n', n), ('o', o), ('v', v)
              ]
  where
    (++) = (U.++)
    cC = mkVec "BCDFGHJKLMNPQRSTVWXYZ"
    c  = mkVec "bcdfghjklmnpqrstvwxyz"
    vV = mkVec "AEIOU"
    v  = mkVec "aeiou"
    aA = cC ++ vV
    a  = vV ++ v ++ cC ++ c
    n  = mkVec "0123456789"
    o  = mkVec "@&%?,=[]_:-+*$#!'^~;()/."
    x  = a ++ n ++ o

mpwSitePassword :: Templates -> TemplateSeed -> SitePassword
mpwSitePassword templates (TSeed templateSeed) =
  SPass $ U.zipWith zipper template (U.tail templateSeed)
  where
    template = V.unsafeIndex templates $
      fromIntegral (U.head templateSeed) `rem` V.length templates
    zipper chrGrp tmpSd = U.unsafeIndex cG $ fromIntegral tmpSd `rem` U.length cG
      where cG = charGroup M.! chrGrp

maximumSecurityPassword :: TemplateSeed -> SitePassword
maximumSecurityPassword = mpwSitePassword templates
  where templates = V.fromList
              [ mkVec "anoxxxxxxxxxxxxxxxxx"
              , mkVec "axxxxxxxxxxxxxxxxxno"
              ]

longPassword :: TemplateSeed -> SitePassword
longPassword = mpwSitePassword templates
  where templates = V.fromList
              [ mkVec "CvcvnoCvcvCvcv"
              , mkVec "CvcvCvcvnoCvcv"
              , mkVec "CvcvCvcvCvcvno"
              , mkVec "CvccnoCvcvCvcv"
              , mkVec "CvccCvcvnoCvcv"
              , mkVec "CvccCvcvCvcvno"
              , mkVec "CvcvnoCvccCvcv"
              , mkVec "CvcvCvccnoCvcv"
              , mkVec "CvcvCvccCvcvno"
              , mkVec "CvcvnoCvcvCvcc"
              , mkVec "CvcvCvcvnoCvcc"
              , mkVec "CvcvCvcvCvccno"
              , mkVec "CvccnoCvccCvcv"
              , mkVec "CvccCvccnoCvcv"
              , mkVec "CvccCvccCvcvno"
              , mkVec "CvcvnoCvccCvcc"
              , mkVec "CvcvCvccnoCvcc"
              , mkVec "CvcvCvccCvccno"
              , mkVec "CvccnoCvcvCvcc"
              , mkVec "CvccCvcvnoCvcc"
              , mkVec "CvccCvcvCvccno"
              ]

mediumPassword :: TemplateSeed -> SitePassword
mediumPassword = mpwSitePassword templates
  where templates = V.fromList
              [ mkVec "CvcnoCvc"
              , mkVec "CvcCvcno"
              ]

shortPassword :: TemplateSeed -> SitePassword
shortPassword = mpwSitePassword templates
  where templates = V.fromList
              [ mkVec "Cvcn"
              ]

basicPassword :: TemplateSeed -> SitePassword
basicPassword = mpwSitePassword templates
  where templates = V.fromList
              [ mkVec "aaanaaan"
              , mkVec "aannaaan"
              , mkVec "aaannaaa"
              ]

pin :: TemplateSeed -> SitePassword
pin = mpwSitePassword templates
  where templates = V.fromList
              [ mkVec "nnnn"
              ]

