module Types
  ( MasterKey (..)
  , MasterPassword (..)
  , SiteName (..)
  , SitePassword (..)
  , SiteSeed (..)
  , TemplateSeed (..)
  , UserName (..)
  , UserSeed (..)
  , (...)
  , (.....)
  , on
  , on2
  , bs2T
  , bs2V
  , t2BS
  , v2BS
  , mkVec
  ) where

import Data.Word
import Data.Vector.Unboxed
import Data.Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString as B
import Data.Function (on)
import qualified Codec.Binary.UTF8.String as U8

newtype MasterPassword = MPass { unMP :: Vector Word8 }
newtype SiteName       = SName { unSN :: Vector Word8 }
newtype UserName       = UName { unUN :: Vector Word8 }
newtype UserSeed       = USeed { unUS :: Vector Word8 }
newtype MasterKey      = MKey  { unMK :: Vector Word8 }
newtype SiteSeed       = SSeed { unSS :: Vector Word8 }
newtype TemplateSeed   = TSeed { unTS :: Vector Word8 }
newtype SitePassword   = SPass { unSP :: Vector Word8 }

t2BS :: Text -> B.ByteString
t2BS = encodeUtf8

bs2T :: B.ByteString -> Text
bs2T = decodeUtf8

v2BS :: Vector Word8 -> B.ByteString
v2BS = B.pack . toList

bs2V :: B.ByteString -> Vector Word8
bs2V = fromList . B.unpack

mkVec :: String -> Vector Word8
mkVec = fromList . U8.encode

infixr 9 ...
(...) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(...) = fmap . fmap

infixr 9 .....
(.....) :: (Functor f, Functor g, Functor h)
       => (a -> b) -> f (g (h a)) -> f (g (h b))
(.....) = fmap . fmap . fmap

on2 :: (a -> b -> e) -> (c -> a) -> (d -> b) -> c -> d -> e
on2 f g h c d = f (g c) (h d)
