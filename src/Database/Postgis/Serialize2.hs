{-# LANGUAGE TypeFamilies, GADTs, FlexibleInstances, OverloadedStrings #-}
import Database.Postgis.Geometry
import Database.Postgis.Utils

import Control.Monad.Reader
{-import qualified Data.Attoparsec.ByteString as P-}
import qualified Data.ByteString.Builder as B
{-import Data.Attoparsec.ByteString (Parser)-}
import qualified Data.ByteString as BS
import Control.Applicative ((<*>), (<$>))
import Control.Monad.Writer.Lazy
import Data.Binary.Get


import Data.Binary.IEEE754
import Data.Word
import System.Endian

{-getGeometry :: BS.ByteString -> Geometry-}
{-getGeometry bs = case P.parse parseGeometry bs of-}
    {-P.Fail t ss  s -> error s-}
    {-P.Partial f -> undefined-}
    {-P.Done r g -> g-}

type EndParser = ReaderT Header Parser 
type Parser = Get
type Putter = Writer BS.ByteString 

class Serialize a where
  type Getter a
  put :: a -> B.Builder 
  get :: Getter a 

instance Serialize Geometry where
  type Getter Geometry = Parser Geometry
  put = writeGeometry
  get = parseGeometry 

class Hexable a where
  {-toHex :: a -> B.Builder-}
  toHex :: a -> BS.ByteString
  fromHex :: BS.ByteString -> a

instance Hexable Int where
  toHex = toHexWord32 . fromIntegral
  fromHex = fromHexInt

instance Hexable Word8 where
  toHex = toHexInt8  . fromIntegral
  fromHex = fromHexInt

instance Hexable Word32  where
  toHex =  toHexWord32
  fromHex = fromHexInt

instance Hexable Double where
  fromHex = wordToDouble . fromHexInt 
  toHex = toHexDouble 

data Header = Header {
    _byteOrder :: Endianness
  , _geoType :: Word32
  , _srid :: SRID
} deriving (Show)

{-instance Serialize Header where-}
  {-type Getter Header = EndParser -}
  {-put =  -}


-- writers
writeGeometry = undefined


{-writeNum :: (Hexable a, Num a) => a -> B.Builder-}
{-writeNum = putByteString . toHex-}

{-writeMaybeNum :: (Num a, Hexable a) => (Maybe a) -> B.Builder -}
{-writeMaybeNum (Just n)  = writeNum n -}
{-writeMaybeNum Nothing = return () -}



--
-- getters 
--
parseGeometry :: Parser Geometry 
parseGeometry = undefined 
  

{-parserHeader :: Parser Header-}
{-parserHeader = -}

-- number parsers



parseNumber :: (Hexable a , Num a) => Int -> Endianness -> Parser a
parseNumber l end = do
  bs <- getByteString l
  case end of 
    BigEndian -> return $ fromHex bs
    LittleEndian -> return . fromHex . convertLittleEndian $ bs 

parseInt' :: Endianness -> Parser Int 
-- word32 = 4 bytes * 2 nibbles
parseInt' = parseNumber 8

parseInt :: EndParser Int 
parseInt = (parseInt' <$> (asks _byteOrder)) >>= lift

parseDouble' :: Endianness -> Parser Double 
-- word64 =  8 bytes * 2 nibbles
parseDouble' = parseNumber 16

parseDouble :: EndParser Double
parseDouble = (parseDouble' <$> (asks _byteOrder)) >>= lift


