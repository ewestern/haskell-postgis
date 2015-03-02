{-# LANGUAGE OverloadedStrings #-}

module Database.Postgis.Simple.Serialize  (

) where
import Data.Serialize.Put
{-import Database.Postgis.Simple.Types-}
import qualified Data.ByteString as BS
import Data.ByteString.Lex.Integral
import Data.Bits
import qualified Data.Vector as V
import Data.Text.Read
import Control.Applicative
import Data.Text.Encoding
{-import Data.Binary.IEEE754-}

{-parseHeader :: Get Header-}
{-parseHeader = do-}
	{-or <- parseEndian	-}
	{-t <- parseInt or-}
	{-srid <- if t .&. wkbSRID > 0 then Just <$> parseInt or else return Nothing-}
	{-return $ Header or t srid-}


{-writeEndian :: Putter Endian-}
{-writeEndian BigEndian = putByteString $ -}
{-writeEndian LittenEndian = putB-}


{-parseHex :: Integral a => BS.ByteString -> a-}
{-parseHex bs = case hexadecimal . decodeUtf8 $ bs of-}
      {-Right (v, r) ->  v-}
      {-Left s -> error s -}

{-writeHeader :: Putter Header-}
{-writeHeader head = do-}
  {-writeEndian $ _byteOrder head-}
