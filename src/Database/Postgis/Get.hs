import Data.Serialize.Get
import Control.Monad.Reader
import qualified Data.ByteString as BS

import System.Endian
import Database.Postgis.WKBTypes
{-import Database.Postgis.Parser-}
import Control.Applicative ((<$>))
import Data.Serialize
import Data.Serialize.Get

import Data.Binary.IEEE754
import qualified Data.ByteString as BS
import Data.ByteString.Lex.Integral
import Data.Bits


class Hexable a where
  toHex :: a -> BS.ByteString
  fromHex :: BS.ByteString -> a

instance Hexable Int where
  toHex = toHexInt
  fromHex = fromHexInt

instance Hexable Double where
  fromHex = wordToDouble . fromHexInt 
  toHex = toHexInt . doubleToWord

fromHexInt :: Integral a => BS.ByteString -> a
fromHexInt bs = case readHexadecimal bs of
    Just (v, r) -> v
    Nothing -> error "Cannot parse hexadecimal"

toHexInt :: Integral a => a -> BS.ByteString
toHexInt i = case packHexadecimal i of
    Just bs -> bs
    Nothing -> error "Cannot create bytestring"



{-type Something = ReaderT Header Get-}
type HeadedGet = ReaderT Header Get

parseNum :: (Num a, Hexable a) => HeadedGet a
parseNum = do
  end <- asks byteOrder
  let v = case end of
          BigEndian -> fromHex <$> get
          LittleEndian -> (fromHex . readLittleEndian) <$> get
  lift v
  {-return v-}


{-parseNum BigEndian = fromHex <$> get  -}
{-parseNum LittleEndian = (fromHex . readLittleEndian) <$> get -}

readLittleEndian :: BS.ByteString -> BS.ByteString
readLittleEndian bs = BS.concat . reverse $ splitEvery bs
  where
    splitEvery bs = 
      let (first, rest) = BS.splitAt 2 bs in 
      if BS.null bs then [] else first : (splitEvery rest)
 

{-parseHeaded :: HeadedGet a-}
{-parseHeaded = do-}
  {-bo <- asks byteOrder-}
  {-return  -}
  {-g <- lift -}

{-runSomething :: ReaderT Header Get ()-}
{-runSomething = do-}
  {-content <- ask-}

{-runSomething :: Something a -> Header -> BS.ByteString -> Either String a -}
{-runSomething s h bs = do-}
  {-let g = runReaderT -}
  {-in runGet g bs-}
{-newtype GetT m a = GetT { runGetT :: m (Get a ) }-}


{-instance Monad m => Monad (GetT m) where-}
  {-return = GetT . return . return-}
  {-x >>= f = GetT $ do-}
    {-v <- runGetT-}
