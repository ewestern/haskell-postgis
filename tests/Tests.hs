{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Database.Postgis
import Database.Postgis.Serialize
import Data.Char
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Vector as V
import System.Endian

point1 = GeoPoint (Just 4326) (Point (-79.4217280000002) 42.289467099999925 Nothing Nothing)
point1BS ="0101000020E6100000BCF36F97FDDA53C042E207420D254540"  :: BL.ByteString
linestring1BS = "0102000020E610000005000000805C4A99F98B5DC0BC5768BDDB0E4140805C9A58F98B5DC0C05798F5DC0E4140805C6C2DF88B5DC0C457C846E30E41407E5C3E5CF78B5DC0C857D0B2E70E41407E5CAA69F68B5DC0CC57B813EA0E4140" :: BL.ByteString
linestring1 = GeoLineString (Just 4326) (LineString (V.fromList [(Point (-118.18710930120324) 34.11608092875346 Nothing Nothing), (Point (-118.1870938785014) 34.11611814440357 Nothing Nothing), (Point (-118.18702254850541) 34.116310928176546 Nothing Nothing), (Point (-118.18697267618151) 34.116445876817636 Nothing Nothing), (Point (-118.18691484104963)  34.11651846409913 Nothing Nothing)]))

toUpperBS :: BL.ByteString -> BL.ByteString
toUpperBS = BLC.map toUpper 

main :: IO ()
main = hspec $ do
  describe "Hexable" $ do
    let be = "00" :: BL.ByteString
        le = "01" :: BL.ByteString
    it "should convert Endiannes to ByteString" $ do
      toHex BigEndian `shouldBe` be 
      toHex LittleEndian `shouldBe` le 
    it "should convert ByteString to Endianness" $ do
      fromHex be `shouldBe` BigEndian
      fromHex le `shouldBe` LittleEndian
    {-it "should convert int32 to a hex ByteString" $ do-}
      
  describe "parseGeometry" $ do
    it "Should correctly parse a bytestring into a Point" $ do
        readGeometry point1BS `shouldBe` point1
    it "Should parse a bytestring into a LineString" $ do
        readGeometry linestring1BS `shouldBe` linestring1
  describe "writeGeometry" $ do
    it "Should correctly write a Point into a bytestring" $ do
        (toUpperBS $ writeGeometry point1) `shouldBe` point1BS
    it "Should write a LineString into a bytestring" $ do
        (toUpperBS $ writeGeometry linestring1) `shouldBe` linestring1BS 


