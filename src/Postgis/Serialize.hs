module Postgis.Parser  (

) where
import Data.Serialize.Get
import Database.Postgis.Simple.Types
import qualified Data.ByteString as BS
import Data.Bits
import qualified Data.Vector as V
import Data.Text.Read
import Control.Applicative
import Data.Text.Encoding
import Data.Binary.IEEE754

writeHeader :: Header -> Put
writeHeader head = do
