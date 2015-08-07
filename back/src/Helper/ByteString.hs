module Helper.ByteString (
          toHex
        , fromHex
    ) where
    
import Data.ByteString as BS (ByteString, unpack, pack)
import Numeric (showHex, readHex)
import Data.Word (Word8)

toHex :: ByteString -> String
toHex = (concatMap $ flip showHex "") . unpack

fromHex :: String -> ByteString
fromHex = pack . (groupEvery2With (fst.head.readHex))
    where
        groupEvery2With :: (String -> Word8) -> String -> [Word8]
        groupEvery2With _ [] = []
        groupEvery2With f ls = (f $ take 2 ls) : (groupEvery2With f $ drop 2 ls)
    
