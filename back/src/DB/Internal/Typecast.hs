{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Internal.Typecast where
import Data.Aeson
import Database.HDBC
import Data.Convertible

import Data.Scientific as S (scientific, floatingOrInteger)
import Data.Text as T (pack, unpack)
import Data.ByteString.Char8 as B (pack, unpack)

instance Convertible SqlValue Value where
    safeConvert (SqlString v)     = Right $ toJSON v
    safeConvert (SqlInteger v)    = Right $ toJSON v
    safeConvert (SqlByteString v) = Right $ toJSON $ B.unpack v
    safeConvert (SqlBool v)       = Right $ toJSON v
    safeConvert (SqlInt64 v)      = Right $ toJSON v
    safeConvert v                 = error $ "Typecast.hs: " ++ show v

instance Convertible Value SqlValue where
    safeConvert (String v) = Right $ toSql v
    safeConvert (Number v) = Right $  
        case floatingOrInteger v of
            Left float -> toSql $ (float :: Double)
            Right int  -> toSql $ (int :: Integer)
    safeConvert (Bool v)   = Right $ toSql v
    safeConvert Null       = Right $ SqlNull
    safeConvert v          = error $ "Typecast.hs: " ++ show v
