{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

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
    safeConvert v                 = error $ show v

instance Convertible Value SqlValue where
    safeConvert (String v) = Right $ toSql v
    safeConvert (Number v) = Right $  
        case floatingOrInteger v of
            Left float -> toSql $ (float :: Double)
            Right int  -> toSql $ (int :: Integer)
    safeConvert (Bool v)   = Right $ toSql v
    safeConvert Null       = Right $ SqlNull
    safeConvert v                 = error $ show v

--class CastFrom c t where
--    castFrom :: t -> c

--class CastTo c t where
--    castTo :: c -> t
    
--    castTo_ :: (p -> t) -> c -> t
--    castTo_ _ = castTo

--class Thru t where
--    put :: t -> (a -> t)

--class (CastTo a t, CastFrom b t) => SafeCast a b t where
--    safeCast :: (p -> t) -> a -> b
--    safeCast f a = castFrom $ castTo_ f a
    
--instance CastFrom Value Int where
--    castFrom a = let (Success x) = fromJSON a in x
--instance CastTo Int Value where
--    castTo   = toJSON
    
--instance CastFrom SqlValue Int where
--    castFrom = fromSql
--instance CastTo Int SqlValue where
--    castTo   = toSql
    
--main = do
--    let sql = toSql (1 :: Int)
--    print $ (castTo (castFrom sql :: Int) :: Value)
    
-- | Cast database return type to haskell types that can be used as object properties
--class (CastFrom d1 a, CastFrom d2 a) => SafeCast d1 d2 a where
    
--    fromD2 :: d2 -> a
--    fromD2 = castFrom
    
--    toD1 :: a -> d1
--    toD1 = castTo

--    castFrom :: d2 -> d1
--    --castFrom = toD1 . fromD2
    
--    fromD1 :: d1 -> a
--    fromD1 = castFrom
    
--    toD2 :: a -> d2
--    toD2 = castTo

--    castTo :: d1 -> d2
--    --castTo = toD2 . fromD1 

    
