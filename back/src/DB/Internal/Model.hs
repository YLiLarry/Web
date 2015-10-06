{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DB.Internal.Model where
    
import DB.Internal.Class
import DB.Internal.Query as Query
import Data.Aeson as A (ToJSON(..), Result(..), Value(..), FromJSON(..), fromJSON, object, (.=), decode)
import Data.Aeson.Encode (encodeToTextBuilder)
import qualified Data.Map as M (Map(..), fromList, unionWith, lookup)
import qualified Data.HashMap.Strict as HM (map, toList)
import Control.Monad (foldM)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, mapWriterT, tell)
import Control.Monad.Except (throwError)
import Data.Functor.Identity (Identity)
import Database.HDBC (SqlValue)
import Data.Maybe (fromJust)
import GHC.Generics (Generic(..))
import Data.Typeable (Typeable(..), typeOf)
import Helper
import Control.Applicative (Applicative(..), liftA2)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.Lazy as TL (unpack)
import qualified Data.Text.Lazy.Builder as TL (toLazyText)
import Data.Convertible (convert)
import DB.Internal.PropertyMethodMap
import DB.Internal.PropertyMethod
import DB.Internal.Typecast
import Data.Monoid (Monoid (..))
import Control.Monad.Trans.Elevator (mapElevate)
import Data.ByteString.Lazy.Char8 as BS (pack)

fromResult :: Result a -> a
fromResult (Success a) = a
fromResult (Error a) = error $ "Model.hs: " ++ a

class FromModel a where
    fromModel :: (Model b) => b -> a
    fromModel = fromValue . toJSON
    
    fromValue :: Value -> a

instance FromModel (M.Map PropertyName SqlValue) where
    fromValue (Object hmap) = 
        M.fromList $ 
        map (\(k, v) -> (T.unpack k, v)) $
        HM.toList $ 
        HM.map convert hmap
            
instance FromModel (M.Map PropertyName Value) where
    fromValue = fromResult . fromJSON 
    
instance ToModel [SqlValue] where
    toValue x = toJSON $ (map convert x :: [Value])
        
-- instance FromModel String where
    -- fromValue = TL.unpack . TL.toLazyText . encodeToTextBuilder

-- instance FromModel Int where
    -- fromValue = head . fromResult . fromJSON

-- instance FromModel Value where
    -- fromValue = id

class ToModel a where
    toValue :: a -> Value
    
    toModel :: (Model b) => a -> b
    toModel = fromResult . fromJSON . toValue
    
instance ToModel [(PropertyName, SqlValue)] where
    toValue al = object $ [ (T.pack x) .= toJSON (convert y :: Value) | (x,y) <- al ]
    
instance ToModel (M.Map PropertyName Value) where
    toValue = toJSON
    
instance ToModel String where
    toValue = fromJust . A.decode . BS.pack
    
class (FromJSON a, ToJSON a, Show a, Typeable a) => Model a where
    
    decroGet :: (FromConnEither m) => GetMethods a -> a -> m a
    decroGet b o = fromConnEither $ do
        e <- getOne b
        return $ extend o e
        
    decroGetS :: (FromConnEither m) => [a -> GetMethods a] -> a -> m a
    decroGetS fs o = foldM (\acc f -> decroGet (f acc) acc) o fs
        
    decroEach :: (FromConnEither m) => (a -> ConnEither a) -> ConnEither [a] -> m [a]
    decroEach f lsM = fromConnEither $ do
        ls <- lsM 
        mapM f ls 
        
    decroEachGet :: (FromConnEither m) => (a -> GetMethods a) -> ConnEither [a] -> m [a]
    decroEachGet f m = fromConnEither $ do
        objs <- m 
        decroMapGet f objs
    
    decroMapGet :: (FromConnEither m) => (a -> GetMethods a) -> [a] -> m [a]
    decroMapGet f = mapM (\x -> decroGet (f x) x)
    
    decroMapGetS :: (FromConnEither m) => [(a -> GetMethods a)] -> [a] -> m [a]
    decroMapGetS fs os = sequence [ decroGetS fs o | o <- os ]
    
    extend :: a -> a -> a
    extend a b = toModel $ (u :: M.Map PropertyName Value)
        where
            u = M.unionWith nonNull (fromModel a) (fromModel b) 
            nonNull x Null = x
            nonNull Null y = y
            nonNull x y    = y
    
    save :: (SaveResult r, FromConnEither m) => a -> SaveMethods a -> m r
    save obj ls = fromConnEither $ do
        v <- return $ fromModel obj
        xs <- mapM (f v) $ getSaveMethods ls
        return $ fromList $ concat xs
        where
            f :: M.Map PropertyName SqlValue -> SaveMethod -> ConnEither [(PropertyName, ID)]
            f al sm = do
                let (properties, method) = getSaveMethod sm
                id <- runSaveQuery method [ fromJust $ M.lookup x al | x <- properties ]
                return [ (x, id) | x <- properties ]

    get :: (FromConnEither m) => GetMethods a -> m [a]
    get ls = fromConnEither $ do 
        allAL <- foldM f (Right $ repeat []) $ getGetMethods ls
        case allAL of 
            Left  ls -> return $ map toModel ls
            Right ls -> return $ map toModel ls
        
        where
            f :: Either [[SqlValue]] [[(PropertyName, SqlValue)]] -> GetMethod -> ConnEither (Either [[SqlValue]] [[(PropertyName, SqlValue)]])
            f (Right acc) gm = do
                let (properties, method, sqlVs) = getGetMethod gm
                vals <- runGetQuery method sqlVs
                if null properties then return $ Left $ zipWith (++) (dropFirstField acc) vals 
                else 
                    case vals of
                        []   -> return $ Right $  acc
                        vals -> return $ Right $ zipWith (++) acc [ zip properties x | x <- vals ]
            f (Left acc) gm = do
                let (_, method, sqlVs) = getGetMethod gm
                vals <- runGetQuery method sqlVs
                case vals of
                    []   -> return $ Left $ acc
                    vals -> return $ Left $ zipWith (++) acc vals
                
            dropFirstField :: [[(PropertyName, SqlValue)]] -> [[SqlValue]]
            dropFirstField = (map.map) snd
                
    getOne :: (FromConnEither m) => GetMethods a -> m a
    getOne m = fromConnEither $ do
        objs <- get m
        case objs of 
            [] -> throwError $ "No result found. " ++ show m
            v  -> return $ head objs
    
    getP :: FromResultPagination m => TableName -> Pagination -> GetMethods a -> m [a]
    getP tb pag m = fromResultPagination $ do
        objs <- fromConnEither $ get m
        pagi <- fromConnEither $ getTotal tb pag
        tell $ Meta pagi
        return objs
        
    --delete :: DelMethod l => a -> ConnEither ID

data Meta a = NoMeta | Meta a deriving (Show, Functor)

type ResultPagination = ResultPaginationT IO
type ResultPaginationT m = ConnEitherT (WriterT (Meta Pagination) m)

-- instance MonadIO ResultPagination 

class (Monad t) => FromResultPaginationT m t where
    fromResultPaginationT :: (Show a) => ResultPaginationT m a -> t a

instance FromResultPaginationT IO (ResultPaginationT IO) where
    fromResultPaginationT = id

class (Monad t, FromResultPaginationT IO t) => FromResultPagination t where
    fromResultPagination :: (Show a) => ResultPaginationT IO a -> t a
    fromResultPagination = fromResultPaginationT

instance (FromResultPaginationT IO t) => FromResultPagination t where
    fromResultPagination = fromResultPaginationT

instance {-# OVERLAPS #-} FromConnEitherT IO (ResultPaginationT IO) where
    fromConnEitherT = mapElevate

-- instance {-# OVERLAPS #-} (FromResultPaginationT t m) => FromConnEitherT t m where
    -- fromConnEitherT x = fromResultPaginationT $ (fromConnEitherT x :: ResultPaginationT t m)
     
class SaveResult a where
    fromList :: [(PropertyName, ID)] -> a
    
instance SaveResult (M.Map PropertyName ID) where
    fromList = M.fromList 

instance SaveResult ID where
    fromList = snd . head

instance Model Pagination 
instance Model Int
instance Model a => Model [(String, a)]

getTotal :: TableName -> Pagination -> ConnEither Pagination
getTotal tb pag = do
    pag <- flip decroGet pag $ newGetMethods [ newGetMethod ["total"] (newGetQuery $ "SELECT COUNT(*) FROM " ++ tb) [] ]
    return $ pag { 
        total = do
            t <- total pag
            p <- perPage pag
            return $ t `div` p
    }

-- mapResultPaginationT :: (Monad m, Monad n) => (m (Either String v) -> n (Either String v)) -> ResultPaginationT m a -> ResultPaginationT n a
mapResultPaginationT = mapConnEitherT . mapWriterT

class (Monad m) => ResultPaginationC m where
    newResultPagination :: [a] -> Meta Pagination -> m [a]
    getResultPagination :: m [a] -> m ([a], Meta Pagination)
    
    forR :: m [a] -> (a -> b) -> m [b]
    forR a f = do
        (ls,p) <- getResultPagination a
        newResultPagination (map f ls) p
    
    forP :: m [a] -> (Meta Pagination -> Meta Pagination) -> m [a]
    forP a f = do
        (ls,p) <- getResultPagination a
        newResultPagination ls $ f p
    
    mapR :: (a -> b) -> m [a] -> m [b]
    mapR = flip forR
        
    extR :: (Model a) => m [a] -> a -> m [a]
    extR x y = forR x (\a -> extend a y)
    
    extP :: m [a] -> Meta Pagination -> m [a]
    extP x y = forP x (\a -> mappend a y)
    
    --forRM :: (Monad n) => m [a] -> (a -> n b) -> n (m [b])
    --forRM a f = return $ do
    --    (ls,p) <- getResultPagination a
    --    ls' <- mapM f ls 
    --    newResultPagination ls' p
        
    --mapRM :: (Monad n) => (a -> n b) -> m [a] -> n (m [b])
    --mapRM = flip forRM
        
    --extRM :: (Monad m, Monad n, Model a) => m a -> n (m a) -> n (m a) 
    --extRM x y = do
    --    y' <- y
    --    return $ liftM2 extend x y'
    
-- instance Applicative Meta where
--     pure x  = Meta x
--     (Meta f) <*> (Meta x) = pure $ f x
--     (Meta f) <*> NoMeta   = NoMeta
--     NoMeta   <*> NoMeta   = NoMeta
--     -- NoMeta <*> x   = x
--     -- _ <*> NoMeta   = NoMeta
    
instance (Model x) => Monoid (Meta x) where
    mempty = NoMeta
    mappend (Meta x) (Meta y) = Meta $ extend x y
    mappend (Meta x) NoMeta   = Meta $ x
    mappend NoMeta   (Meta y) = Meta $ y
    mappend NoMeta   NoMeta   = NoMeta

instance ToJSON a => ToJSON (Meta a) where
    toJSON NoMeta = Null
    toJSON (Meta x) = toJSON x
    