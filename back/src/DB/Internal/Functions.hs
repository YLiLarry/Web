{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module DB.Internal.Functions where
    
import DB.Internal.Class
import DB.Internal.Query
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Convertible
import Data.List (intercalate)
import Control.Monad (liftM)
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except


connMaybeToEither :: String -> ConnMaybe v -> ConnEither v
connMaybeToEither msg mb =
    let f conn = do
            errorIfNothing msg $ runReaderT mb conn
    in ReaderT f

errorIfNothing :: Monad m => String -> MaybeT m v -> ExceptT String m v
errorIfNothing msg maybeT = do
    let mMaybe  = runMaybeT maybeT
    let mEither = liftM (maybeToEither msg) mMaybe
    ExceptT mEither
        
maybeToEither :: String -> Maybe v -> Either String v
maybeToEither msg maybeVal =
    case maybeVal of
        Nothing -> Left msg
        Just vl -> Right vl   

newPagination :: Int -> Int -> Pagination
newPagination c p = Pagination c 0 p

dbLocation :: FilePath
dbLocation = "../db.sqlite"

-- | Run the statement and fetch a column, if the column doesn't exist, then returns Nothing
column :: (Convertible SqlValue a) => String -> Statement -> MaybeIO a
column colName = MaybeT . (fmap (maybe Nothing fromSql . lookup colName =<<)) . fetchRowAL

-- | Run the statement and fetch columns as associated list, if the column doesn't exist, then returns Nothing
columns :: Statement -> MaybeIO [(ColumnName, String)]
columns stmt = do
    ls <- MaybeT $ fetchRowAL stmt
    return $ map (\(colName, sqlVal) -> (colName, fromSql sqlVal)) ls
    
columnsALL :: Statement -> IO [[(ColumnName, String)]]
columnsALL stmt = do
    ls <- fetchAllRowsAL stmt
    return $ (map.map) (\(colName, sqlVal) -> (colName, fromSql sqlVal)) ls
 
get :: Query -> Conn [[(ColumnName, String)]]
get query = 
    let f conn = do
            stmt <- lift $ prepare conn $ getQuery query
            lift $ execute stmt $ getSQLVals query
            column cl stmt 
    in ReaderT f

getBy :: (Convertible SqlValue b) => Query -> TableName -> ColumnName -> ConnMaybe b
getBy query tb cl = 
    let f conn = do
            stmt <- lift $ prepare conn $ "SELECT " ++ cl ++ " FROM " ++ tb ++ " WHERE " ++ getQuery query
            lift $ execute stmt $ getSQLVals query
            column cl stmt 
    in ReaderT f
    
getByID :: (Convertible SqlValue b) => Integer -> TableName -> ColumnName -> ConnMaybe b
getByID id = getBy Query "id =", id)    

getIDBy :: Query -> TableName -> ConnMaybe ID
getIDBy q table = getBy q table "id"
    
connectDB :: IO Connection
connectDB = connectSqlite3 dbLocation

getUserIDBy :: (Convertible a SqlValue) => (ColumnName, a) -> ConnMaybe ID
getUserIDBy q = getIDBy q "User"

getAllPaginated :: TableName -> [ColumnName] -> Pagination -> Conn ([[(ColumnName, String)]], Pagination)
getAllPaginated tb cols pag = 
    let f conn = do
            let offset = show $ ((current pag) - 1) * (perPage pag)
            let top    = show $ perPage pag
            stmt <- prepare conn $ "SELECT " ++ intercalate "," cols ++ " FROM " ++ tb ++ " LIMIT " ++ offset ++ ", " ++ top
            execute stmt []
            result <- fetchAllRowsAL stmt
            count <- quickQuery conn ("SELECT COUNT(*) FROM " ++ tb) []
            let ls = (map.map) (\(colName, sqlVal) -> (colName, fromSql sqlVal)) $ result
            return (ls, pag {total = fromSql $ head $ head count})
    in ReaderT f
    
getAll :: TableName -> [ColumnName] -> Conn [[(ColumnName, String)]]
getAll tb cols = 
    let f conn = do
            stmt <- prepare conn $ "SELECT " ++ intercalate "," cols ++ " FROM " ++ tb
            execute stmt []
            result <- fetchAllRowsAL stmt
            return $ (map.map) (\(colName, sqlVal) -> (colName, fromSql sqlVal)) result
    in ReaderT f

getAllBy :: Query -> TableName -> [ColumnName] -> Conn [[(ColumnName, String)]]
getAllBy query tb cols =
    let f conn = do
            stmt <- prepare conn $ "SELECT " ++ intercalate "," cols ++ " FROM " ++ tb ++ " WHERE " ++ getQuery query
            execute stmt $ getSQLVals query
            columnsALL stmt
    in ReaderT f
    
getOne :: ID -> TableName -> [ColumnName] -> ConnMaybe [(ColumnName, String)]
getOne id tb cols = 
    let f conn = do
            stmt <- lift $ prepare conn $ "SELECT " ++ intercalate "," cols ++ " FROM " ++ tb ++ " WHERE id = ?"
            lift $ execute stmt [toSql id]
            columns stmt
    in ReaderT f
    
getOneBy :: (Convertible a SqlValue) => (ColumnName, a) -> TableName -> [ColumnName] -> ConnMaybe [(ColumnName, String)]
getOneBy (qry, qrv) tb cols = 
    let f conn = do
            stmt <- lift $ prepare conn $ "SELECT " ++ intercalate "," cols ++ " FROM " ++ tb ++ " WHERE " ++ qry ++ " ?"
            lift $ execute stmt [toSql qrv]
            columns stmt
    in ReaderT f
    
newEither :: TableName -> [ColumnName] -> [SqlValue] -> ConnEither ID
newEither tb cols vals = 
    let f conn = do
            ExceptT $ handleSql (return . Left . seErrorMsg) (fmap Right $ runReaderT (new tb cols vals) conn)
    in ReaderT f
    
new :: TableName -> [ColumnName] -> [SqlValue] -> Conn ID
new tb cols vals =
    let f conn = do
            stmt <- prepare conn $ "INSERT INTO " ++ tb ++ " (" ++ intercalate "," cols ++ ") VALUES (" ++ intercalate "," (map (\_ -> "?") cols) ++ ")"
            execute stmt vals
            commit conn
            fmap (fromSql.head.head) $ quickQuery conn "SELECT last_insert_rowid();" []
    in ReaderT f
    
replace :: TableName -> [ColumnName] -> [SqlValue] -> Conn ID
replace tb cols vals =
    let f conn = do
            stmt <- prepare conn $ "REPLACE INTO " ++ tb ++ " (" ++ intercalate "," cols ++ ") VALUES (" ++ intercalate "," (map (\_ -> "?") cols) ++ ")"
            execute stmt vals
            commit conn
            fmap (fromSql.head.head) $ quickQuery conn "SELECT last_insert_rowid();" []
    in ReaderT f
    
nextID :: ID -> TableName -> ConnMaybe ID
nextID id tb = 
    let f conn = do 
            stmt <- lift $ prepare conn $ "SELECT id FROM " ++ tb ++ " WHERE id > ?"
            lift $ execute stmt [toSql id]
            result <- MaybeT $ fetchRow stmt
            return $ fromSql $ head result    
    in ReaderT f
    
prevID :: ID -> TableName -> ConnMaybe ID
prevID id tb = 
    let f conn = do 
            stmt <- lift $ prepare conn $ "SELECT id FROM " ++ tb ++ " WHERE id < ? ORDER BY id DESC LIMIT 1"
            lift $ execute stmt [toSql id]
            result <- MaybeT $ fetchRow stmt
            return $ fromSql $ head result
    in ReaderT f

hasPair :: TableName -> (ColumnName, ColumnName) -> (ID, ID) -> Conn Bool
hasPair tb (cl1, cl2) (id1, id2) = 
    let f conn = do
            stmt <- prepare conn $ "SELECT id FROM " ++ tb ++ " WHERE " ++ cl1 ++ " = ? AND " ++ cl2 ++ " = ?"
            execute stmt [toSql id1, toSql id2]
            result <- fetchRow stmt
            return $ maybe False (const True) result
    in ReaderT f
    
    
