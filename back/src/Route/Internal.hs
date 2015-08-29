{-# LANGUAGE RankNTypes #-}

module Route.Internal where

import Data.Aeson
import DB(Pagination(..), DB(..), ConnT, ConnEither, IConnection(..))
import Helper
import Happstack.Server
import Happstack.Server.Monads
import Data.Text
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

type ConnServer v = ConnT (ServerPartT IO) v

toJObjs :: DB a => String -> [a] -> String
toJObjs objType ls = encodeToString [(objType, ls)]

toJObj :: DB a => String -> Maybe a -> String
toJObj _       Nothing    = "{}"
toJObj objType (Just obj) = encodeToString [(objType, obj)]

toJObjsPag :: DB a => String -> ([a], Pagination) -> String
toJObjsPag objType (ls,pag) = encodeToString $ 
    [("meta", toJSON [("total", total pag)]), (objType, toJSON ls)]

encodeToString :: ToJSON a => a -> String
encodeToString a =
    case fromJSON $ toJSON a of
        Success val -> val
        Error   msg -> error msg
    
exceptToResponse :: ToMessage a => ExceptT String IO a -> ServerPart Response
exceptToResponse e = do
    val <- lift $ runExceptT e
    case val of
        Left msg -> badRequest $ toResponse msg
        Right vl -> ok $ toResponse vl
        
connExecptToResponse :: (IConnection c, ToMessage a) => ReaderT c (ExceptT String IO) a -> ReaderT c (ServerPartT IO) Response
connExecptToResponse = mapReaderT exceptToResponse

    