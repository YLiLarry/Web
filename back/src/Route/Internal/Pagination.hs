{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Route.Internal.Pagination where
    
import Route.Internal.Serialize.Class
import Route.Internal.Class
import Happstack.Server
import DB.Internal
import Control.Monad.Writer
import DB

getPagination :: ConnServer Pagination
getPagination = do
    c <- lookRead "current"
    p <- lookRead "perpage"
    return Pagination { current = Just c, perPage = Just p, total = Nothing }
