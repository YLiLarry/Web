{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module Route.Internal.Class where

import DB hiding (Meta)
import qualified DB as DB (Meta (..))
import Happstack.Server
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Control.Monad.Except
import Data.Functor.Identity
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Elevator
import Control.Monad.Writer

type Meta = DB.Meta Pagination

type ConnServerT m = ConnEitherT (WriterT Meta (ServerPartT m))
type ConnServer = ConnServerT IO

runConnServerT :: ConnServerT m a -> Connection -> ServerPartT m (Either String a, Meta)
runConnServerT x conn = runWriterT $ runConnEitherT x conn

instance {-# OVERLAPS #-} (Elevator m n) => MapElevator m n ServerPartT where
    mapElevate = mapServerPartT elevate

instance (Monad m, Monad n, MapElevator m n t) => MapElevator (t m) (t n) ServerPartT where
    mapElevate = mapServerPartT mapElevate
        
instance FromConnEitherT IO ConnServer where
    fromConnEitherT = mapElevate

instance FromResultPaginationT IO ConnServer where
    fromResultPaginationT = mapElevate
        
type Ready = ConnT (ServerPartT IO) Response

-- instance FromConnEitherT IO (ServerPartT IO) where
    -- fromConnEitherT = 





