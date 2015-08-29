{-# LANGUAGE FlexibleContexts #-}

module DB (
      module Database.HDBC
    , connectDB
    , Pagination(..)
    , ID
    , newPagination
    , DB(..)
    , ConnT
    , ConnEither
    , ConnMaybe
    , IConnection(..)
) where

import DB.Internal (
          connectDB
        , Pagination(..)
        , ID
        , newPagination
        , DB(..)
        , ConnT
        , ConnEither
        , ConnMaybe
        , IConnection(..)
    )
import Database.HDBC

