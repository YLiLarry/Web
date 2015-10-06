{-# LANGUAGE RankNTypes #-}

module Route.Problem where

import Happstack.Server
import DB
import DB.User as U
import DB.Problem as DB
import Route.Internal
import Control.Monad.Trans
import Helper

problems :: ConnServer [Problem]
problems = do
    pag <- getPagination
    getProblems pag

problem :: ID -> ConnServer Problem
problem = getProblemByID

problemsWithUserSolution :: User -> ConnServer [Problem]
problemsWithUserSolution usr = do
    pag <- getPagination
    getProblemsWithUserSolution pag usr

problemWithUserSolution :: ID -> User -> ConnServer Problem
problemWithUserSolution pid usr = getProblemWithUserSolution pid usr

