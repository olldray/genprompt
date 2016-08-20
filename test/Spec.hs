{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

import Lib
import GHC.Generics ( Generic)



main :: IO ()
main = putStrLn "no run-time testing yet"


-- V1
data Empty deriving Generic
instance Prompt Empty where

-- U1
data TUnitType = TUnitType
    deriving Generic
instance Prompt TUnitType where

-- K1
data TContainerType = TContainerType Int
    deriving Generic
instance Prompt TContainerType where

-- :+:
data TSumType = TSum1 | TSum2
    deriving Generic
instance Prompt TSumType where

-- :*:
data TMulType = TMulType Int String
    deriving Generic
instance Prompt TMulType where

-- multi level type
data TMultiType = TMultiType TContainerType
    deriving Generic
-- Prompt currently requires that all contained
-- types have Read instances
-- TODO: allow Prompt instances as well
--instance Prompt TMultiType where
