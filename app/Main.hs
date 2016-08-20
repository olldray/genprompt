{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Default ( Default(..))
import GHC.Generics ( Generic)
import Lib
import Data.Text as T

data OnlyAnInt = OnlyAnInt Int
    deriving (Show, Generic)

instance Prompt OnlyAnInt where

data BigData = SomethingSimple T.Text | MoreComplicated Int T.Text
    deriving (Show, Generic)

instance Prompt BigData where

main :: IO ()
main = do
    laddend <- prompt
    raddend <- prompt
    print $ go laddend raddend
    where go (OnlyAnInt x) (MoreComplicated y z) = x + y
          go (OnlyAnInt x) (SomethingSimple y) = x * T.length y

