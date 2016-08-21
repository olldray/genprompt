{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Default ( Default(..))
import GHC.Generics ( Generic)
import Lib
import Data.Text as T

data OnlyAnInt = OnlyAnInt Int
    deriving (Show, Generic)

instance Prompt OnlyAnInt where
instance Describe OnlyAnInt where

data BigData = SomethingSimple T.Text | MoreComplicated Bool (Char, T.Text) 
    deriving (Show, Generic)

instance Prompt BigData where
instance Describe BigData where

main :: IO ()
main = do
    laddend <- prompt
    raddend <- prompt
    print $ go laddend raddend
    where go (OnlyAnInt x) (MoreComplicated y z) = if y
                                                    then x
                                                    else T.length $ snd z
          go (OnlyAnInt x) (SomethingSimple y) = x * T.length y

