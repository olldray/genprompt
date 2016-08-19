{-# LANGUAGE DefaultSignatures #-}

module Lib
    ( someFunc
    , Prompt (..)
    ) where

import Data.Default ( Default(..))


class Prompt a where
    prompt :: IO a

    default prompt :: (Default a) => IO a
    prompt = return def


someFunc :: IO ()
someFunc = putStrLn "someFunc"
