{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( someFunc
    , Prompt (..)
    ) where

import Data.Text as T
import Data.Default ( Default(..))
import GHC.Generics ( Generic
                    , Rep
                    , from
                    , to
                    , M1(..)
                    , K1(..)
                    )


class Prompt a where
    prompt :: IO a

    default prompt :: (Generic a, GPrompt (Rep a)) => IO a
    prompt = do
        thing <- gprompt
        return $ to thing

class GPrompt a where
    gprompt :: IO (a p)

instance (GPrompt f) => GPrompt (M1 i t f) where
    gprompt = do
        thing <- gprompt
        return $ M1 thing

instance (Read c) => GPrompt (K1 i c) where
    gprompt = do
        putStrLn $ "I need a thing: "
        thing <- getLine
        let thing' = read thing
        return $ K1 thing'


data Wrapper = Wrapper Int
    deriving (Show)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
