{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( Prompt (..)
    , Describe (..)
    ) where

import Prelude hiding ( putStrLn
                      , print
                      , getLine
                      )
import qualified Prelude
import qualified Data.Text as T
import Data.Proxy ( Proxy(..))
import Data.Default ( Default(..))
import GHC.Generics ( Generic
                    , Rep
                    , from
                    , to
                    , M1(..)
                    , K1(..)
                    , V1
                    , U1(..)
                    , D
                    , (:+:)(..)
                    , (:*:)(..)
                    , Datatype
                    )

-----------------------------------------------------------------------------------

class Prompt a where
    prompt :: (CommandLine m, Describe a) => m a

    default prompt :: (CommandLine m, Describe a, Generic a, GPrompt (Rep a)) => m a
    prompt = do
        print $ describe (undefined :: a)
        thing <- gprompt
        return $ to thing

class GPrompt a where
    gprompt :: (CommandLine m) => m (a p)

instance (GPrompt f) => GPrompt (M1 i t f) where
    gprompt = do
        thing <- gprompt
        return $ M1 thing

instance (GPrompt f, GPrompt g) => GPrompt (f :+: g) where
    gprompt = do
        putStrLn $ "l or r? "
        answer <- getLine
        case answer of
            "l" -> do
                thing <- gprompt
                return $ L1 thing
            "r" -> do
                thing <- gprompt
                return $ R1 thing
            _ -> error "Don't have an error path yet"

instance (GPrompt f, GPrompt g) => GPrompt (f :*: g) where
    gprompt = do
        lthing <- gprompt
        rthing <- gprompt
        return $ lthing :*: rthing

instance (Read c) => GPrompt (K1 i c) where
    gprompt = do
        putStrLn $ "I need a thing: "
        thing <- getLine
        let thing' = read thing
        return $ K1 thing'

instance GPrompt V1 where
    gprompt = undefined

instance GPrompt U1 where
    gprompt = return U1

-----------------------------------------------------------------------------------

-----------------------------------------------------------------------------------

class (Generic a) => Describe a where
    describe :: a -> T.Text

    default describe :: (GDescribe (Rep a)) => a -> T.Text
    describe a = describe' $ from a

class GDescribe (a :: * -> *) where
    describe' :: a p -> T.Text

instance GDescribe (M1 D t f) where
    describe' _ = "Hello"

-----------------------------------------------------------------------------------

class Monad m => CommandLine m where
    putStrLn :: String -> m ()
    print :: (Show a) => a -> m ()
    getLine :: m String

instance CommandLine IO where
    putStrLn = Prelude.putStrLn
    print = Prelude.print
    getLine = Prelude.getLine
