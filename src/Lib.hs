{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module Lib
    ( Prompt (..)
    , Describe (..)
    ) where

import Prelude hiding ( putStrLn
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
                    , C
                    , (:+:)(..)
                    , (:*:)(..)
                    , Datatype(..)
                    , Constructor(..)
                    )

-----------------------------------------------------------------------------------

class Prompt a where
    prompt :: (CommandLine m, Describe a) => m a

    default prompt :: (CommandLine m, Describe a, Generic a, GPrompt (Rep a)) => m a
    prompt = do
        let dataName = describe (undefined :: a)
            statement = T.concat ["Now filling a ", dataName, ": "]
            in putStrLn . T.unpack $ statement
        thing <- gprompt
        return $ to thing

class GPrompt a where
    gprompt :: (CommandLine m) => m (a p)

instance (GPrompt f) => GPrompt (M1 i t f) where
    gprompt = do
        thing <- gprompt
        return $ M1 thing

instance (GPrompt f, GPrompt g, GDescribe f, GDescribe g) => GPrompt (f :+: g) where
    gprompt = do
        let lName = describe' (undefined :: (f p))
            rName = describe' (undefined :: (g p))
            statement = T.concat ["Choose between ", lName, " or ", rName, "\nl or r? "]
            in putStrLn $ T.unpack statement
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

instance (Read c, Describe c) => GPrompt (K1 i c) where
    gprompt = do
        let thingName = describe (undefined :: c)
            statement = T.concat ["I need a ", thingName, ": "]
            in putStrLn $ T.unpack statement
        thing <- getLine
        let thing' = read thing
        return $ K1 thing'

instance GPrompt V1 where
    gprompt = undefined

instance GPrompt U1 where
    gprompt = return U1

-----------------------------------------------------------------------------------

-----------------------------------------------------------------------------------

class Describe a where
    describe :: a -> T.Text

    default describe :: (Generic a, GDescribe (Rep a)) => a -> T.Text
    describe a = describe' $ from a

class GDescribe a where
    describe' :: a p -> T.Text

instance (Datatype t) => GDescribe (M1 D t f) where
    describe' a = T.pack $ datatypeName a

instance (Constructor t) => GDescribe (M1 C t f) where
    describe' a = T.pack $ conName a



instance Describe Bool where
instance Describe Char where
instance Describe Double where
instance Describe Float where
instance Describe Int where
instance Describe Integer where
    describe _ = "Integer"
instance Describe Ordering where
instance (Describe a) => Describe [a] where
    describe _ = T.concat ["[", describe (undefined :: a), "]"]
instance (Describe a) => Describe (Maybe a) where
    describe _ = T.concat ["Maybe ", describe (undefined :: a)]
instance (Describe a, Describe b) => Describe (Either a b) where
    describe _ = T.concat ["Either ", describe (undefined :: a), describe (undefined :: b)]
instance Describe () where
instance (Describe a, Describe b) => Describe (a,b) where
    describe _ = T.concat [ "("
                          , describe (undefined :: a)
                          , ", "
                          , describe (undefined :: b)
                          , ")"
                          ]
instance (Describe a, Describe b, Describe c) => Describe (a,b,c) where
    describe _ = T.concat [ "("
                          , describe (undefined :: a)
                          , ", "
                          , describe (undefined :: b)
                          , ", "
                          , describe (undefined :: c)
                          ,")"
                          ]
instance Describe T.Text where
    describe _ = "Text"

-----------------------------------------------------------------------------------

class Monad m => CommandLine m where
    putStrLn :: String -> m ()
    getLine :: m String

instance CommandLine IO where
    putStrLn = Prelude.putStrLn
    getLine = Prelude.getLine
