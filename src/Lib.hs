{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( Prompt (..)
    ) where

import Prelude hiding ( putStrLn
                      , getLine
                      )
import qualified Prelude
import qualified Data.Text as T
import qualified Data.Typeable as Typeable
import Data.Default ( Default(..))
import GHC.Generics ( Generic
                    , Rep
                    , from
                    , to
                    , M1(..)
                    , K1(..)
                    , V1
                    , U1(..)
                    , (:+:)(..)
                    , (:*:)(..)
                    )

-----------------------------------------------------------------------------------

class Prompt a where
    prompt :: (CommandLine m) => m a

    default prompt :: (CommandLine m, Generic a, GPrompt (Rep a)) => m a
    prompt = do
        thing <- gprompt
        return $ to thing

class GPrompt a where
    gprompt :: (CommandLine m) => m (a p)

instance (GPrompt f, Typeable.Typeable f) => GPrompt (M1 i t f) where
    gprompt = do
        putStrLn . show $ Typeable.typeRep (Typeable.Proxy :: Typeable.Proxy f)
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


class Monad m => CommandLine m where
    putStrLn :: String -> m ()
    getLine :: m String

instance CommandLine IO where
    putStrLn = Prelude.putStrLn
    getLine = Prelude.getLine
