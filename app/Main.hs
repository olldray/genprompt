module Main where

import Data.Default ( Default(..))
import Lib

data Wrapper = Wrapper Int
    deriving (Show)

instance Default Wrapper where
    def = Wrapper 23
instance Prompt Wrapper where

main :: IO ()
main = do
    laddend <- prompt
    raddend <- prompt
    print $ go laddend raddend
    where go :: Wrapper -> Wrapper -> Wrapper
          go (Wrapper x) (Wrapper y) = Wrapper (x+y)

