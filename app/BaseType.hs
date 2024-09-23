{-# LANGUAGE GADTs #-}
module BaseType where

data BaseType where
    Integer :: Int -> BaseType
    Boolean :: Bool -> BaseType
    deriving(Show, Eq)
