{-# LANGUAGE GADTs #-}
module Expr where

data Expr where
    Var        :: String -> Expr
    Universe   :: Int -> Expr
    Pi         :: String -> Expr -> Expr -> Expr
    Lambda     :: String -> Expr -> Expr -> Expr
    App        :: Expr -> Expr -> Expr
    Gensym     :: String -> Int -> Expr
    Definition  :: String -> Expr -> Expr
    Dummy      :: Expr deriving (Eq, Show)
