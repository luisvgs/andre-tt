module Shared
    (Repl(..)
    , Context
    , SubtypeContext
    , Variable
    , ReplState
    ) where
import           Control.Monad.State
import           Expr                (Expr (..))

type ReplState = StateT Repl IO

type Variable = String

data Repl = Repl
    { context :: Context
    , subtype :: SubtypeContext
    , fresh   :: Integer
    }

type Context = [(Variable, (Expr, Maybe Expr))] -- a : A
type SubtypeContext = [(Expr, [Expr])] -- C <: A
