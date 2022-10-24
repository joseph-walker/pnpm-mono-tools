module Data.Task ( Task ) where

import Control.Monad.Except ( ExceptT )

type Task = ExceptT String IO
