module Types where

import Control.Monad.Trans.Reader
import Env

type App = ReaderT Env IO
