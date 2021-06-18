{-# LANGUAGE DeriveGeneric #-}

module Haspara.Accounting.Internal.Account where

import Data.Hashable                           (Hashable)
import GHC.Generics                            (Generic)
import Haspara.Accounting.Internal.AccountKind (AccountKind)


data Account o = Account
  { accountKind   :: !AccountKind
  , accountObject :: !o
  } deriving (Eq, Generic, Ord, Show)


instance Hashable o => Hashable (Account o)
