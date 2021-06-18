{-# LANGUAGE DeriveGeneric #-}

module Haspara.Accounting.Internal.AccountKind where

import qualified Data.Aeson    as Aeson
import qualified Data.Char     as C
import           Data.Hashable (Hashable)
import qualified Data.Text     as T
import           GHC.Generics  (Generic)


data AccountKind =
    AccountKindAsset
  | AccountKindLiability
  | AccountKindEquity
  | AccountKindRevenue
  | AccountKindExpense
  deriving (Enum, Eq, Generic, Ord, Show)


instance Hashable AccountKind


instance Aeson.FromJSON AccountKind where
  parseJSON = Aeson.withText "AccountKind" $ \t -> case T.map C.toUpper t of
    "ASSET"     -> pure AccountKindAsset
    "LIABILITY" -> pure AccountKindLiability
    "EQUITY"    -> pure AccountKindEquity
    "REVENUE"   -> pure AccountKindRevenue
    "EXPENSE"   -> pure AccountKindExpense
    _           -> fail $ "Unknown account kind: " <> show t


accountKindText :: AccountKind -> T.Text
accountKindText AccountKindAsset     = "Asset"
accountKindText AccountKindLiability = "Liability"
accountKindText AccountKindEquity    = "Equity"
accountKindText AccountKindRevenue   = "Revenue"
accountKindText AccountKindExpense   = "Expense"
