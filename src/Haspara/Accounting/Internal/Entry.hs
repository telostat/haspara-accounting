{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Haspara.Accounting.Internal.Entry where

import           GHC.TypeLits                            (KnownNat, Nat)
import qualified Haspara                                 as H
import           Haspara.Accounting.Internal.AccountKind (AccountKind(..))
import           Haspara.Accounting.Internal.Event       (Event(..))
import           Haspara.Accounting.Internal.Types       (PositiveQuantity)
import           Refined                                 (unrefine)


data Entry o (s :: Nat) =
    EntryDebit H.Date o (PositiveQuantity s)
  | EntryCredit H.Date o (PositiveQuantity s)
  deriving (Eq, Ord, Show)


entryDate :: KnownNat s => Entry o s -> H.Date
entryDate (EntryDebit d _ _)  = d
entryDate (EntryCredit d _ _) = d


entryQuantity :: KnownNat s => Entry o s -> H.Quantity s
entryQuantity (EntryDebit _ _ q)  = unrefine q
entryQuantity (EntryCredit _ _ q) = -(unrefine q)


entryObject :: KnownNat s => Entry o s -> o
entryObject (EntryDebit _ o _)  = o
entryObject (EntryCredit _ o _) = o


entryDebit :: KnownNat s => Entry o s -> Maybe (PositiveQuantity s)
entryDebit (EntryDebit _ _ x) = Just x
entryDebit EntryCredit {}     = Nothing


entryCredit :: KnownNat s => Entry o s -> Maybe (PositiveQuantity s)
entryCredit EntryDebit {}       = Nothing
entryCredit (EntryCredit _ _ x) = Just x


-- |
--
-- +-----------------------+----------+----------+
-- | Kind of account       | Debit    | Credit   |
-- +-----------------------+----------+----------+
-- | Asset                 | Increase | Decrease |
-- +-----------------------+----------+----------+
-- | Liability             | Decrease | Increase |
-- +-----------------------+----------+----------+
-- | Equity/Capital        | Decrease | Increase |
-- +-----------------------+----------+----------+
-- | Income/Revenue        | Decrease | Increase |
-- +-----------------------+----------+----------+
-- | Expense/Cost/Dividend | Increase | Decrease |
-- +-----------------------+----------+----------+
--
buildEntry :: (KnownNat s) => Event o s -> AccountKind -> Entry o s
buildEntry (EventDecrement d o x) AccountKindAsset     = EntryCredit d o x
buildEntry (EventIncrement d o x) AccountKindAsset     = EntryDebit  d o x
buildEntry (EventDecrement d o x) AccountKindLiability = EntryDebit  d o x
buildEntry (EventIncrement d o x) AccountKindLiability = EntryCredit d o x
buildEntry (EventDecrement d o x) AccountKindEquity    = EntryDebit  d o x
buildEntry (EventIncrement d o x) AccountKindEquity    = EntryCredit d o x
buildEntry (EventDecrement d o x) AccountKindRevenue   = EntryDebit  d o x
buildEntry (EventIncrement d o x) AccountKindRevenue   = EntryCredit d o x
buildEntry (EventDecrement d o x) AccountKindExpense   = EntryCredit d o x
buildEntry (EventIncrement d o x) AccountKindExpense   = EntryDebit  d o x
