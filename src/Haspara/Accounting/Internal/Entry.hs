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
    EntryDebit o (PositiveQuantity s)
  | EntryCredit o (PositiveQuantity s)
  deriving (Eq, Ord, Show)


entryQuantity :: KnownNat s => Entry o s -> H.Quantity s
entryQuantity (EntryDebit _ q)  = unrefine q
entryQuantity (EntryCredit _ q) = -(unrefine q)


entryObject :: KnownNat s => Entry o s -> o
entryObject (EntryDebit o _ ) = o
entryObject (EntryCredit o _) = o


entryDebit :: KnownNat s => Entry o s -> Maybe (PositiveQuantity s)
entryDebit (EntryDebit _ x)  = Just x
entryDebit (EntryCredit _ _) = Nothing


entryCredit :: KnownNat s => Entry o s -> Maybe (PositiveQuantity s)
entryCredit (EntryDebit _ _)  = Nothing
entryCredit (EntryCredit _ x) = Just x


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
buildEntry (EventDecrement o x) AccountKindAsset     = EntryCredit o x
buildEntry (EventIncrement o x) AccountKindAsset     = EntryDebit  o x
buildEntry (EventDecrement o x) AccountKindLiability = EntryDebit  o x
buildEntry (EventIncrement o x) AccountKindLiability = EntryCredit o x
buildEntry (EventDecrement o x) AccountKindEquity    = EntryDebit  o x
buildEntry (EventIncrement o x) AccountKindEquity    = EntryCredit o x
buildEntry (EventDecrement o x) AccountKindRevenue   = EntryDebit  o x
buildEntry (EventIncrement o x) AccountKindRevenue   = EntryCredit o x
buildEntry (EventDecrement o x) AccountKindExpense   = EntryCredit o x
buildEntry (EventIncrement o x) AccountKindExpense   = EntryDebit  o x
