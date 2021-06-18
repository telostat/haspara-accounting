{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}

module Haspara.Accounting.Internal.Event where

import Control.Monad.Except              (MonadError(throwError))
import GHC.TypeLits                      (KnownNat, Nat)
import Haspara.Accounting.Internal.Types (PositiveQuantity)
import Haspara.Quantity                  (Quantity)
import Refined                           (refine)


data Event o (s :: Nat) =
    EventDecrement o (PositiveQuantity s)
  | EventIncrement o (PositiveQuantity s)
  deriving (Eq, Ord, Show)


eventObject :: (KnownNat s) => Event o s -> o
eventObject (EventDecrement o _) = o
eventObject (EventIncrement o _) = o


negateEvent :: (KnownNat s) => Event o s -> Event o s
negateEvent (EventDecrement o x) = EventIncrement o x
negateEvent (EventIncrement o x) = EventDecrement o x


mkEvent :: (MonadError String m, KnownNat s) => o -> Quantity s -> m (Event o s)
mkEvent o x
  | x < 0 = either (throwError . show) pure $ EventDecrement o <$> refine (abs x)
  | x > 0 = either (throwError . show) pure $ EventIncrement o <$> refine (abs x)
  | otherwise = throwError "There is no event-type defined for zero values."
