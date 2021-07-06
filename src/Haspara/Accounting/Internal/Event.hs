{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}

module Haspara.Accounting.Internal.Event where

import           Control.Monad.Except              (MonadError(throwError))
import           GHC.TypeLits                      (KnownNat, Nat)
import qualified Haspara                           as H
import           Haspara.Accounting.Internal.Types (PositiveQuantity)
import           Refined                           (refine)


data Event o (s :: Nat) =
    EventDecrement H.Date o (PositiveQuantity s)
  | EventIncrement H.Date o (PositiveQuantity s)
  deriving (Eq, Ord, Show)


eventDate :: (KnownNat s) => Event o s -> H.Date
eventDate (EventDecrement d _ _) = d
eventDate (EventIncrement d _ _) = d


eventObject :: (KnownNat s) => Event o s -> o
eventObject (EventDecrement _ o _) = o
eventObject (EventIncrement _ o _) = o


negateEvent :: (KnownNat s) => Event o s -> Event o s
negateEvent (EventDecrement d o x) = EventIncrement d o x
negateEvent (EventIncrement d o x) = EventDecrement d o x


mkEvent :: (MonadError String m, KnownNat s) => H.Date -> o -> H.Quantity s -> m (Event o s)
mkEvent d o x
  | x < 0 = either (throwError . show) pure $ EventDecrement d o <$> refine (abs x)
  | x > 0 = either (throwError . show) pure $ EventIncrement d o <$> refine (abs x)
  | otherwise = throwError "There is no event-type defined for zero values."
