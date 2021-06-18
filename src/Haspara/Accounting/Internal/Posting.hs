{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

module Haspara.Accounting.Internal.Posting where

import qualified Data.List.NonEmpty                  as NE
import           GHC.TypeLits                        (KnownNat, Nat)
import           Haspara.Accounting.Internal.Account (Account(accountKind))
import           Haspara.Accounting.Internal.Entry   (Entry, buildEntry)
import           Haspara.Accounting.Internal.Event   (Event, eventObject)


newtype Posting a o (s :: Nat) = Posting (NE.NonEmpty (Event o s, Account a))
  deriving (Eq, Ord, Show)


postingEvents :: (KnownNat s) => Posting a o s -> [o]
postingEvents (Posting es)  = eventObject . fst <$> NE.toList es


post :: (KnownNat s) => Posting a o s -> [(Account a, Entry o s)]
post (Posting xs)       = go (NE.toList xs)
  where
    go []              = []
    go ((ev, ac) : ys) = (ac, buildEntry ev (accountKind ac)) : go ys
