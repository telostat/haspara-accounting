module Haspara.Accounting.Internal.Types where

import Haspara.Quantity (Quantity)
import Refined          (NonNegative, Refined)


type UnsignedQuantity s = Refined NonNegative (Quantity s)
