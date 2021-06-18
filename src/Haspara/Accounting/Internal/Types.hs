module Haspara.Accounting.Internal.Types where

import Haspara.Quantity (Quantity)
import Refined          (Positive, Refined)


type PositiveQuantity s = Refined Positive (Quantity s)
