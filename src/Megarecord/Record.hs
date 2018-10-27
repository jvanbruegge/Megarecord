module Megarecord.Record (
        Record
    ) where

import Megarecord.Row (Row)

data Record (r :: Row k) = Record
