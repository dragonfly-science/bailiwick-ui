module Bailiwick.State
where

import Data.ByteString

-- The application has two parts of the state:
--   1. The selected indicator, area, year, feature etc.,
--   2. The selected presentation.
-- The state is held in a single dynamic at the top level.


data Message 
  = SetPath ByteString

data State = State ByteString

