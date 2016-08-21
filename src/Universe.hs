module Universe where

import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Grid
import Types
import qualified LifeForm

new size = Universe (Grid.new size) size 0 (DS.empty)

addLifeForm uv@(Universe _ _ _ lfs) lf = uv { uLifeForms = DS.insert lf lfs }

step (Universe g s t lfs) = Universe g s (t+1) (DS.map LifeForm.step lfs)
  
