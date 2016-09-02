module SpaceHash ( new
                 , add
                 , remove
                 ) where 

import Types
import qualified Data.Map as DM
import qualified Data.Set as DS
                   
new bucketSize = SpaceHash bucketSize DM.empty

addToBucket el (SpaceHash bucketSize store) loc =
  let bucket = case DM.lookup loc store of
        -- check if bucket exists
        Nothing -> DS.insert el DS.empty
        Just b -> DS.insert el b
      store' = DM.insert loc bucket store      
  in SpaceHash bucketSize store'

removeFromBucket el (SpaceHash bucketSize store) loc =
  let store' = case DM.lookup loc store of
        -- check if bucket exists
        Nothing -> store -- do nothing.
        Just bucket ->
          let b = DS.delete el bucket
          -- if the bucket is now empty              
          in if DS.size b == 0
             -- remove the entire bucket to prevent space leak.
             then DM.delete loc store
             -- just remove el from bucket.
             else DM.insert loc (DS.delete el b) store
  in SpaceHash bucketSize store'

generateSpan bucketSize elLoc@(Loc x y) elWidth elHeight = 
  let left = x - (x `mod` bucketSize) -- rectify to grid.
      right = (x + elWidth) - (x + elWidth `mod` bucketSize)
      top = y - (y `mod` bucketSize)
      bottom = (y + elWidth) - (y + elHeight `mod` bucketSize)
      bucketLocs = [Loc bx by |
                    bx <- [left, left+bucketSize .. right],
                    by <- [top, top+bucketSize .. bottom]]
  in bucketLocs

add :: Ord a => SpaceHash a -> Loc -> a -> Integer -> Integer -> SpaceHash a
add sh@(SpaceHash bucketSize store) elLoc@(Loc x y) el elWidth elHeight =
  let bucketLocs = generateSpan bucketSize elLoc elWidth elHeight
  in foldl (addToBucket el) sh bucketLocs

remove :: Ord a => SpaceHash a -> Loc -> a -> Integer -> Integer -> SpaceHash a
remove sh@(SpaceHash bucketSize store) elLoc@(Loc x y) el elWidth elHeight =
  let bucketLocs = generateSpan bucketSize elLoc elWidth elHeight
  in foldl (removeFromBucket el) sh bucketLocs
     
  
  
