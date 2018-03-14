module ProtocolLogic 
(
  initMachine,
  readCache,
  writeCache
) where 

import DataTypes

{- Initializes the Machine state -}
initMachine :: Int -> Int -> Int -> Machine
initMachine addNum cacheNum cacheSpace = 
  Machine
  (Memory [MemUnit add 0 | add <- [0..addNum-1]])
  ([Cache cNum cacheUnitList | cNum <- [0..cacheNum-1]])
  where
    cacheUnitList = [CacheUnit I (MemUnit add 0) | add <- [0..cacheSpace-1]]

readCache :: Machine -> Int -> (Machine, Int)
readCache oldMachine readAdd = undefined

writeCache :: Machine -> Int -> Machine
writeCache oldMachine writeAdd = undefined