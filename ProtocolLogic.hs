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
  ([(add, 0) | add <- [0..addNum-1]], 
   [Cache cNum cacheUnitList | cNum <- [0..cacheNum-1]])
  where
    cacheUnitList = [CacheUnit I add 0 | add <- [0..cacheSpace-1]]

{- Executes the instruction on the machine state -}
execute :: Machine -> Instruction -> Machine
execute oldMachine (Instruction Read cacheAdd addNum) = readCache oldMachine cacheAdd addNum
execute oldMachine (Instruction Write cacheAdd addNum) = writeCache oldMachine cacheAdd addNum

{- Reads from the Cache -}
readCache :: Machine -> Int -> Int -> Machine
readCache m@(mem, cList) cacheAdd readAdd = readCache' m (getCache cList cacheAdd) readAdd

readCache' :: Machine -> Cache -> Int -> Machine
readCache' m (Cache _ cuList) readAdd = undefined

{- Writes on the Cache -}
writeCache :: Machine -> Int -> Int -> Machine
writeCache oldMachine cacheAdd writeAdd = undefined

{- Ancillary Functions -}
getCache :: [Cache] -> Int -> Cache
getCache cList add = 
  head $ filter (\c -> if add == (cacheNum c) then True else False) cList

getCacheUnit :: Cache -> Int -> Maybe(CacheUnit)
getCacheUnit (Cache _ cuList) add = 
  if filteredList == [] 
    then Nothing 
    else Just(head filteredList)
  where
    filteredList = filter (\cu -> if add == (address cu) then True else False) cuList

{- For all flushing operations -}
flushCacheUnit :: Memory -> CacheUnit -> (Memory, CacheUnit)
flushCacheUnit m c@(CacheUnit M a v) = (storeInMem m a v, flushCacheUnit' c)
flushCacheUnit m c = (m, flushCacheUnit' c)

flushCacheUnit' :: CacheUnit -> CacheUnit
flushCacheUnit' c = c{state=I}

storeInMem :: Memory -> Int -> Int -> Memory
storeInMem oldMUList a v = map (modifyMem a v) oldMUList

modifyMem :: Int -> Int -> MemUnit -> MemUnit
modifyMem a newV (oldA, oldV) = if oldA == a then (oldA, newV) else (oldA, oldV)










