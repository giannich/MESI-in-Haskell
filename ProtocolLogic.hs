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

{- Executes the instruction on the machine state -}
execute :: Machine -> Instruction -> Machine
execute oldMachine (Instruction Read cacheAdd addNum) = readCache oldMachine cacheAdd addNum
execute oldMachine (Instruction Write cacheAdd addNum) = writeCache oldMachine cacheAdd addNum

{- Reads from the Cache -}
readCache :: Machine -> Int -> Int -> Machine
readCache m cacheAdd readAdd = readCache' m (getCache m cacheAdd) readAdd

readCache' :: Machine -> Cache -> Int -> Machine
readCache' m (Cache _ cuList) readAdd = undefined

{- Writes on the Cache -}
writeCache :: Machine -> Int -> Int -> Machine
writeCache oldMachine cacheAdd writeAdd = undefined

{- Ancillary Functions -}
getCache :: Machine -> Int -> Cache
getCache (Machine _ cList) add = 
  head $ filter (\c -> if add == (cacheNum c) then True else False) cList

isCacheUnitFree :: Cache -> Int -> Bool
isCacheUnitFree c a = undefined
  where
    cu = getCacheUnit c a

getCacheUnit :: Cache -> Int -> CacheUnit
getCacheUnit (Cache _ cList) a = undefined

{- For all flushing operations -}
flushCacheUnit :: CacheUnit -> CacheUnit
flushCacheUnit c = c{state=I}

storeCacheUnit :: Memory -> CacheUnit -> (Memory, CacheUnit)
storeCacheUnit m c@(CacheUnit M (MemUnit a v)) = (storeCacheUnit' m a v, flushCacheUnit c)

storeCacheUnit' :: Memory -> Int -> Int -> Memory
storeCacheUnit' m@(Memory oldMUList) a v = m{memList=map (modifyMem a v) oldMUList}

modifyMem :: Int -> Int -> MemUnit -> MemUnit
modifyMem a newV m@(MemUnit oldA _) = if oldA == a then m{value=newV} else m

