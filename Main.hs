module Main(
    main 
) where 

import ProtocolLogic
import DataTypes

{- Static Parameters -}

memoryAddresses = 4
cacheNumbers = 3
cacheAddresses = 2

----------
-- MAIN --
----------

{- Main Function -}
main :: IO()
main = do
  let machine = initMachine memoryAddresses cacheNumbers cacheAddresses
  (putStrLn.show) machine  
