module DataTypes 
(
    Machine(..),
    Memory(..),
    Cache(..),
    CacheUnit(..),
    MemUnit(..),
    State(..)
) where 

data Machine = Machine {memory :: Memory,
                        caches :: [Cache]}
                        deriving (Eq, Show)

data Memory = Memory {memList :: [MemUnit]}
                      deriving (Eq)

data Cache = Cache {cacheNum :: Int,
                    cacheList :: [CacheUnit]}
                    deriving (Eq)

data CacheUnit = CacheUnit {state :: State,
                            cMemUnit :: MemUnit} 
                            deriving (Eq)

data MemUnit = MemUnit {address :: Int,
                        value :: Int}
                        deriving (Eq)

data State = M | E | S | I deriving (Eq, Show)

data Instruction = Instruction {iType :: InstructionType,
                                iCacheNum :: Int,
                                iCacheAdd :: Int}
                                deriving (Eq, Show)

data InstructionType = Read | Write deriving (Eq, Show)

instance Show Memory where
  show (Memory mList) = show mList

instance Show Cache where
  show (Cache num cList) = "\nCache Number:" ++ show num ++ show cList

instance Show CacheUnit where
  show cUnit = "\nState:" ++ show (state cUnit) ++ show (cMemUnit cUnit)

instance Show MemUnit where
  show mUnit = "\nAddress:" ++ show (address mUnit) ++
               "\nValue:" ++ show (value mUnit) ++ "\n"