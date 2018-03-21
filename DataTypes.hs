module DataTypes 
(
    Machine(..),
    Memory(..),
    MemUnit(..),
    Cache(..),
    CacheUnit(..),
    State(..),
    Instruction(..),
    InstructionType(..)
) where 

type Memory = [MemUnit]
type MemUnit = (Int, Int)
type Machine = (Memory, [Cache])

data Cache = Cache {cacheNum :: Int,
                    cacheList :: [CacheUnit]}
                    deriving (Eq)

data CacheUnit = CacheUnit {state :: State,
                            address :: Int,
                            value :: Int} 
                            deriving (Eq)

data State = M | E | S | I deriving (Eq, Show)

data Instruction = Instruction {iType :: InstructionType,
                                iCacheNum :: Int,
                                iCacheAdd :: Int}
                                deriving (Eq, Show)

data InstructionType = Read | Write deriving (Eq, Show)

instance Show Cache where
  show (Cache num cList) = "\nCache Number:" ++ show num ++ show cList

instance Show CacheUnit where
  show cUnit = "\nState:" ++ show (state cUnit) ++
               "\nAddress:" ++ show (address cUnit) ++
               "\nValue:" ++ show (value cUnit) ++ "\n"