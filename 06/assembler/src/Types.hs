module Types where

import ClassyPrelude
import Control.Lens.TH (makePrisms)

newtype Code = Code { unCode :: Text }
  deriving (Eq, Ord, Show)

newtype Symbol = Symbol { unSymbol :: Text }
  deriving (Eq, Ord, Show)

newtype Literal = Literal { unLiteral :: Int }
  deriving (Eq, Ord, Show)

data Dest
  = DestA
  | DestD
  | DestM
  | DestMD
  | DestAM
  | DestAD
  | DestAMD
  deriving (Eq, Ord, Show)

data Comp
  = Comp0
  | Comp1
  | Comp_1
  | CompD
  | CompA
  | CompBangD
  | CompBangA
  | Comp_D
  | Comp_A
  | CompD1
  | CompA1
  | CompD_1
  | CompA_1
  | CompDA
  | CompD_A
  | CompA_D
  | CompAXD
  | CompDOA
  | CompM
  | CompBangM
  | Comp_M
  | CompM1
  | CompM_1
  | CompDM
  | CompD_M
  | CompM_D
  | CompDXM
  | CompDOM
  deriving (Eq, Ord, Show)

data Cond = CondA | CondD | CondM | Cond0
  deriving (Eq, Ord, Show)

data Jump = JumpEQ | JumpGT | JumpGE | JumpLT | JumpNE | JumpLE | JumpAny
  deriving (Eq, Ord, Show)

data Command
  = CommandLabel Symbol
  | CommandSymbol Symbol
  | CommandLiteral Literal
  | CommandComp Dest Comp
  | CommandJump Cond Jump
  deriving (Eq, Ord, Show)

makePrisms ''Symbol
makePrisms ''Literal
makePrisms ''Dest
makePrisms ''Comp
makePrisms ''Cond
makePrisms ''Jump
makePrisms ''Command
