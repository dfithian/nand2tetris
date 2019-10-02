module Code where

import ClassyPrelude
import Control.Lens (assign, at, modifying, use)
import Control.Lens.TH (makeLenses)
import Control.Monad.State (MonadState, evalStateT)
import Data.Char (intToDigit)
import Numeric (showIntAtBase)

import qualified Types as T

data AssemblerError = AssemblerError Text
  deriving (Eq, Show)

instance Exception AssemblerError

data AssemblerState = AssemblerState
  { _assemblerStateSymbolTable            :: Map T.Symbol Int
  , _assemblerStateLabelTable             :: Map T.Symbol Int
  , _assemblerStateVariableAddressCounter :: Int
  , _assemblerStateLabelAddressCounter    :: Int
  } deriving (Eq, Ord, Show)

makeLenses ''AssemblerState

baseVariableAddress, baseLabelAddress :: Int
baseVariableAddress = 16
baseLabelAddress = 0

emptyAssemblerState :: AssemblerState
emptyAssemblerState = AssemblerState knownSymbols mempty baseVariableAddress baseLabelAddress

nextVariableAddress :: (MonadState AssemblerState m) => m Int
nextVariableAddress = do
  i <- use assemblerStateVariableAddressCounter
  assign assemblerStateVariableAddressCounter (i + 1)
  pure i

currentLabelAddress :: (MonadState AssemblerState m) => m Int
currentLabelAddress = use assemblerStateLabelAddressCounter

incLabelAddress :: (MonadState AssemblerState m) => m ()
incLabelAddress = modifying assemblerStateLabelAddressCounter (+ 1)

knownSymbols :: Map T.Symbol Int
knownSymbols = mapFromList
  [ (T.Symbol "SP", 0)
  , (T.Symbol "LCL", 1)
  , (T.Symbol "ARG", 2)
  , (T.Symbol "THIS", 3)
  , (T.Symbol "THAT", 4)
  , (T.Symbol "R0", 0)
  , (T.Symbol "R1", 1)
  , (T.Symbol "R2", 2)
  , (T.Symbol "R3", 3)
  , (T.Symbol "R4", 4)
  , (T.Symbol "R5", 5)
  , (T.Symbol "R6", 6)
  , (T.Symbol "R7", 7)
  , (T.Symbol "R8", 8)
  , (T.Symbol "R9", 9)
  , (T.Symbol "R10", 10)
  , (T.Symbol "R11", 11)
  , (T.Symbol "R12", 12)
  , (T.Symbol "R13", 13)
  , (T.Symbol "R14", 14)
  , (T.Symbol "R15", 15)
  , (T.Symbol "SCREEN", 16384)
  , (T.Symbol "KBD", 24576)
  ]

fillZeroes :: Int -> Text -> Text
fillZeroes len str =
  case length str of
    short | short < len -> replicate (len - short) '0' <> str
    long | long > len   -> drop (long - len) str
    _                   -> str

assemble :: (MonadIO m) => [T.Command] -> m [T.Code]
assemble commands = flip evalStateT emptyAssemblerState $ do
  traverse_ assembleLabel commands
  catMaybes <$> traverse assembleCommand commands

assembleLabel :: (MonadIO m, MonadState AssemblerState m) => T.Command -> m ()
assembleLabel = \ case
  T.CommandLabel x ->
    use (assemblerStateSymbolTable . at x) >>= \ case
      Just _ -> throwIO . AssemblerError $ "Already a symbol for " <> tshow x
      Nothing -> do
        address <- currentLabelAddress
        assign (assemblerStateSymbolTable . at x) (Just address)
  _ -> incLabelAddress

assembleCommand :: (MonadIO m, MonadState AssemblerState m) => T.Command -> m (Maybe T.Code)
assembleCommand = \ case
  T.CommandLabel _ -> pure Nothing
  T.CommandSymbol x -> do
    address <- use (assemblerStateSymbolTable . at x) >>= \ case
      Nothing -> do
        address <- nextVariableAddress
        assign (assemblerStateSymbolTable . at x) (Just address)
        pure address
      Just address -> pure address
    pure . Just . T.Code . fillZeroes 16 . pack . showIntAtBase 2 intToDigit address $ ""
  T.CommandLiteral (T.Literal i) ->
    pure . Just . T.Code . fillZeroes 16 . pack . showIntAtBase 2 intToDigit i $ ""
  T.CommandComp dest comp ->
    let destBits = case dest of
          T.DestA   -> "100"
          T.DestD   -> "010"
          T.DestM   -> "001"
          T.DestAM  -> "101"
          T.DestAD  -> "110"
          T.DestMD  -> "011"
          T.DestAMD -> "111"
        compBits = case comp of
          T.Comp0     -> "0101010"
          T.Comp1     -> "0111111"
          T.Comp_1    -> "0111010"
          T.CompD     -> "0001100"
          T.CompA     -> "0110000"
          T.CompBangD -> "0001101"
          T.CompBangA -> "0110001"
          T.Comp_D    -> "0001111"
          T.Comp_A    -> "0110011"
          T.CompD1    -> "0011111"
          T.CompA1    -> "0110111"
          T.CompD_1   -> "0001110"
          T.CompA_1   -> "0110010"
          T.CompDA    -> "0000010"
          T.CompD_A   -> "0010011"
          T.CompA_D   -> "0000111"
          T.CompAXD   -> "0000000"
          T.CompDOA   -> "0010101"
          T.CompM     -> "1110000"
          T.CompBangM -> "1110001"
          T.Comp_M    -> "1110011"
          T.CompM1    -> "1110111"
          T.CompM_1   -> "1110010"
          T.CompDM    -> "1000010"
          T.CompD_M   -> "1010011"
          T.CompM_D   -> "1000111"
          T.CompDXM   -> "1000000"
          T.CompDOM   -> "1010101"
    in pure . Just . T.Code $ "111" <> compBits <> destBits <> "000"
  T.CommandJump cond jump ->
    let condBits = case cond of
          T.CondA -> "0110000" -- CompA
          T.CondD -> "0001100" -- CompD
          T.CondM -> "1110000" -- CompM
          T.Cond0 -> "0101010" -- Comp0
        jumpBits = case jump of
          T.JumpEQ  -> "010"
          T.JumpGT  -> "001"
          T.JumpGE  -> "011"
          T.JumpLT  -> "100"
          T.JumpNE  -> "101"
          T.JumpLE  -> "110"
          T.JumpAny -> "111"
    in pure . Just . T.Code $ "111" <> condBits <> "000" <> jumpBits
