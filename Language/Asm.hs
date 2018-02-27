-- |
-- Module: Asm
-- Description: Embedded assembler.
--
-- Embedded assembler

module Language.Asm
	( AsmState(..)
	, AsmT
	, Asm
	, instruction
	, instruction_
	, label
	, assembleT
	, assemble
) where

import Control.Monad.State
import Control.Monad.Identity

-- | Assembler monad state
data AsmState addr inst = AsmState
	{ asmtAddress :: addr
	, asmtListing :: [inst] } --TODO annotations

type AsmT addr inst m a = StateT (AsmState addr inst) m a

type Asm addr inst a = AsmT addr inst Identity a


-- | Add instruction to program and move memory pointer by specified amount and return address of instruction
instruction :: (Monad m, Num addr)
	=> inst -- ^ Instruction inserted to program
	-> addr -- ^ Size of instruction
	-> AsmT addr inst m addr
instruction i size = label <* instruction_ i size

-- | Add instruction to program and move memory pointer by specified amount
instruction_ :: (Monad m, Num addr)
	=> inst -- ^ Instruction inserted to program
	-> addr -- ^ Size of instruction
	-> AsmT addr inst m ()
instruction_ i size = modify $ \st -> st
	{ asmtAddress = asmtAddress st + size
	, asmtListing = i : asmtListing st }

--TODO non-mdo version
-- | Extract current memory pointer value
label :: (Monad m) => AsmT addr inst m addr
label = gets asmtAddress

-- | Assemble program
assembleT :: (Monad m, Num addr) => AsmT addr inst m a -> m [inst]
assembleT p
	= reverse . asmtListing <$> execStateT p (AsmState 0 [])

-- | Assemble program
assemble :: (Num addr) => Asm addr inst a -> [inst]
assemble = runIdentity . assembleT

