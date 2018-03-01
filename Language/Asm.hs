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
	, assembleT_
	, assemble_
	, setPointer
) where

import Control.Monad.State
import Control.Monad.Identity

-- | Assembler monad state
data AsmState addr inst = AsmState
	{ asmtAddress :: addr
	, asmtListing :: [inst] } --TODO annotations, maybe

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

-- | Set current adress
setPointer :: (Monad m) => addr -> AsmT addr inst m ()
setPointer v = modify $ \st -> st { asmtAddress = v }

--TODO non-mdo version
-- | Extract current memory pointer value
label :: (Monad m) => AsmT addr inst m addr
label = gets asmtAddress

-- | Assemble program
assembleT :: (Monad m, Num addr) => AsmT addr inst m a -> m (a, [inst])
assembleT p = ((reverse . asmtListing) <$>) <$> runStateT p (AsmState 0 [])

-- | Assemble program
assemble :: (Num addr) => Asm addr inst a -> (a, [inst])
assemble = runIdentity . assembleT

-- | Version of 'assembleT' that discards result
assembleT_ :: (Monad m, Num addr) => AsmT addr inst m a -> m [inst]
assembleT_ = (snd <$>) . assembleT

-- | Version of 'assemble' that discards result
assemble_ :: (Num addr) => Asm addr inst a -> [inst]
assemble_ = snd . assemble
