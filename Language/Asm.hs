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
	, setPointer
	, annotate_
	, annotated_
	, assembleT
	, assemble
	, assembleT_
	, assemble_
) where

import Control.Monad.State
import Control.Monad.Identity

-- | Assembler monad state
data AsmState addr inst ann = AsmState
	{ asmtAddress :: addr
	, asmtListing :: [(inst, ann)] } --TODO annotations, maybe

type AsmT addr inst ann m a = StateT (AsmState addr inst ann) m a

type Asm addr inst ann a = AsmT addr inst ann Identity a

-- | Add instruction to program and move memory pointer by specified amount and return address of instruction
instruction :: (Monad m, Num addr, Monoid ann)
	=> inst -- ^ Instruction inserted to program
	-> addr -- ^ Size of instruction
	-> AsmT addr inst ann m addr
instruction i size = label <* instruction_ i size

-- | Add instruction with annotation
annotated_ :: (Monad m, Num addr, Monoid ann)
	=> ann -- ^
	-> inst
	-> addr
	-> AsmT addr inst ann m ()
annotated_ ann i size = modify $ \st -> st
	{ asmtAddress = asmtAddress st + size
	, asmtListing = (i, ann) : asmtListing st }

-- | Add annotation to last added instruction, annotation is discarded if program is empty
annotate_ :: (Monad m, Monoid ann)
	=> ann -- ^
	-> AsmT addr inst ann m ()
annotate_ ann' = modify $ \st -> case asmtListing st of
	[] -> st
	(inst, ann) : xs -> st { asmtListing = (inst, mappend ann ann') : xs }

-- | Add instruction to program and move memory pointer by specified amount
instruction_ :: (Monad m, Num addr, Monoid ann)
	=> inst -- ^ Instruction inserted to program
	-> addr -- ^ Size of instruction
	-> AsmT addr inst ann m ()
instruction_ = annotated_ mempty

-- | Set current adress
setPointer :: (Monad m) => addr -> AsmT addr inst ann m ()
setPointer v = modify $ \st -> st { asmtAddress = v }

--TODO non-mdo version
-- | Extract current memory pointer value
label :: (Monad m) => AsmT addr inst ann m addr
label = gets asmtAddress

-- | Assemble program with annotations
assembleAnnotatedT :: (Monad m, Num addr) => AsmT addr inst ann m a -> m (a, [(inst, ann)])
assembleAnnotatedT p = ((reverse . asmtListing) <$>) <$> runStateT p (AsmState 0 [])

-- | Assemble program
assembleT :: (Monad m, Num addr) => AsmT addr inst ann m a -> m (a, [inst])
assembleT p = ((reverse . fmap fst . asmtListing) <$>) <$> runStateT p (AsmState 0 [])

-- | Assemble program
assemble :: (Num addr) => Asm addr inst ann a -> (a, [inst])
assemble = runIdentity . assembleT

-- | Version of 'assembleT' that discards result
assembleT_ :: (Monad m, Num addr) => AsmT addr inst ann m a -> m [inst]
assembleT_ = (snd <$>) . assembleT

-- | Version of 'assemble' that discards result
assemble_ :: (Num addr) => Asm addr inst ann a -> [inst]
assemble_ = snd . assemble

