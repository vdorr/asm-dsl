{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE CPP, FlexibleInstances, FlexibleContexts #-}

module Language.Asm where

import Control.Monad.State
import Control.Monad.Identity

data AsmState addr inst = AsmState
	{ asmtAddress :: addr
	, asmtListing :: [inst] } --TODO annotations

type AsmT addr inst m a = StateT (AsmState addr inst) m a

type Asm addr inst a = AsmT addr inst Identity a

instruction :: (Monad m, Num addr) => inst -> addr -> AsmT addr inst m ()
instruction i size = modify $ \st -> st {
	  asmtAddress = asmtAddress st + size
	, asmtListing = i : asmtListing st
}

--TODO non-mdo version
label :: (Monad m) => AsmT addr inst m addr
label = gets asmtAddress

assembleT :: (Monad m, Num addr) => AsmT addr inst m a -> m [inst]
assembleT xx = do
	q <- execStateT xx (AsmState 0 [])
	return $ reverse $ asmtListing q

assemble :: (Num addr) => Asm addr inst a -> [inst]
assemble xx = runIdentity $ assembleT xx
