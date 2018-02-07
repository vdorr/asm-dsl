{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE CPP, FlexibleInstances, FlexibleContexts #-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Language.Asm where

#if 0
import Control.Monad.Fix
import Control.Applicative

--------------------------------------------------------------------------------

--TODO use proper formulation hierarchy Functor/Applicative/Monad

newtype Asm addr inst a = Asm ((addr, [inst]) -> (a, (addr, [inst])))

instance Num addr => Monad (Asm addr inst) where
	Asm x >>= k = Asm $ \s -> let
		(a, s') = x s
		Asm λ = k a
		in λ s'
	return a = Asm $ \s -> (a, s)

instance Num addr => MonadFix (Asm addr inst) where
	mfix k = Asm $ \s -> let
		Asm m = k $ fst ans
		ans = m s
		in ans

instruction :: Num addr => inst -> addr -> Asm addr inst ()
instruction i size = Asm $ \(a, l) -> ((), (a + size, i:l))

--TODO non-mdo version
label :: Asm addr inst addr
label = Asm $ \s -> (fst s, s)

assemble :: Num addr => Asm addr inst a -> [inst]
assemble (Asm x) = reverse $ snd $ snd $ x (0, [])

instance Num addr => Applicative (Asm addr inst) where
	x <*> y = x >>= \f -> y >>= return . f
	pure = return

instance Num addr => Functor (Asm addr inst) where
	fmap f g = g >>= return . f

--------------------------------------------------------------------------------
#else
import Control.Monad.State
import Control.Monad.Identity
--import Control.Applicative

data AsmState addr inst = AsmState { asmtAddress :: addr, asmtListing :: [inst] } --TODO annotations

--newtype AsmT addr inst m a = AsmT (StateT (AsmState addr inst) m a)
type AsmT addr inst m a = StateT (AsmState addr inst) m a
type Asm addr inst a = AsmT addr inst Identity a

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

#endif
