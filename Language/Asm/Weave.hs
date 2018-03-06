
module Language.Asm.Weave (
	  P(..)
	, weaveM
) where

import Control.Monad (mapM)
--import Debug.Trace

-- | Program element
data P a
	= Action a -- ^ simple action
	| Jump Bool Int -- ^ jump to absolute address, flag indicates conditional jump

-- |
weaveM :: Monad m =>
	(i -> P (m a)) -- ^
	-> m a -- ^ action to run last, usually "return ()"
	-> m Bool -- ^ action that returns state of condition variable, True to jump
	-> [i] -- ^ input program code
	-> Either String (m a) -- ^ resulting action or error message regarding jump target
weaveM f exit cf p = weave exit cf <$> mapM g (zip p [(0::Int)..])
	where
	len = length p
	g (i, addr) = case f i of
		j@(Jump _ d) | d >= 0 && d < len -> return j
		Jump _ d -> Left $ "invalid jump target " ++ show d ++ " at "
			++ show addr ++ ", program length is " ++ show len
		action -> return action

--TODO consider seq
weave :: Monad m => m b -> m Bool -> [P (m b)] -> m b
weave exit cf = fst . f exit [] . reverse
	where
	f p steps (Action a : rest) = let
		a' = a >> p
		in f a' (a' : steps) rest
--		in traceShow "c1" $ f a' (a' : steps) rest
	f p steps (Jump conditional d : rest) = let
		a'	| conditional = cf >>= \c -> if c then target else p
			| otherwise = target
		r@(_, whole) = f a' (a' : steps) rest
		target = whole !! d
		in r
--		in traceShow "c2" r
	f p steps [] = (p, steps)


