-- |
-- Module: Tiny
-- Description: Basic example.
--
-- Show how to generate helper and use it to create simple program
-- for rudimentary stack and memory virtual machine

{-# LANGUAGE TemplateHaskell, RecursiveDo #-}

module Language.Asm.Example.Tiny where

import Prelude hiding (not, drop)

import Language.Asm
import Language.Asm.TH

--------------------------------------------------------------------------------

-- * Target virtual machine

-- | Instruction set type
data I
	= Ld -- ^ address -- value
	| St -- value address --

	| Gt | GtE | Lt | LtE | Eq
	| Add

	| Jmp Int -- absolute jump
	| CJmp Int -- conditional absolute jump

	| Push Int
	| Dup | Drop | Swap | Over
	deriving Show

evalOne :: I -> ([Int], Int, [Int]) -> ([Int], Int, [Int])
evalOne (Push k) (m, i, d) = (m, i, k : d)
evalOne Ld (m, i, a:d) = (m, i, (m !! a) : d)
evalOne St (m, i, a:v:d) = (setElem m a v, i, d)
	where
	setElem lst idx val = let (l, _:u) = splitAt idx lst in l ++ val : u
evalOne LtE (m, i, y:x:d) = (m, i, (if x >= y then 1 else 0) : d)
evalOne Eq (m, i, y:x:d) = (m, i, (if x == y then 1 else 0) : d)
evalOne Add (m, i, y:x:d) = (m, i, (x + y) : d)
evalOne (CJmp a) (m, i, x:d) = (m, if x == 0 then i else a, d)
evalOne (Jmp a) (m, _, d) = (m, a, d)
evalOne Drop (m, i, _:d) = (m, i, d)
evalOne Dup (m, i, x:d) = (m, i, x:x:d)
evalOne Over (m, i, x:y:d) = (m, i, y:x:y:d)
evalOne Swap (m, i, x:y:d) = (m, i, y:x:d)
evalOne i st = error ("evalOne " ++ show (i, st))

eval :: [I] -> ([Int], Int, [Int]) -> ([Int], Int, [Int])
eval p st' = let
	end = length p
	f k st@(m, i, d)
		| i >= k = st
		| otherwise = f k (evalOne (p !! i) (m, i + 1, d))
	in f end st'

--------------------------------------------------------------------------------

addInstruction :: Monad m => I -> AsmT Int I m ()
addInstruction = flip instruction_ 1


-- * Generated helpers

$(instructionSet "addInstruction" "Int" "I" "" False)

-- * Using embedded assembler

-- | See source for details
sumArray :: Asm Int I ()
sumArray = mdo

	push 0 --number of cells
	ld
	push 1 --add address of start of array
	add --this is start value

	push 0 --sum

	begin <- label
	over --check for cycle end
	push 1
	eq
	cjmp end

	swap
	push (-1) --decrement
	add
	swap

	over
	ld
	add

	jmp begin
	end <- label

	push 0
	ld
	push 1
	add
	st

	drop --discard cycle counter


-- | Resulting program, can be displayed with 'Prelude.show'
sumArrayProg :: [I]
sumArrayProg = assemble sumArray
