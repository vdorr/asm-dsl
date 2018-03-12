-- |
-- Module: Tiny
-- Description: Basic example.
--
-- Show how to generate helper and use it to create simple program
-- for rudimentary stack and memory virtual machine

{-# LANGUAGE TemplateHaskell, RecursiveDo #-}

module Language.Asm.Example.Tiny where

import Prelude hiding (not, drop)
import Control.Monad.State

import Language.Asm
import Language.Asm.TH
import Language.Asm.Weave

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

	| PushNum Int
	| PushBit Bool
	| Dup | Drop | Swap | Over
	deriving Show

-- | Memory cell type
data Cell
	= Bit Bool
	| Num Int
	deriving (Show, Eq)

-- | State of interpreter: memory, instruction pointer and data stack
type VM = ([Cell], Int, [Cell])

evalOne :: I -> VM -> VM
evalOne (PushNum k) (m, i, d) = (m, i, Num k : d)
evalOne (PushBit k) (m, i, d) = (m, i, Bit k : d)
evalOne Ld (m, i, Num a:d) = (m, i, (m !! a) : d)
evalOne St (m, i, Num a:v:d) = (setElem m a v, i, d)
	where
	setElem lst idx val = let (l, _:u) = splitAt idx lst in l ++ val : u
evalOne LtE (m, i, Num y:Num x:d) = (m, i, Bit (x >= y) : d)
evalOne Eq (m, i, y:x:d) = (m, i, Bit (x == y) : d)
evalOne Add (m, i, Num y:Num x:d) = (m, i, Num (x + y) : d)
evalOne (CJmp a) (m, i, Bit x:d) = (m, if x then a else i, d)
evalOne (Jmp a) (m, _, d) = (m, a, d)
evalOne Drop (m, i, _:d) = (m, i, d)
evalOne Dup (m, i, x:d) = (m, i, x:x:d)
evalOne Over (m, i, x:y:d) = (m, i, y:x:y:d)
evalOne Swap (m, i, x:y:d) = (m, i, y:x:d)
evalOne i st = error ("evalOne " ++ show (i, st))

eval :: [I] -> VM -> VM
eval p st' = let
	end = length p
	f k st@(m, i, d)
		| i >= k = st
		| otherwise = f k (evalOne (p !! i) (m, i + 1, d))
	in f end st'

--------------------------------------------------------------------------------

type MonadicVM a = State VM a

monadicEval :: I -> MonadicVM ()
monadicEval = modify . evalOne

toWeaveProg :: I -> P (MonadicVM ())
toWeaveProg (Jmp a) = Jump False a
toWeaveProg (CJmp a) = Jump True a
toWeaveProg i = Action $ monadicEval i

--destructively read jump condition
conditionFlag :: MonadicVM Bool
conditionFlag = get >>= \(m, i, Bit x:d) -> put (m, i, d) >> return x

weaveI :: [I] -> Either String (MonadicVM ())
weaveI = weaveM toWeaveProg (return ()) conditionFlag

runVM :: MonadicVM () -> VM -> VM
runVM = execState

--------------------------------------------------------------------------------

addInstruction :: Monad m => I -> AsmT Int I () m ()
addInstruction = flip instruction_ 1

-- * Generated helpers for instruction set

$(instructionSet "addInstruction" "Int" "I" Nothing "" Nothing False)

addCell :: Monad m => Cell -> AsmT Int Cell () m Int
addCell = flip instruction 1

-- * Generated helpers for data memory

$(instructionSet "addCell" "Int" "Cell" Nothing "" (Just "Int") False)

-- * Using embedded assembler

-- | An example program, see source for details
sumArray :: Asm Int I () ()
sumArray = mdo

	pushnum 0 --number of cells
	ld
	pushnum 1 --add address of start of array
	add --this is start value

	pushnum 0 --sum

	begin <- label
	over --check for cycle end
	pushnum 1
	eq
	cjmp end

	swap
	pushnum (-1) --decrement
	add
	swap

	over
	ld
	add

	jmp begin
	end <- label

	pushnum 0
	ld
	pushnum 1
	add
	st

	drop --discard cycle counter


-- | Resulting program, can be displayed with 'Prelude.show'
sumArrayProg :: [I]
sumArrayProg = assemble_ sumArray

sumArrayProg' :: [Int] -> ([I], [Cell])
sumArrayProg' a = assemble $ do
	bit False
	count <- num 4
	array <- label
	forM_ a num
	result <- num 0
	assembleT_ $ mdo
		pushnum count --address of 'count' cell
		ld
		pushnum array
		add

		pushnum 0 --sum

		begin <- label
		over --check for cycle end
		pushnum 1
		eq
		cjmp end

		swap
		pushnum (-1) --decrement
		add
		swap

		over
		ld
		add

		jmp begin
		end <- label

		pushnum result
		st

		drop --discard cycle counter

