{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE TemplateHaskell, RecursiveDo #-}

module Language.Asm.Example.Tiny where

import Prelude hiding (not, drop)

import Language.Asm
import Language.Asm.TH

data I
	= Ld -- address -- value
	| St -- value address --

	| Gt | GtE | Lt | Eq
	| Add
	| LNot

	| Jmp Int | CJmp Int

	| Push Int
	| Dup | Drop | Swap | Over
	-- | Over2


addInstruction :: Monad m => I -> AsmT Int I m ()
addInstruction = flip instruction 1

$(instructionSet "addInstruction" "Int" "I" "" False)


sortArray :: Asm Int I ()
sortArray = mdo
	push 0 --first cell contains number of elements
	ld
	push (-1)
	add
	push 0
	forLoopMacro $ mdo

		dup --get index value of outer loop
		push 1 --convert it to adress to data array
		add
		scratchpadAddress
		st


		push 0 --first cell contains number of elements
		ld
		over --get index value of outer loop one more time
		push 1
		add
		forLoopMacro $ mdo
			dup --get current loop index
			push 1 --convert it to adress to data array
			add
			ld --load value

			scratchpadAddress --load other value
			ld
			ld

			lt
			lnot -- ??
			cjmp skip

			dup --current loop index is new value
			scratchpadAddress
			st

			skip <- label
			return ()

		dup --get index value of outer loop
		scratchpadAddress
		ld
		eq
		cjmp don'tSwap
		--TODO do swap

		don'tSwap <- label
		return ()

	where
	scratchpadAddress = do
		push 0 --compute address of first free cell after data array
		ld
		push 1
		add
		st


-- ... final initial --
forLoopMacro :: Asm Int I () -> Asm Int I ()
forLoopMacro body = mdo
	start <- label
	over --clone final...
	over -- and initial value
	gt --check if we are at end of loop
	cjmp end --if yes, skip to end
	push 1 --increment initial value
	add
	body --run loop body
	jmp start --next iteration
	end <- label
	drop --discard init and final values
	drop



--XXX fuck it, it is too long, just do sum of array and something with forward jump

eval :: [I] -> ([Int], [Int]) -> ([Int], [Int])
eval = undefined

main :: IO ()
main = do

	let testData = [ 4, 1000, -10, 256, 500, 0 ]


	print $ eval (assemble sortArray) (testData, [])

	print 1
