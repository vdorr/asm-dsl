{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE TemplateHaskell, RecursiveDo #-}

module Language.Asm.Example.Tiny where

import Prelude hiding (not, drop)

import Language.Asm
import Language.Asm.TH

data I
	= Ld
	| St

	| Gt | GtE | Lt
	| Add
	| LNot

	| Jmp Int | CJmp Int

	| Push Int
	| Dup | Drop | Swap | Over | Over2


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
	forLoopMacro $ do

		push 0 --first cell contains number of elements
		ld
		over --get index value of outer loop
		push 1
		add
		forLoopMacro $ do
			undefined


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





main :: IO ()
main = do
	print 1
