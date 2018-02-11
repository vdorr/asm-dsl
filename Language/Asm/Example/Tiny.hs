{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE TemplateHaskell, RecursiveDo #-}

module Language.Asm.Example.Tiny where

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
	| Dup | Drop | Swap | Over


addInstruction :: Monad m => I -> AsmT Int I m ()
addInstruction = flip instruction 1

$(instructionSet "addInstruction" "Int" "I" "" False)


sortArray :: Asm Int I ()
sortArray = mdo
	push 0 --first cell contains number of elements
	ld
	dup --check if we have more than 1 cell
	push 1
	gte
	lnot --if not then branch to the end
	cjmp end

	loop <- label

	end <- label
	return ()

-- ... final initial --
forLoop body = mdo
	start <- label
	-- initVal --push initial and final value on stack
	-- endVal
	over; over --clone init and final value
	gt
	cjmp end --end of loop, goto end
	push 1; add --increment initial value
	body
	jmp start
	end <- label
	drop; drop --discard init and final values
	return ()





main :: IO ()
main = do
	print 1
