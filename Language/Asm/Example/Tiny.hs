{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE CPP, TemplateHaskell, RecursiveDo, UnicodeSyntax #-}

module Language.Asm.Example.Tiny where

import Language.Asm
import Language.Asm.TH

data I
	= Ld Int
	| St Int
	| Cmp
	| Br Int

--addInstruction :: I -> Asm Int I ()
addInstruction = undefined

$(instructionSet "addInstruction" "Int" "I" "" False)

bubbleSort = undefined

main :: IO ()
main = do
	print 1

