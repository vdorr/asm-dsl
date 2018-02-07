
module Language.Asm.Example.Tiny where

import Language.Asm
import Language.Asm.TH

data I
	= Ld Int
	| St Int
	| Cmp
	| Br Int

bubbleSort = undefined

main :: IO ()
main = do
	print 1

