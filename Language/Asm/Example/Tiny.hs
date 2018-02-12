{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE TemplateHaskell, RecursiveDo #-}

module Language.Asm.Example.Tiny where

import Prelude hiding (not, drop)

import Language.Asm
import Language.Asm.TH

data I
	= Ld -- address -- value
	| St -- value address --

	| Gt | GtE | Lt | LtE | Eq
	| Add
	| Not

	| Jmp Int -- absolute jump
	| CJmp Int -- conditional absolute jump

	| Push Int
	| Dup | Drop | Swap | Over
	deriving (Show)


addInstruction :: Monad m => I -> AsmT Int I m ()
addInstruction = flip instruction 1

$(instructionSet "addInstruction" "Int" "I" "" False)


example :: Asm Int I ()
example = mdo
	push 0
	ld
	push 1
	add

	begin <- label
	dup
	push 1
	lte
	cjmp end

	dup
	ld

	push (-1)
	add

	jmp begin
	end <- label
	push 0
	ld
	push 1
	add
	st

eval :: [I] -> ([Int], Int, [Int]) -> ([Int], Int, [Int])
eval p st' = let
	end = length p
	f k p st@(m, i, d)
		| i >= k = st
		| otherwise = f k p (evalOne (p !! i) (m, i + 1, d))
	in f end p st'

evalOne :: I -> ([Int], Int, [Int]) -> ([Int], Int, [Int])
evalOne (Push k) (m, i, d) = (m, i, k : d)
evalOne Ld (m, i, a:d) = (m, i, (m !! a) : d)
evalOne St (m, i, a:v:d) = let (l, _:u) = splitAt a m in (l ++ v : u, i, d)
evalOne LtE (m, i, y:x:d) = (m, i, (if x >= y then 1 else 0) : d)
evalOne Add (m, i, y:x:d) = (m, i, (x + y) : d)
evalOne (CJmp a) (m, i, x:d) = (m, if x == 0 then i else a, d)
evalOne (Jmp a) (m, _, d) = (m, a, d)
evalOne i st = error ("evalOne " ++ show (i, st))

main :: IO ()
main = do

	let testData = [ 4, 1000, -10, 256, 500, 0 ]


	print $ eval (assemble example) (testData, 0, [])

	print 1
