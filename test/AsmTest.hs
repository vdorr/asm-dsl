{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE CPP, TemplateHaskell, RecursiveDo, UnicodeSyntax #-}

import Mem hiding (main)
import Asm hiding (main)
import AsmTH

import Data.Word
import Control.Applicative

--------------------------------------------------------------------------------

--data RTL
--	= A
--	| D
--	|

{-
[
	[ ld in1, and in2 ] ==> [out1, not out2],
	[ ld in3 ] ==> [out3]
]
-}

data MC14500BInstruction u
	= NOPO -- No change in registers. RR -> RR, Flag O -> <pulse>
	| LD -- Load result register. Data -> RR
	| LDC -- Load complement. /Data -> RR
	| AND -- Logical AND. RR & Data -> RR
	| ANDC -- Logical AND complement. RR & /Data -> RR
	| OR -- Logical OR. RR + Data -> RR
	| ORC -- Logical OR complement. RR + /Data -> RR
	| XNOR -- Exclusive NOR. If RR = Data, RR -> 1
	| STO -- Store. RR -> Data Pin, Write -> <pulse>
	| STOC -- Store complement. RR -> Data Pin, Write  -> <pulse>
	| IEN -- Input enable. Data -> IEN Register
	| OEN -- Output enable. Data -> OEN Register
	| JMP -- Jump. JMP Flag  -> <pulse>
	| RTN -- Return. RTN Flag  -> <pulse> and skip next instruction
	| SKZ -- Skip next instruction if RR = 0
	| NOPF -- No change in registers. RR RR, Flag F  -> <pulse>
	deriving Show

(¬) = not

q a = (¬) a

--------------------------------------------------------------------------------

data Cell
	= CWord Word32
	| CDWord Word64
	deriving Show

data Op
	= CJmp Word32
	| Push Word32
	| Ld
	| St
	| Dec
	| Interrupt Word32
	| CodeMemoryData [Cell] --TODO make memory return grand total
	| TestTest Word Bool
	deriving Show

#if 0

addInstruction k = instruction k 1

--generate helpers for our "assembly language"
$(instructionSet "addInstruction" "Word32" "Op" False)

--------------------------------------------------------------------------------

instructionLength :: Op -> Word32
instructionLength _i = 1

op k = instruction k (instructionLength k)

type Assembly = Asm Word32 Op ()

type Program = ([Cell], [Op])

dword = cell 8 . CDWord
word = cell 4 . CWord

--example using $(instructionSet "Op") helpers
test3 :: Program
test3 = memory 0 $ do
	en <- word 1
	eno <- word 1
	cnt <- dword 0
	return $ assemble $ mdo
		begin <- label
		push cnt
		ld
		dec
		st
		cjmp begin
		end <- label
		cjmp begin


test2 :: Assembly
test2 = mdo {
	begin <- label;
		cjmp end;
		cjmp begin;
	end <- label;
		cjmp begin;
}


test = do
	op $ Push 10
	loop <- label
	op Dec
	op $ CJmp loop

#else


addInstruction k = do
	k' <- k
	instruction k' 1

--generate helpers for our "assembly language"
$(instructionSet "addInstruction" "Word32" "Op" "" True)

test = do
	push (pure 10)
	loop <- label
	dec
	cjmp (pure loop)

#endif

main = do
	print $ show $ assemble test
--	print $ show $ test3

