{-# LANGUAGE RecursiveDo #-}

module Main where

import Language.Asm.Example.Tiny
import Language.Asm

import Data.Functor.Identity
import System.Exit (exitFailure)

invalidJump :: Asm Int I () ()
invalidJump = do
	jmp 10

invalidJumpProg :: [I]
invalidJumpProg = runIdentity $ assembleT_ invalidJump

jumpForwardProg :: [I]
jumpForwardProg = snd $ assemble $ mdo
	pushnum 1
	jmp end
	pushnum 2
	end <- label
	pushnum 1

--jumpForwardProg :: [I]
--jumpForwardProg = assemble_ $ mdo
--	push 1
--	cjmp end
--	push 2
--	end <- label
--	push 1

--FIXME make proper tests
main :: IO ()
main = do
        let testArray = [ 1000, -10, 256, 500 ]
        let testData = fmap Num (length testArray : testArray ++ [0])
	let initState = (testData, 0, [])

	print sumArrayProg

	print "test 1"
        let result@(m0, _, _) = eval sumArrayProg initState
	print result
	let m = fmap (\(Num x) -> x) m0
        if last m /= sum (tail (init m))
                then do
			print (last m, sum (tail (init m)))
			exitFailure
                else return ()

	print "test 2"
	case weaveI sumArrayProg of
		Left err -> error $ "error weaving: " ++ err
		Right p -> do
			let result'@(m0', _, _) = runVM p initState
			print result'
			let m' = fmap (\(Num x) -> x) m0'
			if last m' /= sum (tail (init m'))
				then exitFailure
				else return ()

	print "test 3"
	case weaveI invalidJumpProg of
		Left err -> putStrLn $ "correctly found error: " ++ err
		Right _ -> do
			putStrLn "expected failure"
			exitFailure

	print "test 4"
	case weaveI jumpForwardProg of
		Left err -> do
			putStrLn $ "failed jump forwardr: " ++ err
			exitFailure
		Right p -> do
--			print "failed jump forward"
			let result' = runVM p initState
			print result'
--			return ()

	print "test 5"
	let (prog5, mem5) = sumArrayProg' testArray
	case weaveI prog5 of
		Left err -> error $ "error weaving: " ++ err
		Right p -> do
			let result'@(m0', _, _) = runVM p (mem5, 0, [])
			print ("test 5", result')
			let m' = fmap (\(Num x) -> x) m0'
			if last m' /= sum (tail (init m'))
				then exitFailure
				else return ()


 
