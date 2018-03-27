{-# LANGUAGE RecursiveDo #-}

module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

import Language.Asm.Example.Tiny

testArray :: [Int]
testArray = [ 1000, -10, 256, 500 ]

testData :: [Cell]
testData = fmap Num (length testArray : testArray ++ [0])

initState :: VM
initState = (testData, 0, [])

test1 :: Assertion
test1 = let
	(m0, _, _) = eval sumArrayProg initState
	m = fmap (\(Num x) -> x) m0
        	in assertBool
			(show (last m, sum (tail (init m))))
			(last m == sum (tail (init m)))

test5 :: Assertion
test5 = let
	(prog5, mem5) = sumArrayProg' testArray
	_result'@(m0', _, _) = eval prog5 (mem5, 0, [])
--	print ("test 5", result')
	m' = fmap (\(Num x) -> x) m0'
	in assertBool "failure" (last m' == sum (tail (init m')))

main :: IO ()
main = defaultMain [ testGroup "g1"
	[ testCase "t1" test1
	, testCase "t5" test5
	] ]

