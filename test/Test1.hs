
module Main where

import Language.Asm.Example.Tiny

import System.Exit (exitFailure)

--FIXME make proper tests
main :: IO ()
main = do
        let testData = [ 4, 1000, -10, 256, 500, 0 ]
	let initState = (testData, 0, [])
        let result@(m, _, _) = eval sumArrayProg initState
	print result
        if last m /= sum (tail (init m))
                then exitFailure
                else return ()
	case weaveI sumArrayProg of
		Left err -> error $ "error weaving: " ++ err
		Right p -> do
			let result'@(m', _, _) = runVM p initState
			print result'
			if last m' /= sum (tail (init m'))
				then exitFailure
				else return ()

