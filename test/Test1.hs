

module Main where

import Language.Asm.Example.Tiny
import System.Exit (exitFailure)

main :: IO ()
main = do
        let testData = [ 4, 1000, -10, 256, 500, 0 ]
        let result@(m, _, _) = eval sumArrayProg (testData, 0, [])
	print result
        if last m /= sum (tail (init m))
                then exitFailure
                else return ()
