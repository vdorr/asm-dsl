

module Main where

import Language.Asm
import Language.Asm.Example.Tiny hiding (main)

main = do
        print "this was hard"

        let testData = [ 4, 1000, -10, 256, 500, 0 ]
	print $ eval (assemble example) (testData, 0, [])
