-- |
-- Module: TH
-- Description: Common functions.
--
-- Embedded assembler helper generation

module Language.Asm.TH ( instructionSet ) where

import Language.Haskell.TH
import Control.Monad
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

makeInstr :: String -> String -> Name -> Name -> Maybe Name -> Name -> Maybe Name -> [Type] -> Q [Dec]
makeInstr suffix callName addrType opType annType n retValType typeList = let
	ari = length typeList
	fName = mkName $ map toLower (nameBase n) ++ suffix
	argNames = map (mkName . ("v" ++) . show) [1..ari]
	args = map varP argNames

	body = foldl (\e arg -> appE e (varE arg)) (conE n) argNames

	body' = appE (varE (mkName callName)) body

	mVar = mkName "m" --variable representing underlying Monad
	retValType' = fromMaybe (tupleT 0) (fmap conT retValType)
	annType' = fromMaybe (tupleT 0) (fmap conT annType)

	signature = foldr (\ a b -> appT (appT arrowT (return a)) b) retType typeList

	retType = actionType addrType opType annType' retValType'

	in do
		sig <- sigD fName $ forallT
			[PlainTV mVar] --forall m.
			(cxt
				[ simpleClassP "Monad" mVar
				] )
			signature

		fun <- funD fName [ clause args (normalB body') [] ]
		return [sig, fun]

simpleClassP :: String -> Name -> Q Type
simpleClassP conName varName = (appT
	(conT (mkName conName))
	(varT varName))

-- | Generate type of putter action 'AsmT addrType opType annotation resultType'
actionType :: Name -> Name -> TypeQ -> TypeQ -> TypeQ
actionType addrType opType annType resultType
	= appT
		(appT
			(appT
				(appT (appT
					(conT (mkName "AsmT"))
					(conT addrType))
					(conT opType))
--					(conT annType))
					annType)
					(varT (mkName "m")))
					resultType

makeInstrMonadicArgs :: String -> String -> Name -> Name -> Maybe Name -> Name -> Maybe Name -> [Type] -> Q [Dec]
makeInstrMonadicArgs suffix callName addrType opType annType n retValType typeList = let
	ari = length typeList
	fName = mkName $ map toLower (nameBase n) ++ suffix
	argNames = map (mkName . ("v" ++) . show) [1..ari]
	args = map varP argNames

	body = case argNames of
		[] -> appE (varE (mkName "return")) (conE n)
		firstArg:rest -> foldl
			(\e arg -> appE (appE (varE (mkName "<*>")) e) (varE arg))
			(appE (appE (varE $ mkName "fmap") (conE n)) (varE firstArg))
			rest

	body' = appE (varE (mkName callName)) body

	signature = foldr (\ a b -> appT (appT arrowT ( actionType addrType opType annType' (return a))) b) retType typeList

	retValType' = fromMaybe (tupleT 0) (fmap conT retValType)
	retType = actionType addrType opType annType' retValType'
	annType' = fromMaybe (tupleT 0) (fmap conT annType)

	in do
		sig <- sigD fName $ forallT
			[PlainTV (mkName "m")]
			(cxt
				[ simpleClassP "Monad" (mkName "m")
				, simpleClassP "Functor" (mkName "m") ])
			signature

		fun <- funD fName [ clause args (normalB body') [] ]
		return [sig, fun]

-- | Generate helper functions for embedded assembler
instructionSet
	:: String -- ^ name of function to call in instruction value, typically 'Language.Asm.instruction'
	-> String -- ^ name of type of memory address, addr parameter of 'Language.Asm.AsmT' type
	-> String -- ^ name of instruction set type
	-> Maybe String -- ^ name of annotation type
	-> String -- ^ suffix added to each helper name
	-> Maybe String -- ^ name of return value of each putter or nothing for '()'
	-> Bool -- ^ make arguments of instruction monadic
	-> Q [Dec]
instructionSet callName addrTyName tyName annTyName suffix retValTyName monadicArgs = do
	let addrType = mkName addrTyName
	let mkI = if monadicArgs
		then makeInstrMonadicArgs suffix callName addrType
		else makeInstr suffix callName addrType
	Just t <- lookupTypeName tyName

	tyRetVal <- case retValTyName of
		Nothing -> return Nothing
		Just ty -> lookupTypeName ty

	TyConI (DataD _ _ _ _ con _) <- reify t
	fmap concat $ forM con $ \c -> case c of
		NormalC n bangTypeList -> mkI t (fmap mkName annTyName) n tyRetVal (map snd bangTypeList)
		RecC n varBangTypeList -> mkI n (fmap mkName annTyName) t tyRetVal (map (\(_, _, tp) -> tp) varBangTypeList)
		_ -> error "AsmTH.instructionSet" --FIXME elaborate

