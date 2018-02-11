{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE CPP, TemplateHaskell #-}

module Language.Asm.TH ( instructionSet ) where

import Language.Haskell.TH
import Control.Monad
import Data.Char (toLower)

{-

runghc -ddump-splices AsmTest.hs

-}

{- TODO
instruction set extensions
data Ext1 = EOp_1_1 | EOp_1_2
data Ext2 = EOp_2_1 | EOp_2_2
data I = IAdd | ISub | IExt1 Ext1 | IExt2 Ext2
--or
data BasicI = IAdd | ISub
data I = Basic BasicI | IExt1 Ext1 | IExt2 Ext2
-}

makeInstr :: String -> String -> Name -> Name -> Name -> [Type] -> Q [Dec]
makeInstr suffix callName addrType opType n typeList = let
	ari = length typeList
	fName = mkName $ map toLower (nameBase n) ++ suffix
	argNames = map (mkName . ("v" ++) . show) [1..ari]
	args = map varP argNames

	body = foldl (\e arg -> appE e (varE arg)) (conE n) argNames

	body' = appE (varE (mkName callName)) body

	mVar = mkName "m"

	signature = foldr (\ a b -> appT (appT arrowT (return a)) b) retType typeList

	retType = actionType addrType opType (tupleT 0)
	in do
		sig <- sigD fName $ forallT
			[PlainTV mVar]
			(cxt [
				simpleClassP "Monad" mVar
				] )
			signature

		fun <- funD fName [ clause args (normalB body') [] ]
		return [sig, fun]


simpleClassP :: String -> Name -> Q Type
simpleClassP conName varName = (appT
	(conT (mkName conName))
	(varT varName))


actionType :: Name -> Name -> TypeQ -> TypeQ
actionType addrType opType resultType
	= appT (appT
		(appT
			(appT
				(conT (mkName "AsmT"))
				(conT addrType))
			(conT opType))
		(varT (mkName "m")))
		resultType




makeInstrMonadicArgs :: String -> String -> Name -> Name -> Name -> [Type] -> Q [Dec]
makeInstrMonadicArgs suffix callName addrType opType n typeList = let
	ari = length typeList
	fName = mkName $ map toLower (nameBase n) ++ suffix
	argNames = map (mkName . ("v" ++) . show) [1..ari]
	args = map varP argNames



--	body = foldl (\e arg -> appE e (varE arg)) (conE n) argNames
	body = case argNames of
		[] -> appE (varE (mkName "return")) (conE n)
		firstArg:rest -> foldl
			(\e arg -> appE (appE (varE (mkName "<*>")) e) (varE arg))
			(appE (appE (varE $ mkName "fmap") (conE n)) (varE firstArg))
			rest


	body' = appE (varE (mkName callName)) body


	signature = foldr (\ a b -> appT (appT arrowT ( actionType addrType opType(return a))) b) retType typeList

	retType = actionType addrType opType (tupleT 0)
	in do
		sig <- sigD fName $ forallT
			[PlainTV (mkName "m")]
			(cxt
				[ simpleClassP "Monad" (mkName "m")
				, simpleClassP "Functor" (mkName "m") ])
			signature

		fun <- funD fName [ clause args (normalB body') [] ]
		return [sig, fun]



instructionSet :: String -> String -> String -> String -> Bool -> Q [Dec]
instructionSet callName addrTyName tyName suffix monadicArgs = do
	let addrType = mkName addrTyName
	Just t <- lookupTypeName tyName
	let mkI = if monadicArgs
		then makeInstrMonadicArgs suffix callName addrType
		else makeInstr suffix callName addrType
	TyConI (DataD _ _ _ _ con _) <- reify t
	fmap concat $ forM con $ \c -> case c of
		NormalC n bangTypeList -> mkI t n (map snd bangTypeList)
		RecC n varBangTypeList -> mkI n t (map (\(_, _, t) -> t) varBangTypeList)
		_ -> error "AsmTH.instructionSet" --FIXME elaborate
