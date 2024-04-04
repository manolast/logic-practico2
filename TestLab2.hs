{-# OPTIONS_GHC -fno-warn-tabs #-}
module TestLab2 where

import Lab2        -- Nombre del archivo .hs de la solución
import Test.HUnit  -- Acerca de HUnit: https://hackage.haskell.org/package/HUnit
import Data.List

-- Para ejecutar todas las pruebas usar: runTestTT allTests 

raz1True_eval_test = TestCase (assertEqual "eval raz1True" True (eval (\x -> True) raz1))
raz1False_eval_test = TestCase (assertEqual "eval raz1False" False (eval (\x -> False) raz1))
raz2True_eval_test = TestCase (assertEqual "eval raz2True" True (eval (\x -> True) raz2))
raz2False_eval_test = TestCase (assertEqual "eval raz2False" True (eval (\x -> False) raz2))
raz3True_eval_test = TestCase (assertEqual "eval raz3True" True (eval (\x -> True) raz3))
raz3False_eval_test = TestCase (assertEqual "eval raz3False" True (eval (\x -> False) raz3))
raz4True_eval_test = TestCase (assertEqual "eval raz4True" True (eval (\x -> True) raz4))
raz4False_eval_test = TestCase (assertEqual "eval raz4False" True (eval (\x -> False) raz4))
raz5True_eval_test = TestCase (assertEqual "eval raz5True" True (eval (\x -> True) raz5))
raz5False_eval_test = TestCase (assertEqual "eval raz5False" True (eval (\x -> False) raz5))
raz6True_eval_test = TestCase (assertEqual "eval raz6True" True (eval (\x -> True) raz6))
raz6False_eval_test = TestCase (assertEqual "eval raz6False" False (eval (\x -> False) raz6))
raz7True_eval_test = TestCase (assertEqual "eval raz7True" False (eval (\x -> True) raz7))
raz7False_eval_test = TestCase (assertEqual "eval raz7False" False (eval (\x -> False) raz7))
raz8True_eval_test = TestCase (assertEqual "eval raz8True" False (eval (\x -> True) raz8))
raz8False_eval_test = TestCase (assertEqual "eval raz8False" False (eval (\x -> False) raz8))
raz9True_eval_test = TestCase (assertEqual "eval raz9True" False (eval (\x -> True) raz9))
raz9False_eval_test = TestCase (assertEqual "eval raz9False" False (eval (\x -> False) raz9))
raz10True_eval_test = TestCase (assertEqual "eval raz10True" True (eval (\x -> True) raz10))
raz10False_eval_test = TestCase (assertEqual "eval raz10False" False (eval (\x -> False) raz10))


raz1Intp1_creari_test = TestCase (assertEqual "creari raz1" False (eval (creari interp1) raz1))
raz2Intp1_creari_test = TestCase (assertEqual "creari raz2 interp1" True (eval (creari interp1) raz2))
raz2Intp2_creari_test = TestCase (assertEqual "creari raz2 interp2" True (eval (creari interp2) raz2))
raz2Intp3_creari_test = TestCase (assertEqual "creari raz2 interp3" True (eval (creari interp3) raz2))
raz2Intp6_creari_test = TestCase (assertEqual "creari raz2 interp6" False (eval (creari interp6) raz2))
raz3Intp1_creari_test = TestCase (assertEqual "creari raz3 interp1" True (eval (creari interp1) raz3))
raz3Intp2_creari_test = TestCase (assertEqual "creari raz3 interp2" False (eval (creari interp2) raz3))
raz3Intp3_creari_test = TestCase (assertEqual "creari raz3 interp3" True (eval (creari interp3) raz3))
raz3Intp4_creari_test = TestCase (assertEqual "creari raz3 interp4" False (eval (creari interp4) raz3))
raz3Intp5_creari_test = TestCase (assertEqual "creari raz3 interp5" True (eval (creari interp5) raz3))
raz3Intp6_creari_test = TestCase (assertEqual "creari raz3 interp6" True (eval (creari interp6) raz3))
raz3Intp7_creari_test = TestCase (assertEqual "creari raz3 interp7" True (eval (creari interp7) raz3))
raz3Intp8_creari_test = TestCase (assertEqual "creari raz3 interp8" True (eval (creari interp8) raz3))
raz4Intp1_creari_test = TestCase (assertEqual "creari raz4 interp1" True (eval (creari interp1) raz4))
raz4Intp2_creari_test = TestCase (assertEqual "creari raz4 interp2" True (eval (creari interp2) raz4))
raz4Intp3_creari_test = TestCase (assertEqual "creari raz4 interp3" True (eval (creari interp3) raz4))
raz4Intp4_creari_test = TestCase (assertEqual "creari raz4 interp4" False (eval (creari interp4) raz4))
raz4Intp5_creari_test = TestCase (assertEqual "creari raz4 interp5" True (eval (creari interp5) raz4))
raz4Intp6_creari_test = TestCase (assertEqual "creari raz4 interp6" True (eval (creari interp6) raz4))
raz4Intp7_creari_test = TestCase (assertEqual "creari raz4 interp7" True (eval (creari interp7) raz4))
raz4Intp8_creari_test = TestCase (assertEqual "creari raz4 interp8" True (eval (creari interp8) raz4))
raz5Intp1_creari_test = TestCase (assertEqual "creari raz5 interp1" True (eval (creari interp1) raz5))
raz5Intp2_creari_test = TestCase (assertEqual "creari raz5 interp2" True (eval (creari interp2) raz5))
raz5Intp3_creari_test = TestCase (assertEqual "creari raz5 interp3" True (eval (creari interp3) raz5))
raz5Intp4_creari_test = TestCase (assertEqual "creari raz4 interp4" True (eval (creari interp4) raz5))
raz5Intp5_creari_test = TestCase (assertEqual "creari raz5 interp5" True (eval (creari interp5) raz5))
raz5Intp6_creari_test = TestCase (assertEqual "creari raz5 interp6" True (eval (creari interp6) raz5))
raz5Intp7_creari_test = TestCase (assertEqual "creari raz5 interp7" True (eval (creari interp7) raz5))
raz5Intp8_creari_test = TestCase (assertEqual "creari raz5 interp8" True (eval (creari interp8) raz5))
raz6Intp1_creari_test = TestCase (assertEqual "creari raz6 interp1" False (eval (creari interp1) raz6))
raz6Intp2_creari_test = TestCase (assertEqual "creari raz6 interp2" True (eval (creari interp2) raz6))
raz6Intp3_creari_test = TestCase (assertEqual "creari raz6 interp3" False (eval (creari interp3) raz6))
raz6Intp4_creari_test = TestCase (assertEqual "creari raz6 interp4" True (eval (creari interp4) raz6))
raz6Intp5_creari_test = TestCase (assertEqual "creari raz6 interp5" False (eval (creari interp5) raz6))
raz6Intp6_creari_test = TestCase (assertEqual "creari raz6 interp6" True (eval (creari interp6) raz6))
raz6Intp7_creari_test = TestCase (assertEqual "creari raz6 interp7" False (eval (creari interp7) raz6))
raz6Intp8_creari_test = TestCase (assertEqual "creari raz6 interp8" True (eval (creari interp8) raz6))
raz9Intp1_creari_test = TestCase (assertEqual "creari raz9 interp1" False (eval (creari interp1) raz9))
raz9Intp2_creari_test = TestCase (assertEqual "creari raz9 interp2" True (eval (creari interp2) raz9))
raz9Intp3_creari_test = TestCase (assertEqual "creari raz9 interp3" True (eval (creari interp3) raz9))
raz9Intp4_creari_test = TestCase (assertEqual "creari raz9 interp4" False (eval (creari interp4) raz9))
raz9Intp5_creari_test = TestCase (assertEqual "creari raz9 interp5" False (eval (creari interp5) raz9))
raz9Intp6_creari_test = TestCase (assertEqual "creari raz9 interp6" True (eval (creari interp6) raz9))
raz9Intp7_creari_test = TestCase (assertEqual "creari raz9 interp7" True (eval (creari interp7) raz9))
raz9Intp8_creari_test = TestCase (assertEqual "creari raz9 interp8" False (eval (creari interp8) raz9))


p_filas_test = TestCase (assertBool "filas p" $ sameList [[("p",True)],[("p",False)]] (filas ["p"]))
filapq_filas_test = TestCase (assertBool "filas pq" $ sameList filapq (filas ["p","q"]))
filapqr_filas_test = TestCase (assertBool "filas pqr" $ sameList filapqr (filas ["p","q","r"]))
filapqrs_filas_test = TestCase (assertBool "filas pqrs" $ sameList filapqrs (filas ["p","q","r","s"]))
filapqrst_filas_test = TestCase (assertBool "filas pqrst" $ sameList filapqrst (filas ["p","q","r","s","t"]))


raz1_tv_test = TestCase (assertBool "tv raz1" $ sameList tvRaz1 (tv raz1))
raz2_tv_test = TestCase (assertBool "tv raz2" $ sameList tvRaz2 (tv raz2))
raz3_tv_test = TestCase (assertBool "tv raz3" $ sameList tvRaz3 (tv raz3))
raz4_tv_test = TestCase (assertBool "tv raz4" $ sameList tvRaz4 (tv raz4))
raz5_tv_test = TestCase (assertBool "tv raz5" $ sameList tvRaz5 (tv raz5))
raz6_tv_test = TestCase (assertBool "tv raz6" $ sameList tvRaz6 (tv raz6))
raz7_tv_test = TestCase (assertBool "tv raz7" $ sameList tvRaz7 (tv raz7))
raz8_tv_test = TestCase (assertBool "tv raz8" $ sameList tvRaz8 (tv raz8))
raz9_tv_test = TestCase (assertBool "tv raz9" $ sameList tvRaz9 (tv raz9))
raz10_tv_test = TestCase (assertBool "tv raz10" $ sameList tvRaz10 (tv raz10))

raz1Tau_es_test = TestCase (assertEqual "es raz1 Tau" False (es raz1 Tau))
raz1Contra_es_test = TestCase (assertEqual "es raz1 Contra" False (es raz1 Contra))
raz1Cont_es_test = TestCase (assertEqual "es raz1 Cont" True (es raz1 Cont))
raz1Sat_es_test = TestCase (assertEqual "es raz1 Sat" True (es raz1 Sat))
raz1Fal_es_test = TestCase (assertEqual "es raz1 Fal" True (es raz1 Fal))
raz2Tau_es_test = TestCase (assertEqual "es raz2 Tau" False (es raz2 Tau))
raz2Contra_es_test = TestCase (assertEqual "es raz2 Contra" False (es raz2 Contra))
raz2Cont_es_test = TestCase (assertEqual "es raz2 Cont" True (es raz2 Cont))
raz2Sat_es_test = TestCase (assertEqual "es raz2 Sat" True (es raz2 Sat))
raz2Fal_es_test = TestCase (assertEqual "es raz2 Fal" True (es raz2 Fal))
raz3Tau_es_test = TestCase (assertEqual "es raz3 Tau" False (es raz3 Tau))
raz3Contra_es_test = TestCase (assertEqual "es raz3 Contra" False (es raz3 Contra))
raz3Cont_es_test = TestCase (assertEqual "es raz3 Cont" True (es raz3 Cont))
raz3Sat_es_test = TestCase (assertEqual "es raz3 Sat" True (es raz3 Sat))
raz3Fal_es_test = TestCase (assertEqual "es raz3 Fal" True (es raz3 Fal))
raz4Tau_es_test = TestCase (assertEqual "es raz4 Tau" False (es raz4 Tau))
raz4Contra_es_test = TestCase (assertEqual "es raz4 Contra" False (es raz4 Contra))
raz4Cont_es_test = TestCase (assertEqual "es raz4 Cont" True (es raz4 Cont))
raz4Sat_es_test = TestCase (assertEqual "es raz4 Sat" True (es raz4 Sat))
raz4Fal_es_test = TestCase (assertEqual "es raz4 Fal" True (es raz4 Fal))
raz5Tau_es_test = TestCase (assertEqual "es raz5 Tau" True (es raz5 Tau))
raz5Contra_es_test = TestCase (assertEqual "es raz5 Contra" False (es raz5 Contra))
raz5Cont_es_test = TestCase (assertEqual "es raz5 Cont" False (es raz5 Cont))
raz5Sat_es_test = TestCase (assertEqual "es raz5 Sat" True (es raz5 Sat))
raz5Fal_es_test = TestCase (assertEqual "es raz5 Fal" False (es raz5 Fal))
raz6Tau_es_test = TestCase (assertEqual "es raz6 Tau" False (es raz6 Tau))
raz6Contra_es_test = TestCase (assertEqual "es raz6 Contra" False (es raz6 Contra))
raz6Cont_es_test = TestCase (assertEqual "es raz6 Cont" True (es raz6 Cont))
raz6Sat_es_test = TestCase (assertEqual "es raz6 Sat" True (es raz6 Sat))
raz6Fal_es_test = TestCase (assertEqual "es raz6 Fal" True (es raz6 Fal))
raz7Tau_es_test = TestCase (assertEqual "es raz7 Tau" False (es raz7 Tau))
raz7Contra_es_test = TestCase (assertEqual "es raz7 Contra" False (es raz7 Contra))
raz7Cont_es_test = TestCase (assertEqual "es raz7 Cont" True (es raz7 Cont))
raz7Sat_es_test = TestCase (assertEqual "es raz7 Sat" True (es raz7 Sat))
raz7Fal_es_test = TestCase (assertEqual "es raz7 Fal" True (es raz7 Fal))
raz8Tau_es_test = TestCase (assertEqual "es raz8 Tau" False (es raz8 Tau))
raz8Contra_es_test = TestCase (assertEqual "es raz8 Contra" True (es raz8 Contra))
raz8Cont_es_test = TestCase (assertEqual "es raz8 Cont" False (es raz8 Cont))
raz8Sat_es_test = TestCase (assertEqual "es raz8 Sat" False (es raz8 Sat))
raz8Fal_es_test = TestCase (assertEqual "es raz8 Fal" True (es raz8 Fal))
raz9Tau_es_test = TestCase (assertEqual "es raz9 Tau" False (es raz9 Tau))
raz9Contra_es_test = TestCase (assertEqual "es raz9 Contra" False (es raz9 Contra))
raz9Cont_es_test = TestCase (assertEqual "es raz9 Cont" True (es raz9 Cont))
raz9Sat_es_test = TestCase (assertEqual "es raz9 Sat" True (es raz9 Sat))
raz9Fal_es_test = TestCase (assertEqual "es raz9 Fal" True (es raz9 Fal))
raz10Tau_es_test = TestCase (assertEqual "es raz10 Tau" False (es raz10 Tau))
raz10Contra_es_test = TestCase (assertEqual "es raz10 Contra" False (es raz10 Contra))
raz10Cont_es_test = TestCase (assertEqual "es raz10 Cont" True (es raz10 Cont))
raz10Sat_es_test = TestCase (assertEqual "es raz10 Sat" True (es raz10 Sat))
raz10Fal_es_test = TestCase (assertEqual "es raz10 Fal" True (es raz10 Fal))

f1_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f1) f1"  True (isFNCof (fnc f1) f1))
f2_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f2) f2"  True (isFNCof (fnc f2) f2))
f3_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f3) f3"  True (isFNCof (fnc f3) f3))
f4_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f4) f4"  True (isFNCof (fnc f4) f4))
f5_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f5) f5"  True (isFNCof (fnc f5) f5))
f6_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f6) f6"  True (isFNCof (fnc f6) f6))
f7_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f7) f7"  True (isFNCof (fnc f7) f7))
f8_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f8) f8"  True (isFNCof (fnc f8) f8))
f9_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f9) f9"  True (isFNCof (fnc f9) f9))
f10_fnc_ok = TestCase (assertEqual "isFNCof (fnc f10) f10" True (isFNCof (fnc f10) f10))
f11_fnc_ok = TestCase (assertEqual "isFNCof (fnc f11) f11" True (isFNCof (fnc f11) f11))
f12_fnc_ok = TestCase (assertEqual "isFNCof (fnc f12) f12" True (isFNCof (fnc f12) f12))

evalTests = [TestLabel "eval 1" raz1True_eval_test,
			TestLabel "eval 2" raz1False_eval_test,
			TestLabel "eval 3" raz2True_eval_test,
			TestLabel "eval 4" raz2False_eval_test,
			TestLabel "eval 5" raz3True_eval_test,
			TestLabel "eval 6" raz3False_eval_test,
			TestLabel "eval 7" raz4True_eval_test,
			TestLabel "eval 8" raz4False_eval_test,
			TestLabel "eval 9" raz5True_eval_test,
			TestLabel "eval 10" raz5False_eval_test,
			TestLabel "eval 11" raz6True_eval_test,
			TestLabel "eval 12" raz6False_eval_test,
			TestLabel "eval 13" raz7True_eval_test,
			TestLabel "eval 14" raz7False_eval_test,
			TestLabel "eval 15" raz8True_eval_test,
			TestLabel "eval 16" raz8False_eval_test,
			TestLabel "eval 17" raz9True_eval_test,
			TestLabel "eval 18" raz9False_eval_test,
			TestLabel "eval 19" raz10True_eval_test,
			TestLabel "eval 20" raz10False_eval_test];
			
creariTests = [TestLabel "creari 1" raz1Intp1_creari_test,
			TestLabel "creari 2" raz2Intp1_creari_test,
			TestLabel "creari 3" raz2Intp2_creari_test,
			TestLabel "creari 4" raz2Intp3_creari_test,
			TestLabel "creari 5" raz2Intp6_creari_test,	
			TestLabel "creari 6" raz3Intp1_creari_test,
			TestLabel "creari 7" raz3Intp2_creari_test,
			TestLabel "creari 8" raz3Intp3_creari_test,
			TestLabel "creari 9" raz3Intp4_creari_test,
			TestLabel "creari 10" raz3Intp5_creari_test,
			TestLabel "creari 11" raz3Intp6_creari_test,
			TestLabel "creari 12" raz3Intp7_creari_test,
			TestLabel "creari 13" raz3Intp8_creari_test,
			TestLabel "creari 14" raz4Intp1_creari_test,
			TestLabel "creari 15" raz4Intp2_creari_test,
			TestLabel "creari 16" raz4Intp3_creari_test,
			TestLabel "creari 17" raz4Intp4_creari_test,
			TestLabel "creari 18" raz4Intp5_creari_test,
			TestLabel "creari 19" raz4Intp6_creari_test,
			TestLabel "creari 20" raz4Intp7_creari_test,
			TestLabel "creari 21" raz4Intp8_creari_test,		
			TestLabel "creari 22" raz5Intp1_creari_test,
			TestLabel "creari 23" raz5Intp2_creari_test,
			TestLabel "creari 24" raz5Intp3_creari_test,
			TestLabel "creari 25" raz5Intp4_creari_test,
			TestLabel "creari 26" raz5Intp5_creari_test,
			TestLabel "creari 27" raz5Intp6_creari_test,
			TestLabel "creari 28" raz5Intp7_creari_test,
			TestLabel "creari 29" raz5Intp8_creari_test,		
			TestLabel "creari 30" raz6Intp1_creari_test,
			TestLabel "creari 31" raz6Intp2_creari_test,
			TestLabel "creari 32" raz6Intp3_creari_test,
			TestLabel "creari 33" raz6Intp4_creari_test,
			TestLabel "creari 34" raz6Intp5_creari_test,
			TestLabel "creari 35" raz6Intp6_creari_test,
			TestLabel "creari 36" raz6Intp7_creari_test,
			TestLabel "creari 37" raz6Intp8_creari_test,
			TestLabel "creari 38" raz9Intp1_creari_test,
			TestLabel "creari 39" raz9Intp2_creari_test,
			TestLabel "creari 40" raz9Intp3_creari_test,
			TestLabel "creari 41" raz9Intp4_creari_test,
			TestLabel "creari 42" raz9Intp5_creari_test,
			TestLabel "creari 43" raz9Intp6_creari_test,
			TestLabel "creari 44" raz9Intp7_creari_test,
			TestLabel "creari 45" raz9Intp8_creari_test];
			
filasTests = [TestLabel "filas 1" p_filas_test,
			TestLabel "filas 2" filapq_filas_test,
			TestLabel "filas 3" filapqr_filas_test,
			TestLabel "filas 4" filapqrs_filas_test,
			TestLabel "filas 5" filapqrst_filas_test];

tvTests = [TestLabel "tv 1" raz1_tv_test, 
			TestLabel "tv 2" raz2_tv_test, 
			TestLabel "tv 3" raz3_tv_test, 
			TestLabel "tv 4" raz4_tv_test, 
			TestLabel "tv 5" raz5_tv_test, 
			TestLabel "tv 6" raz6_tv_test, 
			TestLabel "tv 7" raz7_tv_test, 
			TestLabel "tv 8" raz8_tv_test,
			TestLabel "tv 9" raz9_tv_test,
			TestLabel "tv 10" raz10_tv_test];
			
esTests = [TestLabel "es 1" raz1Tau_es_test, 
			TestLabel "es 2" raz1Contra_es_test, 
			TestLabel "es 3" raz1Cont_es_test, 
			TestLabel "es 4" raz1Sat_es_test, 
			TestLabel "es 5" raz1Fal_es_test,			
			TestLabel "es 6" raz2Tau_es_test, 
			TestLabel "es 7" raz2Contra_es_test, 
			TestLabel "es 8" raz2Cont_es_test, 
			TestLabel "es 9" raz2Sat_es_test, 
			TestLabel "es 10" raz2Fal_es_test,
			TestLabel "es 11" raz3Tau_es_test, 
			TestLabel "es 12" raz3Contra_es_test, 
			TestLabel "es 13" raz3Cont_es_test, 
			TestLabel "es 14" raz3Sat_es_test, 
			TestLabel "es 15" raz3Fal_es_test,
			TestLabel "es 16" raz4Tau_es_test, 
			TestLabel "es 17" raz4Contra_es_test, 
			TestLabel "es 18" raz4Cont_es_test, 
			TestLabel "es 19" raz4Sat_es_test, 
			TestLabel "es 20" raz4Fal_es_test,		
			TestLabel "es 21" raz5Tau_es_test, 
			TestLabel "es 22" raz5Contra_es_test, 
			TestLabel "es 23" raz5Cont_es_test, 
			TestLabel "es 24" raz5Sat_es_test, 
			TestLabel "es 25" raz5Fal_es_test,
			TestLabel "es 26" raz6Tau_es_test, 
			TestLabel "es 27" raz6Contra_es_test, 
			TestLabel "es 28" raz6Cont_es_test, 
			TestLabel "es 29" raz6Sat_es_test, 
			TestLabel "es 30" raz6Fal_es_test,
			TestLabel "es 26" raz7Tau_es_test, 
			TestLabel "es 27" raz7Contra_es_test, 
			TestLabel "es 28" raz7Cont_es_test, 
			TestLabel "es 29" raz7Sat_es_test, 
			TestLabel "es 30" raz7Fal_es_test,
			TestLabel "es 31" raz8Tau_es_test, 
			TestLabel "es 32" raz8Contra_es_test, 
			TestLabel "es 33" raz8Cont_es_test, 
			TestLabel "es 34" raz8Sat_es_test, 
			TestLabel "es 35" raz8Fal_es_test,
			TestLabel "es 36" raz9Tau_es_test, 
			TestLabel "es 37" raz9Contra_es_test, 
			TestLabel "es 38" raz9Cont_es_test, 
			TestLabel "es 39" raz9Sat_es_test, 
			TestLabel "es 40" raz9Fal_es_test,
			TestLabel "es 41" raz10Tau_es_test, 
			TestLabel "es 42" raz10Contra_es_test, 
			TestLabel "es 43" raz10Cont_es_test, 
			TestLabel "es 44" raz10Sat_es_test, 
			TestLabel "es 45" raz10Fal_es_test
			];

fncTests = [TestLabel "FNC 1"  f1_fnc_ok,
            TestLabel "FNC 2"  f2_fnc_ok,
            TestLabel "FNC 3"  f3_fnc_ok,
            TestLabel "FNC 4"  f4_fnc_ok,
            TestLabel "FNC 5"  f5_fnc_ok,
            TestLabel "FNC 6"  f6_fnc_ok,
            TestLabel "FNC 7"  f7_fnc_ok,
            TestLabel "FNC 8"  f8_fnc_ok,
            TestLabel "FNC 9"  f9_fnc_ok,
            TestLabel "FNC 10" f10_fnc_ok,
            TestLabel "FNC 11" f11_fnc_ok,
            TestLabel "FNC 12" f12_fnc_ok]

-- Todas las pruebas 
allTests = TestList $ evalTests ++ creariTests ++ filasTests ++ tvTests ++ esTests ++ fncTests

-- Formulas
raz1 = p 
raz2 = (Bin (Bin q And r) Imp p) 
raz3 = (Bin (Neg q) Or r) 
raz4 = (Bin (Bin (Neg p) Or (Neg q)) Or r) 
raz5 = (Bin (Bin (Bin p Imp q) Imp p) Imp p) 
raz6 = (Bin (Bin (Bin p And q) Or (Neg p)) And q) 
raz7 = (Bin p And (Neg q)) 
raz8 = (Bin (Bin p Imp q) And (Bin p And (Neg q))) 
raz9 = (Bin p Iff (Neg q))
raz10 = (Bin (Neg (Neg p)) Or (Bin q And p))

-- Conversión a FND
f1 = p
f2 = Neg p
f3 = Bin p Or q
f4 = Bin p And q
f5 = Bin p Imp q
f6 = Bin p Iff q
f7 = Bin (Bin q And r) Or p
f8 = Bin (Neg q) Or r
f9 = Bin (Bin (Neg p) Or (Neg q)) Or r
f10 = Bin (Bin (Bin p Imp q) Imp p) Imp p
f11 = Bin (Bin (Neg p) Imp (Neg q)) Imp (Bin p Imp q)
f12 = Bin (Bin (V "p1") And (V "p2")) Or (Bin (V "p3") And (V "p4"))

--Interpretaciones 
interp1 :: [(Var, Bool)]
interp1 = [("r" , False) , ("p", False) , ("q", False)];

interp2 :: [(Var, Bool)]
interp2 = [("r" , False) , ("p", False) , ("q", True)];

interp3 :: [(Var, Bool)]
interp3 = [("r" , False) , ("p", True) , ("q", False)];


interp4 :: [(Var, Bool)]
interp4 = [("r" , False) , ("p", True) , ("q", True)];

interp5 :: [(Var, Bool)]
interp5 = [("r" , True) , ("p", False) , ("q", False)];

interp6 :: [(Var, Bool)]
interp6 = [("r" , True) , ("p", False) , ("q", True)];

interp7 :: [(Var, Bool)]
interp7 = [("r" , True) , ("p", True) , ("q", False)];

interp8 :: [(Var, Bool)]
interp8 = [("r" , True) , ("p", True) , ("q", True)];

--casos filas
filapq :: [Fila]
filapq = [[("p",True),("q",True)],[("p",True),("q",False)],[("p",False),("q",True)],[("p",False),("q",False)]];

filapqr :: [Fila]
filapqr = [[("p",True),("q",True),("r",True)],
			[("p",True),("q",True),("r",False)],
			[("p",True),("q",False),("r",True)],
			[("p",True),("q",False),("r",False)],
			[("p",False),("q",True),("r",True)],
			[("p",False),("q",True),("r",False)],
			[("p",False),("q",False),("r",True)],
			[("p",False),("q",False),("r",False)]];

filapqrs :: [Fila]
filapqrs = [[("p",True),("q",True),("r",True),("s",True)],
			[("p",True),("q",True),("r",True),("s",False)],
			[("p",True),("q",True),("r",False),("s",True)],
			[("p",True),("q",True),("r",False),("s",False)],
			[("p",True),("q",False),("r",True),("s",True)],
			[("p",True),("q",False),("r",True),("s",False)],
			[("p",True),("q",False),("r",False),("s",True)],
			[("p",True),("q",False),("r",False),("s",False)],
			[("p",False),("q",True),("r",True),("s",True)],
			[("p",False),("q",True),("r",True),("s",False)],
			[("p",False),("q",True),("r",False),("s",True)],
			[("p",False),("q",True),("r",False),("s",False)],
			[("p",False),("q",False),("r",True),("s",True)],
			[("p",False),("q",False),("r",True),("s",False)],
			[("p",False),("q",False),("r",False),("s",True)],
			[("p",False),("q",False),("r",False),("s",False)]]

filapqrst :: [Fila]
filapqrst = [[("p",True),("q",True),("r",True),("s",True),("t",True)],
			[("p",True),("q",True),("r",True),("s",True),("t",False)],
			[("p",True),("q",True),("r",True),("s",False),("t",True)],
			[("p",True),("q",True),("r",True),("s",False),("t",False)],
			[("p",True),("q",True),("r",False),("s",True),("t",True)],
			[("p",True),("q",True),("r",False),("s",True),("t",False)],
			[("p",True),("q",True),("r",False),("s",False),("t",True)],
			[("p",True),("q",True),("r",False),("s",False),("t",False)],
			[("p",True),("q",False),("r",True),("s",True),("t",True)],
			[("p",True),("q",False),("r",True),("s",True),("t",False)],
			[("p",True),("q",False),("r",True),("s",False),("t",True)],
			[("p",True),("q",False),("r",True),("s",False),("t",False)],
			[("p",True),("q",False),("r",False),("s",True),("t",True)],
			[("p",True),("q",False),("r",False),("s",True),("t",False)],
			[("p",True),("q",False),("r",False),("s",False),("t",True)],
			[("p",True),("q",False),("r",False),("s",False),("t",False)],
			[("p",False),("q",True),("r",True),("s",True),("t",True)],
			[("p",False),("q",True),("r",True),("s",True),("t",False)],
			[("p",False),("q",True),("r",True),("s",False),("t",True)],
			[("p",False),("q",True),("r",True),("s",False),("t",False)],
			[("p",False),("q",True),("r",False),("s",True),("t",True)],
			[("p",False),("q",True),("r",False),("s",True),("t",False)],
			[("p",False),("q",True),("r",False),("s",False),("t",True)],
			[("p",False),("q",True),("r",False),("s",False),("t",False)],
			[("p",False),("q",False),("r",True),("s",True),("t",True)],
			[("p",False),("q",False),("r",True),("s",True),("t",False)],
			[("p",False),("q",False),("r",True),("s",False),("t",True)],
			[("p",False),("q",False),("r",True),("s",False),("t",False)],
			[("p",False),("q",False),("r",False),("s",True),("t",True)],
			[("p",False),("q",False),("r",False),("s",True),("t",False)],
			[("p",False),("q",False),("r",False),("s",False),("t",True)],
			[("p",False),("q",False),("r",False),("s",False),("t",False)]]

-- casos tv
tvRaz1 :: TV
tvRaz1 = [([("p",True)],True),([("p",False)],False)];

tvRaz2 :: TV
tvRaz2 = [([("q",True),("r",True),("p",True)],True),
		([("q",True),("r",True),("p",False)],False),
		([("q",True),("r",False),("p",True)],True),
		([("q",True),("r",False),("p",False)],True),
		([("q",False),("r",True),("p",True)],True),
		([("q",False),("r",True),("p",False)],True),
		([("q",False),("r",False),("p",True)],True),
		([("q",False),("r",False),("p",False)],True)]
		
tvRaz3 :: TV
tvRaz3 = [([("q",True),("r",True)],True),
		([("q",True),("r",False)],False),
		([("q",False),("r",True)],True),
		([("q",False),("r",False)],True)]

tvRaz4 :: TV
tvRaz4 = [([("p",True),("q",True),("r",True)],True),
		([("p",True),("q",True),("r",False)],False),
		([("p",True),("q",False),("r",True)],True),
		([("p",True),("q",False),("r",False)],True),
		([("p",False),("q",True),("r",True)],True),
		([("p",False),("q",True),("r",False)],True),
		([("p",False),("q",False),("r",True)],True),
		([("p",False),("q",False),("r",False)],True)]

tvRaz5 :: TV
tvRaz5 = [([("p",True),("q",True)],True),
		([("p",True),("q",False)],True),
		([("p",False),("q",True)],True),
		([("p",False),("q",False)],True)]

tvRaz6 :: TV
tvRaz6 = [([("p",True),("q",True)],True),([("p",True),("q",False)],False),([("p",False),("q",True)],True),([("p",False),("q",False)],False)]


tvRaz7 :: TV
tvRaz7 = [([("p",True),("q",True)],False),
		([("p",True),("q",False)],True),
		([("p",False),("q",True)],False),
		([("p",False),("q",False)],False)]


tvRaz8 :: TV
tvRaz8 = [([("p",True),("q",True)],False),
		([("p",True),("q",False)],False),
		([("p",False),("q",True)],False),
		([("p",False),("q",False)],False)]

tvRaz9 :: TV
tvRaz9 = [([("p",True),("q",True)],False),
			([("p",True),("q",False)],True),
			([("p",False),("q",True)],True),
			([("p",False),("q",False)],False)]

tvRaz10 :: TV
tvRaz10 = [([("p",True),("q",True)],True),
			([("p",True),("q",False)],True),
			([("p",False),("q",True)],False),
			([("p",False),("q",False)],False)]


-- Auxiliares
isFNC :: L -> Bool
isFNC = isConjunction 
  where 
  isConjunction (Bin a And b) = isConjunction a && isConjunction b
  isConjunction a             = isClause a

  isClause (Bin a Or b) = isClause a && isClause b
  isClause a            = isLiteral a  

  isLiteral (V p)       = True   
  isLiteral (Neg (V p)) = True
  isLiteral _           = False

vars :: L -> [Var]
vars (V p)       = [p]
vars (Neg a)     = vars a
vars (Bin a _ b) = nub $ vars a ++ vars b

equiv :: L -> L -> Bool
equiv f1 f2 = all (\i -> eval (creari i) f1 == eval (creari i) f2) 
                  (ints $ nub (vars f1 ++ vars f2))
  where 
    ints :: [Var] -> [[(Var, Bool)]]
    ints []     = [[]]
    ints (v:vs) = [[(v,b)] ++ xs | b <- [True,False], xs <- ints vs]

sameList :: (Ord a) => [a] -> [a] -> Bool
sameList xs ys = sort xs == sort ys

isFNCof :: L -> L -> Bool
isFNCof f1 f2 = isFNC f1 && f1 `equiv` f2