import Data.List
import Test.HUnit

data Proposition = Var String | Not Proposition | And Proposition Proposition | Or Proposition Proposition | Impl Proposition Proposition deriving Eq

type Assignment = String -> Bool

-- recProp :: 
recProp  :: (String -> c) ->  					-- recVar
		(Proposition -> c -> c)-> 			-- recNot
		(Proposition -> Proposition -> c -> c -> c) ->  -- recAnd
		(Proposition -> Proposition -> c -> c -> c)->   -- recOr
		(Proposition -> Proposition -> c -> c -> c)->   -- recImpl
		Proposition -> 					-- prop
		c 
recProp recVar recNot recAnd recOr recImpl prop = case prop of Var str -> recVar str
                                                               Not p -> recNot p (rec p)
                                                               And p1 p2 -> recAnd p1 p2 (rec p1) (rec p2)
                                                               Or p1 p2 -> recOr p1 p2 (rec p1) (rec p2)
                                                               Impl p1 p2 -> recImpl p1 p2 (rec p1) (rec p2)
							where rec = recProp recVar recNot recAnd recOr recImpl

foldProp :: (String -> c) -> 					-- fVar
		(c -> c) -> 					-- fNot
		(c -> c -> c) -> 				-- fAnd
		(c -> c -> c) -> 				-- fOr
		(c -> c -> c) -> 				-- fImpl
		Proposition -> 					-- prop
		c
foldProp fVar fNot fAnd fOr fImpl prop = case prop of Var b -> fVar b
                                                      Not p1 -> fNot (rec p1)
                                                      And p1 p2 -> fAnd (rec p1)(rec p2)
                                                      Or p1 p2 -> fOr (rec p1)(rec p2)
                                                      Impl p1 p2 -> fImpl (rec p1) (rec p2)
						  where rec = foldProp fVar fNot fAnd fOr fImpl


instance Show Proposition where
  show  = foldProp (id) (\x -> "\172"++x) (\x y -> x++" \8835 "++y) (\x y -> x++" \8743 "++y) (\x y -> "("++x++" \8835 "++y++")")
     --Códigos Unicode para simbolitos por si hay problemas de codificación:  \172, \8835, \8743, \8744.

assignTrue :: [String] -> Assignment
assignTrue ls x = elem x ls -- flip elem

eval :: Assignment -> Proposition -> Bool
eval val = undefined

elimImpl :: Proposition -> Proposition
elimImpl = undefined

negateProp :: Proposition -> Proposition
negateProp = undefined

nnf :: Proposition -> Proposition
nnf = undefined

vars :: Proposition -> [String]
vars = undefined

parts :: [a] -> [[a]]
parts = foldr (\x res -> res ++ (map (x:) res)) [[]]

sat :: Proposition -> [[String]]
sat = undefined

satisfiable :: Proposition -> Bool
satisfiable = undefined

tautology :: Proposition -> Bool
tautology = undefined

equivalent :: Proposition -> Proposition -> Bool
equivalent = undefined

-- Proposiciones de prueba --

f1 = Impl (Var "p") (Var "q")
f2 = Not(Impl (Var "p") (Var "q"))
f3 = Not (Var "p")
f4 = Or f1 f2
f5 = Impl (Var "r") (Var "r")


-- Tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2
  --"ejercicio3" ~: testsEj3,
  --"ejercicio4" ~: testsEj4,
  --"ejercicio5" ~: testsEj5,
  --"ejercicio6" ~: testsEj6
  ]

testsEj1 = test [
  5 ~=? foldProp (const 1) (+1) (+) (+) (+) f4
  ]
  
testsEj2 = test [
  "\172(p \8835 q)"~=? show f2
  ]

testsEj3 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj4 = test [
  True ~=?  eval (assignTrue ["p","q"]) (Impl (And (Var "p") (Var "r")) (And (Not (Var "q")) (Var "q")))
  ]

testsEj5 = test [
  Var "q" ~=?  negateProp (Not $ Var "q"),
  Var "p" ~=?  nnf (Not $ Not $ Var "p")
  ]

testsEj6 = test [
  [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]] ~=? parts [1,2,3]
  ]

