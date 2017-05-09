module LFCFTTestes where

import LFCFT

import Test.HUnit

v5 = Valor 5
v3 = Valor 3

s1 = Soma v5 v3

s2 = Soma s1 v3

-- let x = 5 in x + x
let01 = Let "x" (Valor 5) (Soma (Ref "x") (Ref "x"))
-- (\x -> x+x)5
-- 10

-- let x = 5 in let y = 10 in x + y
let02 = Let "x" (Valor 5)
        (Let "y" (Valor 10) (Soma (Ref "x") (Ref "y")))

-- let x = 5 in x + let x = 10 in x + 3
let03 = Let "x" (Valor 5)
         (Soma (Ref "x") (Let "x" (Valor 10) (Soma (Ref "x") (Valor 3))))

--avaliar ( Let "x" (Valor 5) (Soma (Ref "x") (Let "x" (Valor 10) (Soma (Ref "x") (Valor 3)))) )
--avaliar ( App (Lambda "x" (Soma (Ref "x") (Let "x" (Valor 10) (Soma (Ref "x") (Valor 3))))) (Valor 5))
                                           ----e----  ---------------------d----------------------
--avaliar ( substituicao "x" (Valor 5) (Soma (Ref "x") (Let "x" (Valor 10) (Soma (Ref "x") (Valor 3) ) ) ) )
--avaliar ( Soma (substituicao "x" (Valor 5) (Ref "x") (substituicao "x" (Valor 5) (Let "x" (Valor 10) (Soma (Ref "x") (Valor 3))))))
--avaliar ( Soma (Valor 5) )

-- let x = 5 in let y = x in y
let04 = Let "x" (Valor 5)
        (Let "y" (Ref "x") (Ref "y"))
-- let x=5
--    in let y=x
--        in y

-- let x = 5 in let x = x in x
let05 = Let "x" (Valor 5)
        (Let "x" (Ref "x") (Ref "x"))

teste1 = TestCase (assertEqual "avaliar 5" 5 (toInt v5))

teste2 = TestCase (assertEqual "avaliar 5 + 3" 8 (toInt (avaliar s1)))

teste3 = TestCase (assertEqual "avaliar (5 + 3) + 3" 11 (toInt (avaliar s2)))

teste4 = TestCase (assertEqual "avaliar let x = 5 in x + x" 10 (toInt(avaliar let01)))

teste5 = TestCase (assertEqual "avaliar let x = 5 in let y = 10 in x + y" 15 (toInt(avaliar let02)))

teste6 = TestCase (assertEqual "avaliar let x = 5 in x + let x = 10 in x + 3" 18 (toInt(avaliar let03)))

teste7 = TestCase (assertEqual "avaliar let x = 5 in let y = x in y" 5 (toInt(avaliar let04)))

teste8 = TestCase (assertEqual "avaliar let x = 5 in let x = x in x" 5 (toInt(avaliar let05)))

todosOsTestes = TestList [ teste1
                         , teste2
                         , teste3
                         , teste4
                         , teste5
                         , teste6
                         , teste7
                         , teste8
                         ]

executarTestes = runTestTT todosOsTestes


--           argFor   exp2  ---------corpo---------
--            subId  expNom  -------corpoExp-------
--avaliar (Let "x" (Valor 5)(Soma (Ref "x")(Valor 1))) =======> (\x -> x+x)5
--avaliar (App ("x" (Soma(Ref "x")(Valor 1))) (Valor 5))
--              -------------exp1------------ ---exp2---
--avaliar (Let subId expNomeada corpoExp) = avaliar(App (Lambda subId (corpoExp)) expNomeada)

--avaliar (App (Lambda "x" (Soma(Ref"x")(Valor 1)))())
--let expLambda = avaliar exp1
--in case expLambda of
--    (Lambda arg corpo) ->

--let01 = Let "x" (Valor 5) (Soma (Ref "x") (Ref "x"))
-- (\x -> x+x)5
-- 10
--avaliar (Let "x"(Valor 5)(Soma(Ref "x")(Ref "x")))
--

--definindo expressoes lambda:
--incre = \x -> x+1
--incre x = x+1
--
--somar = \x -> \y -> x+y
--somar x y = x+y
--
--(\x -> x+4)4
--8
--
-- Let "x" (Valor 5) (Soma(Ref "x")(Ref "y"))
-- (\x -> x+x)5
-- 10
--
--(\x -> 5+5)1
--10
