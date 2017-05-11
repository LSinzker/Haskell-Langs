module LFLETestes where

import LFLE02E

import Test.HUnit

inc :: DecFuncao
inc = DecFuncao "inc" "x" (Soma (Ref "x") (Valor 1))

sqr :: DecFuncao
sqr = DecFuncao "sqr" "x" (Multiplicacao (Ref "x") (Ref "x"))

f :: DecFuncao
f = DecFuncao "f" "p" (Ref "n")

app1 :: Expressao
app1 = Aplicacao "inc" (Valor 5)

app2 :: Expressao
app2 = Aplicacao "sqr" (Valor 5)

let1 :: Expressao
let1 = Let "x" (Valor 5) (Soma (Ref "x") (Ref "x"))

let2 :: Expressao
let2 = Let "x" (Valor 10) (Aplicacao "sqr" (Ref "x"))

let3 :: Expressao
let3 = Let "n" (Valor 5) (Aplicacao "f" (Valor 10))

let4 :: Expressao
let4 = Let "x" (Valor 10) (Soma (Ref "x") (Let "x" (Valor 5) (Soma (Ref "x") (Valor 8))))

let5 :: Expressao
let5 = Let "x" (Valor 10)
        (Soma (Let "x" (Valor 5) (Soma (Ref "x") (Valor 8))) (Ref "x"))


amb = [inc, sqr, f]

testeInc = TestCase (assertEqual "avaliar inc 5" 6 (avaliar app1 amb []))

testeSqr = TestCase (assertEqual "avaliar sqr 5" 25 (avaliar app2 amb []))

testeLet1 = TestCase (assertEqual "avaliar let x = 5 in x + x" 10 (avaliar let1 [] []))

testeLet2 = TestCase (assertEqual "avaliar let x = 10 in sqr x)" 100 (avaliar let2 amb []))

testeLet3 = TestCase (assertEqual "avaliar let n = 5 in f 10)" 5 (avaliar let3 amb []))

testeLet4 = TestCase (assertEqual "avaliar let x = 10 in x + (let x = 5 in x + 8)" 28
                      (avaliar let4 [] []))

testeLet5 = TestCase (assertEqual "avaliar let x = 10 in (let x = 5 in x + 8) + x" 28
                      (avaliar let5 [] []))

todosOsTestes = TestList [ testeInc
                         , testeSqr
                         , testeLet1
                         , testeLet2
                         , testeLet3
                         ]

executarTestes = runTestTT todosOsTestes
