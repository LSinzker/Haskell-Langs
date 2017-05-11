module LFLEDTestes where

import LFLE01D

import Test.HUnit

amb = [inc, add, sub, divi]

inc = DecFuncao "inc" "x" (Soma (Ref "x")(Valor 1))
add = DecFuncao "add" "x" (Soma (Ref "x")(Ref "y"))
sub = DecFuncao "sub" "x" (Subtracao (Ref "x")(Ref "y"))
divi = DecFuncao "divi" "x" (Divisao (Ref "x")(Ref "y"))

apInc = Aplicacao "inc" (Valor 2)
apAdd = Aplicacao "add" (Valor 3)


let00 = Let "x" (Valor 3)(Let "y" (Valor 4) apAdd)

let01 = Let "x" (Valor 4)(apInc)

let02 = Let "y" (Valor 3)(Let "x" (Valor 4) (Aplicacao "add" (Valor 3)))

let03 = Let "x" (Valor 4)(Let "y" (Valor 3) (Aplicacao "add" (Valor 3)))

--let08 = let x=10 in let x=x+5 in x+2
let04 = Let "x" (Valor 10) (Let "x" (Soma (Ref "x")(Valor 5)) (Soma(Ref "x")(Valor 2)))
