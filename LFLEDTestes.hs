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
--avaliar (Let "x" (Valor 4)(Aplicacao "inc" (Valor 2))) amb
--avaliar (pesquisarArgumento "x" 4 (Aplicacap "inc" (Valor 2)) amb) amb
--avaliar (Aplicacao "inc" (Valor 4)) amb
--avaliar (substAplica "x" 4 (Soma(Ref "x")..)


let02 = Let "y" (Valor 3)(Let "x" (Valor 4) (Aplicacao "add" (Valor 3)))
--      Let "y" (Valor 3)(Aplicacao "add" (Valor 4))

--avaliar (Let "y" (Valor 3)(Let "x" (Valor 4) (Aplicacao "add" (Valor 3))))
--avaliar (pesquisarArgumento "add" "y" 3 (Let "x" (Valor 4) (Aplicacao "add" (Valor 3))))
--avaliar (Let "y" (Valor 3) )
