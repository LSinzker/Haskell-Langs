module LFLEETestes where

import LFLE01E

import Test.HUnit

inc :: DecFuncao
inc = DecFuncao "inc" "x" (Soma (Ref "x")(Valor 1))
add = DecFuncao "add" "x" (Soma (Ref "x")(Ref "y"))
sub = DecFuncao "sub" "x" (Subtracao (Ref "x")(Ref "y"))
divi = DecFuncao "divi" "x" (Divisao (Ref "x")(Ref "y"))

amb1 = [inc]
amb2 = [add]
amb3 = [sub]
amb4 = [divi]

--apInc = Aplicacao "inc" (Ref "x")
apInc = Aplicacao "inc" (Valor 3)
apAdd = Aplicacao "add" (Valor 4)
apSub = Aplicacao "sub" (Valor 2)
apDiv = Aplicacao "divi" (Valor 2)

--let x = 3 in let inc x = x+1 in inc x
let01 = Let "x" (Valor 3)(apInc)
--      Let "x" (Valor 3)(Aplicacao "inc" (Ref "x"))

--let01 = Let "y" (Valor 3)(Let "x"(Valor 4)(Soma(Ref "x")(Ref "y")))
--let y = 3 in let add x = x + y in add 4
let02 = Let "y" (Valor 3)(apAdd)-- avaliar let02 amb2
--              subId expNom  -------corpoExp---------    -------------------amb----------------------
-- avaliar (let "y" (Valor 3)(Aplicacao "add" (Valor 4)))(DecFuncao "add" "x" (Soma(Ref "x")(Ref "y")))
--                                                                    n    a             e
let03 = Let "y" (Valor 8)(apSub)
--    let y=8 in let sub x = x-y in sub 2

let04 = Let "y" (Valor 10)(apDiv)
