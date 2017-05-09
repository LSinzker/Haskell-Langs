module LFLEETestes where

import LFLE01E

import Test.HUnit

amb = [inc, add, sub, divi]

inc = DecFuncao "inc" "x" (Soma (Ref "x")(Valor 1))
add = DecFuncao "add" "x" (Soma (Ref "x")(Ref "y"))
sub = DecFuncao "sub" "x" (Subtracao (Ref "x")(Ref "y"))
divi = DecFuncao "divi" "x" (Divisao (Ref "y")(Ref "x"))

--apInc = Aplicacao "inc" (Ref "x")
apInc = Aplicacao "inc" (Valor 2)
apAdd = Aplicacao "add" (Valor 3)
apSub = Aplicacao "sub" (Valor 2)
apDiv = Aplicacao "divi" (Valor 2)

let00 = Let "x" (Valor 4)(Let "y" (Valor 3) apAdd)
--
--let x = 3 in let inc x = x+1 in inc x
let01 = Let "x" (Valor 3)(apInc)
--      Let "x" (Valor 3)(Aplicacao "inc" (Valor 2))

--let01 = Let "y" (Valor 3)(Let "x"(Valor 4)(Soma(Ref "x")(Ref "y")))
--let y = 3 in let add x = x + y in add 4
let02 = Let "y" (Valor 3)(apAdd)-- avaliar let02 amb2
--              subId expNom  -------corpoExp---------    -------------------amb----------------------
-- avaliar (let "y" (Valor 3)(Aplicacao "add" (Valor 4)))(DecFuncao "add" "x" (Soma(Ref "x")(Ref "y")))
--                                                                    n    a             e
let03 = Let "y" (Valor 8)(apSub)
--    let y=8 in let sub x = x-y in sub 2

let04 = Let "y" (Valor 10)(apDiv)

let05 = Let "x" (Valor 5)(Let "y" (Ref "x")(Ref "y"))

let06 = Let "x" (Valor 5)
        (Let "x" (Ref "x") (Ref "x"))

let07 = Let "x" (Valor 5)
                (Soma (Ref "x") (Let "x" (Valor 10) (Soma (Ref "x") (Valor 3))))

let11 = Let "y" (Valor 3)(Let "x" (Valor 4) apAdd)
--          subId   val     -------------------corpoExp----------------
--                         boundId  namedExp  -----------bodyExp----------
--avaliar (Let "y" (Valor 3)(Let "x"(Valor 4) (Aplicacao "add" (Valor 3))) amb
--avaliar (substAplica "y" 3 (Let "x"(Valor 4)(Aplicacao "add" (valor 3))) amb
--avaliar (Let "x" (Valor 4)(subs))
