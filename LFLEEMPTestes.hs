module LFLEEMPTestes where

import LFLE01EMP

import Test.HUnit

add = DecFuncao "add" ["x","y"] ((Soma (Ref "z"))(Soma (Ref "x")(Ref "y")))
sub = DecFuncao "sub" ["x","y"] ((Subtracao (Ref "z"))(Subtracao (Ref "x")(Ref "y")))
mult = DecFuncao "mult" ["x","y"] ((Multiplicacao (Ref "z"))(Multiplicacao (Ref "x")(Ref "y")))

amb = [add, sub, mult]

apAdd = Aplicacao "add" [(Valor 1), (Valor 2)]
apSub = Aplicacao "sub" [(Valor 1), (Valor 2)]
apMul = Aplicacao "mult" [(Valor 1), (Valor 2)]

let01 = Let "z" (Valor 4)(apAdd)
--      Let "z" (Valor 4)(Aplicacao "apAdd" [(Valor 1), (Valor2)])
let02 = Let "z" (Valor 4)(apSub)
let03 = Let "z" (Valor 4)(apMul)

avAdd = avaliar let01 amb
--      avaliar (Let "z" (Valor 4)(Aplicacao "apAdd" [(Valor 1), (Valor 2)])) (DecFuncao "add" ["x","y"] (Soma (Ref "z")(Soma (Ref "x")(Ref "y"))))
--  let z = 4
--    in let add x y = z+x+y
--         in add 1 2
-- >9

avSub = avaliar let02 amb
-- let z = 4
--    in let sub x y = z-(y-x)
--          in sub 2 1
-- >5
avMul = avaliar let03 amb
