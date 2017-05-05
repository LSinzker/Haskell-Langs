module LFLE01E where

type Id = String
type Nome = String
type Arg = String

type Ambiente = [DecFuncao]

data DecFuncao = DecFuncao Nome Arg Expressao

data Expressao = Valor Int
               | Soma Expressao Expressao
               | Subtracao Expressao Expressao
               | Multiplicacao Expressao Expressao
               | Divisao Expressao Expressao
               | Let Id Expressao Expressao
               | Ref Id
               | Aplicacao Nome Expressao
               | ExpExp Expressao Expressao
 deriving(Show, Eq)

-- O interpretador da linguagem LLE eh
-- basicamente um avaliador de expressoes, mas
-- com suporte a substituicao.

avaliar :: Expressao -> Ambiente -> Int
avaliar (Valor n) _ = n
avaliar (Soma e d) amb =  avaliar e amb + avaliar d amb
avaliar (Subtracao e d) amb = avaliar  e amb - avaliar d amb
avaliar (Multiplicacao e d) amb =  avaliar e amb * avaliar d amb
avaliar (Divisao e d) amb = avaliar e amb `div` avaliar d amb

--                "add"  v4
avaliar (Aplicacao nome exp) amb =
  let (DecFuncao n arg corpo) = pesquisarFuncao nome amb
  in avaliar (substAplica arg (avaliar exp amb) corpo amb) amb

--             "y"   (Valor 3) (Aplicacao "add" (Valor 4))
avaliar (Let subId expNomeada corpoExp) amb
  |corpoExp /= (Aplicacao n e) = avaliar (substituicao subId (avaliar expNomeada amb) corpoExp) amb
  |otherwise = avaliar (substAplica subId (avaliar expNomeada amb) corpoExp amb) amb
  --avaliar (substituicao "y"             3       (Aplicacao "add" 3) )
  where
    Aplicacao n e = pesqArg2 corpoExp

avaliar (Ref var) _ = error "avaliando uma variavel livre."

pesquisarFuncao :: Nome -> Ambiente -> DecFuncao
pesquisarFuncao nome [] = error ("Funcao " ++ nome ++ " nao declarada")
pesquisarFuncao nome (dec@(DecFuncao n a e):xs)
 | nome == n = dec
 | otherwise = pesquisarFuncao nome xs

--substituicao :: Id -> Int -> Expressao -> Maybe Ambiente -> Expressao
substituicao :: Id -> Int -> Expressao -> Expressao
substituicao subId val (Valor n) = Valor n
substituicao subId val (Soma e d) = Soma (substituicao subId val e)(substituicao subId val d)
substituicao subId val (Subtracao e d) = Subtracao (substituicao subId val e)(substituicao subId val d)
substituicao subId val (Multiplicacao e d) = Multiplicacao (substituicao subId val e)(substituicao subId val d)
substituicao subId val (Divisao e d) = Divisao (substituicao subId val e)(substituicao subId val d)
substituicao subId val (Let boundId namedExp bodyExp)
 | subId == boundId  = (Let boundId namedExp bodyExp)
 | otherwise = Let boundId namedExp (substituicao subId val bodyExp)
substituicao subId val (Ref var)
 | subId == var = (Valor val)
 | otherwise = (Ref var)

pesqArg :: Expressao -> Expressao
pesqArg (Soma ve vd) = ExpExp ve vd
pesqArg (Subtracao ve vd) = ExpExp ve vd
pesqArg (Multiplicacao ve vd) = ExpExp ve vd
pesqArg (Divisao ve vd) = ExpExp ve vd

pesqArg2 (Aplicacao vn ve) = Aplicacao vn ve

substAplica :: Id -> Int -> Expressao -> Ambiente -> Expressao
substAplica subId val (Aplicacao nome exp) amb
--  |corpo == (Soma (Ref arg)(Ref subId)) = Soma (substituicao subId val exp)(substituicao subId val (Valor val))
  |corpo == (Soma e d) = Soma (substituicao arg (avaliar exp amb) e)(substituicao subId val d)
  |corpo == (Subtracao e d) = Subtracao (substituicao arg (avaliar exp amb) e)(substituicao subId val d)
  |corpo == (Multiplicacao e d) = Multiplicacao (substituicao arg (avaliar exp amb) e)(substituicao subId val d)
  |corpo == (Divisao e d) = Divisao (substituicao arg (avaliar exp amb) e)(substituicao subId val d)
  where
    DecFuncao n arg corpo = pesquisarFuncao nome amb
    ExpExp e d = pesqArg corpo
--                             corpo = Soma(Ref "x")(Ref "y")

-- avaliar let "y" (Valor 3)(Aplicacao "add" (Valor 4))
-- avaliar (substituicao "y" 3 (Aplicacao "add" (Valor 4)) amb)
--             "y"  3   Aplicacao "add" V4                   "y"  3

--pesquisarFuncao nome ambiente =
--  let res = [dec | dec@(DecFuncao n a e) <- ambiente, nome == n]
--  in case res of
--    [dec] -> dec
--    (d:ds) -> error "duplicidade de declaracao de funcao"
--    otherwise -> error "funcao nao existe"
