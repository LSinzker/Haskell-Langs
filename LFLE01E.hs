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
 deriving(Show, Eq)

avaliar :: Expressao -> Ambiente -> Int
avaliar (Valor n) _ = n
avaliar (Soma e d) amb =  avaliar e amb + avaliar d amb
avaliar (Subtracao e d) amb = avaliar  e amb - avaliar d amb
avaliar (Multiplicacao e d) amb =  avaliar e amb * avaliar d amb
avaliar (Divisao e d) amb = avaliar e amb `div` avaliar d amb

avaliar (Aplicacao nome exp) amb =
  let (DecFuncao n arg corpo) = pesquisarFuncao nome amb
  in avaliar (substAplica arg (avaliar exp amb) corpo amb) amb

avaliar (Let subId expNomeada corpoExp) amb =
   avaliar (substAplica subId (avaliar expNomeada amb) corpoExp amb) amb
--  |corpoExp /= (Aplicacao n e) = avaliar (substituicao subId (avaliar expNomeada amb) corpoExp) amb
--  |otherwise = avaliar (substAplica subId (avaliar expNomeada amb) corpoExp amb) amb
--  where
--    Aplicacao n e = corpoExp

avaliar (Ref var) _ = error "avaliando uma variavel livre."

pesquisarFuncao :: Nome -> Ambiente -> DecFuncao
pesquisarFuncao nome [] = error ("Funcao " ++ nome ++ " nao declarada")
pesquisarFuncao nome (dec@(DecFuncao n a e):xs)
 | nome == n = dec
 | otherwise = pesquisarFuncao nome xs

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

substituicao subId val (Aplicacao nome exp) = Aplicacao nome exp

substAplica :: Id -> Int -> Expressao -> Ambiente -> Expressao
substAplica subId val (Valor n) amb = Valor n
substAplica subId val (Soma e d) amb = Soma (substituicao subId val e)(substituicao subId val d)
substAplica subId val (Subtracao e d) amb = Subtracao (substituicao subId val e)(substituicao subId val d)
substAplica subId val (Multiplicacao e d) amb = Multiplicacao (substituicao subId val e)(substituicao subId val d)
substAplica subId val (Divisao e d) amb = Divisao (substituicao subId val e)(substituicao subId val d)
substAplica subId val (Let boundId namedExp bodyExp) amb =
  substRef subId val (Let boundId namedExp bodyExp) amb

substAplica subId val (Ref var) amb
  | subId == var = (Valor val)
  | otherwise = (Ref var)

substAplica subId val (Aplicacao nome exp) amb =
  let (DecFuncao n arg corpo) = pesquisarFuncao nome amb
  in Let subId (Valor val)(Let arg exp (corpo))

substRef :: Id -> Int -> Expressao -> Ambiente -> Expressao
substRef subId val (Let boundId namedExp bodyExp) amb
  | namedExp == (Ref subId) = Let boundId (Valor val) bodyExp
  | otherwise = Let boundId namedExp (substAplica subId val bodyExp amb)


--  Let boundId namedExp (substAplica subId val bodyExp amb)
