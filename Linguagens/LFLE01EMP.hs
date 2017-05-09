module LFLE01EMP where

-- Linguagem LFLE01E com suporte a funções com
-- um ou mais argumentos.

type Id = String
type Nome = String
type Arg = String

type Ambiente = [DecFuncao]

data DecFuncao = DecFuncao Nome [Arg] Expressao

data Expressao = Valor Int
               | Soma Expressao Expressao
               | Subtracao Expressao Expressao
               | Multiplicacao Expressao Expressao
               | Divisao Expressao Expressao
               | Let Id Expressao Expressao
               | Ref Id
               | Aplicacao Nome [Expressao]
               | ExpExp Expressao Expressao
               | Expressao
 deriving(Show, Eq)

avaliar :: Expressao -> Ambiente -> Int
avaliar (Valor n) _ = n
avaliar (Soma e d) amb = avaliar e amb + avaliar d amb
avaliar (Subtracao e d) amb = avaliar e amb - avaliar d amb
avaliar (Multiplicacao e d) amb = avaliar e amb * avaliar d amb
avaliar (Divisao e d) amb = avaliar e amb `div` avaliar d amb

avaliar (Aplicacao nome exp) amb
  | length arg == length exp = avaliar (substAplica arg exp corpo) amb
    where
      (DecFuncao n arg corpo) = pesquisarFuncao nome amb

avaliar (Let subId expNomeada corpoExp) amb
  |corpoExp /= (Aplicacao n e) = avaliar (substituicao subId (avaliar expNomeada amb) corpoExp) amb
  |otherwise = avaliar (substAplica subIds vals corpo) amb
    where
      Aplicacao nome e = corpoExp
      DecFuncao n args corpo = pesquisarFuncao nome amb
      subIds = [subId] ++ args -- merge (["z"] ["y", "z"]) = ["z","y","x"]
      vals = [expNomeada] ++ e -- merge ([Valor 4], [(Valor 3), (Valor 2)]) = [(Valor 4), (Valor 3), (Valor 2)]

avaliar (Ref var) _ = error "avaliando uma variavel livre."

pesquisarFuncao :: Nome -> Ambiente -> DecFuncao
pesquisarFuncao nome [] = error ("Funcao" ++ nome ++ "nao declarada")
pesquisarFuncao nome (dec@(DecFuncao n a e): xs)
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

--       ["z","y","x"]
substAplica :: [Id] -> [Expressao] -> Expressao -> Expressao
substAplica subIds vals corpo = lets subIds vals corpo

--subIds = ["z","y","x"]
--vals = [(Valor 4), (Valor 3), (Valor 2)]

lets :: [Id] -> [Expressao] -> Expressao -> Expressao
lets (x:xs)(y:ys) corpo
  |xs == [] = Let x y corpo
  |ys == [] = Let x y corpo
  |otherwise = Let x y (lets xs ys corpo)
