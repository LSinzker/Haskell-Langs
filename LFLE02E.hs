module LFLE02E where

-- A linguagem LFLE02 suporta tanto
-- expressoes identificadas (LET) quanto
-- identificadores e expressoes + funcoes.
-- As funcoes aceitam apenas um argumento
-- sem informacoes de tipo.
--
-- As substituicoes sao realizadas de maneira
-- postergadas! Essa eh a principal diferenca
-- desta versao para a disponibilizada na
-- linguagem LFLE01
--

type Id = String
type Nome = String
type Arg = String

type Ambiente = [DecFuncao]

type Referencias = [(Id, Int)]

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

-- O interpretador da linguagem LLE eh
-- basicamente um avaliador de expressoes, mas
-- com suporte a substituicao.

avaliar :: Expressao -> Ambiente -> Referencias -> Int
avaliar (Valor n) _ _ = n
avaliar (Soma e d) amb ref =  avaliar e amb ref + avaliar d amb ref
avaliar (Subtracao e d) amb ref = avaliar e amb ref - avaliar d amb ref
avaliar (Multiplicacao e d) amb ref =  avaliar e amb ref * avaliar d amb ref
avaliar (Divisao e d) amb ref = avaliar e amb ref `div` avaliar d amb ref
avaliar (Aplicacao nome exp) amb ref = avaliar corpo amb ref'
 where
   (DecFuncao n arg corpo) = pesquisarFuncao nome amb
   valor = avaliar exp amb ref
   ref'  = (arg, valor):ref

avaliar (Let subId expNomeada corpoExp) amb ref = avaliar corpoExp amb ref'
  where
    valor = avaliar expNomeada amb ref
    ref'  = ref ++ [(subId, valor)]

avaliar (Ref var) amb ref = pesquisarValor var ref

pesquisarFuncao :: Nome -> Ambiente -> DecFuncao
pesquisarFuncao nome [] = error ("Funcao " ++ nome ++ " nao declarada")
pesquisarFuncao nome (dec@(DecFuncao n a e):xs)
 | nome == n = dec
 | otherwise = pesquisarFuncao nome xs

pesquisarValor :: Nome -> Referencias -> Int
pesquisarValor n [] = error ("identificador " ++ n ++ " nao declarado")
pesquisarValor nome ((n, v):rs)
 | nome == n = v
 | otherwise = pesquisarValor nome rs
