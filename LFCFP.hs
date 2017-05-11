module LFCFP where

-- A linguagem LFCF suporta tanto
-- expressoes identificadas (LET) quanto
-- identificadores e funcoes de alta ordem
-- (com o mecanismo de expressoes lambda).

type Id = String
type Ambiente = [(Id, Expressao)]

data Expressao = Valor Int
               | Soma Expressao Expressao
               | Subtracao Expressao Expressao
               | Multiplicacao Expressao Expressao
               | Divisao Expressao Expressao
               | Let Id Expressao Expressao
               | Ref Id
               | Lambda Id Expressao
               | App Expressao Expressao
 deriving(Show, Eq)

-- O interpretador da linguagem LFCF
-- precisa ser melhor discutido, uma vez que
-- o tipo de retorno nao pode ser simplesmente
-- um inteiro. O que seria a avaliacao de uma
-- expressao lambda (\x -> x + 1)?

--Ambiente que realiza o mapeamento entre identificadores e expressoes

toInt :: Expressao -> Int
toInt (Valor n) = n

avaliar :: Expressao -> Ambiente -> Expressao
avaliar (Valor n) amb = (Valor n)
avaliar (Soma e d) amb = avaliarExpBin e d (+) amb
avaliar (Subtracao e d) amb = avaliarExpBin e d (-) amb
avaliar (Multiplicacao e d) amb = avaliarExpBin e d (*) amb
avaliar (Divisao e d) amb = avaliarExpBin e d div amb
avaliar (Lambda argFormal corpo) amb = (Lambda argFormal corpo)

avaliar (App exp1 exp2) amb
     |exp1 == (Lambda argFormal corpo) = avaliar corpo amb'
     |otherwise = error "aplicando uma expressao que nao eh lambda"
     where
       Lambda argFormal corpo = exp1
       amb' = (argFormal, exp2):amb
       
avaliar (Let subId expNomeada corpoExp) amb = avaliar (App exp1 exp2) amb'
    where
      Let boundId namedExp bodyExp = corpoExp
      exp1 = Lambda subId corpoExp
      exp2 = avaliar expNomeada amb
      amb' = amb ++ [(subId, expNomeada)]

avaliar (Ref var) amb = pesquisarValor var amb

avaliarExpBin :: Expressao -> Expressao -> (Int -> Int -> Int) -> Ambiente -> Expressao
avaliarExpBin e d op amb = Valor (op ve vd)
 where
  (Valor ve) = avaliar e amb
  (Valor vd) = avaliar d amb

pesquisarValor :: Id -> Ambiente -> Expressao
pesquisarValor n [] = error ("identificador " ++ n ++ " nao declarado")
pesquisarValor nome ((n, v):rs)
 | nome == n = v
 | otherwise = pesquisarValor nome rs

-- Considerando a linguagem LFCF, implemente uma versão que posterga as substituiçoes
-- usando um ambiente que realiza o mapeamento entre identiﬁcadores e expressões.
-- Leia a Seção 6.4 do livro base da disciplina. O resultado deve ser a linguagem
-- LFCFP (P de substituições postergadas).
