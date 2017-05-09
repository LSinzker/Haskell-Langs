module LFCFT where

-- A linguagem LFCF suporta tanto
-- expressoes identificadas (LET) quanto
-- identificadores e funcoes de alta ordem
-- (com o mecanismo de expressoes lambda).

type Id = String

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

toInt :: Expressao -> Int
toInt (Valor n) = n

avaliar :: Expressao -> Expressao
avaliar (Valor n) = (Valor n)

avaliar (Soma e d) = Valor (ve + vd)
 where
  (Valor ve) = avaliar e
  (Valor vd) = avaliar d

avaliar (Subtracao e d) = Valor (ve - vd)
  where
   (Valor ve) = avaliar e
   (Valor vd) = avaliar d

avaliar (Multiplicacao e d) = Valor (ve * vd)
  where
   (Valor ve) = avaliar e
   (Valor vd) = avaliar d

avaliar (Divisao e d) = Valor (ve `div` vd)
  where
   (Valor ve) = avaliar e
   (Valor vd) = avaliar d

avaliar (Lambda argFormal corpo) = (Lambda argFormal corpo)

avaliar (App exp1 exp2) =
 let expLambda = avaliar exp1
 in case expLambda of
     (Lambda argFormal corpo) -> avaliar (substituicao argFormal exp2 corpo)
     otherwise -> error "aplicando uma expressao que nao eh lambda"

avaliar (Let subId expNomeada corpoExp) = avaliar (App exp1 exp2)
--  | corpoExp /= Let boundId namedExp bodyExp = substituicao subId expNomeada corpoExp
--  | otherwise = avaliar (App exp1 exp2)
    where
      Let boundId namedExp bodyExp = corpoExp
      exp1 = Lambda subId corpoExp
      exp2 = expNomeada

avaliar (Ref var) = error "avaliando uma variavel livre."

substituicao :: Id -> Expressao -> Expressao -> Expressao
substituicao subId val (Valor n) = Valor n
substituicao subId val (Soma e d)
  | d == Let boundId namedExp bodyExp = Soma (substituicao subId val e)(avaliar d)
  | otherwise = Soma (substituicao subId val e)(substituicao subId val d)
  where
    Let boundId namedExp bodyExp = d

substituicao subId val (Subtracao e d)
  | d == Let boundId namedExp bodyExp = Subtracao (substituicao subId val e)(avaliar d)
  | otherwise = Subtracao (substituicao subId val e)(substituicao subId val d)
  where
    Let boundId namedExp bodyExp = d

substituicao subId val (Multiplicacao e d)
  | d == Let boundId namedExp bodyExp = Multiplicacao (substituicao subId val e)(avaliar d)
  | otherwise = Multiplicacao (substituicao subId val e)(substituicao subId val d)
  where
    Let boundId namedExp bodyExp = d

substituicao subId val (Divisao e d)
  | d == Let boundId namedExp bodyExp = Divisao (substituicao subId val e)(avaliar d)
  | otherwise = Divisao (substituicao subId val e)(substituicao subId val d)
  where
    Let boundId namedExp bodyExp = d

substituicao subId val (Let boundId namedExp bodyExp) =
  substRef subId val (Let boundId namedExp bodyExp)

substituicao subId val (Ref var)
  | subId == var =  val
  | otherwise = (Ref var)

substRef :: Id -> Expressao -> Expressao -> Expressao
substRef subId val (Let boundId namedExp bodyExp)
  | namedExp == (Ref subId) = Let boundId val bodyExp
  | otherwise = Let boundId namedExp (substituicao subId val bodyExp)
