module Operacoes where

-- Calcula o tamanho de uma lista
tamanho :: [t] -> Int
tamanho [] = 0
tamanho (x:y) = 1 + tamanho y

-- Calcular total
total :: [Int] -> Int
total [x]
 |(tamanho[x] == 1) = x
total(x:y)
 |otherwise =  total y + x

-- Calcular maior valor
maior::[Int]->Int
maior [x]
   | (tamanho [x] == 1) = x
maior(x:y)
  |(x > maior y ) = x
  |otherwise =  maior y

-- Calcular menor valor
menor::[Int]->Int
menor [x]
  | (tamanho [x] == 1) = x
menor(x:y)
  |(x < menor y ) = x
  |otherwise =  menor y

--Calcular media
media :: [Int]->Int
media x = div (total x) (tamanho x)

--Multiplicar lista
multiplicar :: [Int]->Int->[Int]
multiplicar [x] y 
 |(tamanho[x] == 1) = [x*y]
multiplicar (x:y) z = [x*z] ++ multiplicar y z
