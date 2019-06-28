module Dados where
import Operacoes

-- valores do produto
valor :: Int -> Int
valor x
 |(x == 1) = 2 --pao
 |(x == 2) = 4 --broa

-- valores de vendas correspondentes aos meses

-- Quantidade de pao
qtPao :: Int -> Int
qtPao x
  |(x == 1) = 213
  |(x == 2) = 120
  |(x == 3) = 789
  |(x == 4) = 3151
  |(x == 5) = 231
  |(x == 6) = 8646
  |(x == 7) = 5646
  |(x == 8) = 3215
  |(x == 9) = 8865
  |(x == 10) = 212
  |(x == 11) = 1065
  |(x == 12) = 985


-- Quantidade de broa
qtBroa :: Int -> Int
qtBroa x
  |(x == 1) = 100
  |(x == 2) = 250
  |(x == 3) = 800
  |(x == 4) = 2345
  |(x == 5) = 454
  |(x == 6) = 345
  |(x == 7) = 765
  |(x == 8) = 766
  |(x == 9) = 898
  |(x == 10) = 234
  |(x == 11) = 54
  |(x == 12) = 246

-- Funcoes get

-- retorna lista de quantidade de paes
getQtPao :: Int -> [Int]
getQtPao x
 |(x == 1) = [qtPao 1]
 |(x > 1) = [qtPao x] ++ getQtPao (x-1)

-- retorna lista de quantidade de broas
getQtBroa :: Int -> [Int]
getQtBroa x
 |(x == 1) = [qtBroa 1]
 |(x > 1) = [qtBroa x] ++ getQtBroa (x-1)

-- retorna lista de quantidades
getQt :: Int->Int->[Int]
getQt x y
 |(x == 1) = getQtPao (y)
 |(x == 2) = getQtBroa (y)

--retorna lista de valores
getValores :: Int->Int->[Int]
getValores x
 |(x == 1) = multiplicar (getQtPao y) (valor 1)
 |(x == 2) = multiplicar (getQtBroa y) (valor 2)