module Formatacao where
import Extenso

-- Calcula o tramanho de uma lista
tamanho :: [t] -> Int
tamanho [] = 0
tamanho (x:y) = 1 + tamanho y

-- Imprime tracinhos
trave :: Int -> String
trave n 
 | (n <= 0) = ""
 |otherwise = "-" ++ trave (n-1)

-- Imprime espaco
espaco :: Int -> String
espaco n 
 | (n <= 0) = ""
 |otherwise = " " ++ espaco (n-1)

--Imprimir  o cabecalho
cabecalho :: String -> IO ()
cabecalho n = do
  putStrLn (trave (tamanho n+10))
  putStrLn (espaco 5 ++ n ++ espaco 5)
  putStrLn (trave (tamanho n+10))

-- valores de vendas correspondentes aos meses
vendas :: Int -> Int
vendas n
  |n == 1 = 213
  |n == 2 = 120
  |n == 3 = 789
  |n == 4 = 3151
  |n == 5 = 2315
  |n == 6 = 8646
  |n == 7 = 5646
  |n == 8 = 3215
  |n == 9 = 8865
  |n == 10 = 2125
  |n == 11 = 10654
  |n == 12 = 9852

--convert vendas em lista
lista :: Int -> [Int]
lista n
 |n == 1 = [vendas 1]
 |n > 1 = [vendas n] ++ lista (n-1)

-- Calcular total
total :: Int -> Int
total n
 |n == 1 = vendas n
 |n > 1 = vendas n + total(n-1)

-- Calcular maior valor
linhamaior::[Int]->Int
linhamaior [n]
   | (tamanho [n] == 1) = n
linhamaior(x:y)
  |(x > linhamaior y ) = x
  |otherwise =  linhamaior y

-- Calcular menor valor
linhamenor::[Int]->Int
linhamenor [n]
  | (tamanho [n] == 1) = n
linhamenor(x:y)
  |(x < linhamenor y ) = x
  |otherwise =  linhamenor y

-- Imprimir linha
linha :: Int -> IO ()
linha n = do
 putStrLn (espaco 5 ++ mes n ++ "\t" ++ show(vendas n)++ "\t\t" ++ show(extenso n))

 -- Imprimir total
linhaTotal :: Int -> IO ()
linhaTotal n = do
 putStrLn (espaco 5 ++ "Total" ++ "\t" ++ show(total n)++ "\t\t" ++ show(extenso (vendas n)))

 -- Imprimir maior
linhaMaior :: Int -> IO ()
linhaMaior n = do
 putStrLn (espaco 5 ++ "Maior" ++ "\t" ++ show(linhamaior (lista n))++ "\t\t" ++ show(extenso (vendas n)))

  -- Imprimir maior
linhaMenor :: Int -> IO ()
linhaMenor n = do
 putStrLn (espaco 5 ++ "Menor" ++ "\t" ++ show(linhamenor (lista n))++ "\t\t" ++ show(extenso (vendas n)))

-- Imprimir linhas
linhas :: Int -> IO ()
linhas n 
  |(n <= 1) = do linha n
  |(n >1) = do 
  linhas (n-1) 
  linha n

-- Imprimir corpo
corpo :: Int -> IO ()
corpo n = do
  linhas n
  linhaTotal n
  linhaMaior n
  linhaMenor n


