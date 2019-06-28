module Formatacao where
import Extenso
import Operacoes

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

--Imprimir  o titulo
titulo :: String -> IO ()
titulo n = do
  putStrLn (trave (tamanho n+60))
  putStrLn (espaco 30 ++ n ++ espaco 30)
  putStrLn (trave (tamanho n+60))

--Imprimir  o cabecalho
cabecalho :: String -> IO ()
cabecalho n = do
  putStrLn (trave (tamanho n+60))
  putStrLn (espaco 5 ++ n ++ espaco 5)
  putStrLn (trave (tamanho n+60))



--convert vendas em lista
lista :: Int -> [Int]
lista n
 |n == 1 = [vendas 1]
 |n > 1 = [vendas n] ++ lista (n-1)



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
  linhaMaior n
  linhaMenor n
  linhaMedia n
  linhaTotal n


