module Formatacao where

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


cabecalho :: String -> IO ()
cabecalho n = do
  putStrLn (trave (tamanho n+10))
  putStrLn (espaco 5 ++ n ++ espaco 5)
  putStrLn (trave (tamanho n+10))
