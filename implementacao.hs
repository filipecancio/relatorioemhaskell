module Implementacao where
import Operacoes
import Dados

--Mes
mes :: Int -> String
mes n
  |n == 1 = "Janeiro"
  |n == 2 = "Fevereiro"
  |n == 3 = "Março"
  |n == 4 = "Abril"
  |n == 5 = "Maio"
  |n == 6 = "Junho"
  |n == 7 = "Julho"
  |n == 8 = "Agosto"
  |n == 9 = "Setembro"
  |n == 10 = "Outubro"
  |n == 11 = "Novembro"
  |n == 12 = "Dezembro"

-- Imprimir linha
linha :: Int->Int-> IO ()
linha x y
|(y = 1) = do putStrLn (espaco 5 ++ mes x ++ "\t" ++ show(qtPao x)++ "\t\t" ++ show(extenso (vendas n)))
|(y = 2) = do 










 -- Imprimir total
linhaTotal :: Int -> IO ()
linhaTotal n = do
 putStrLn (espaco 5 ++ "Total" ++ "\t" ++ show(total n)++ "\t\t" ++ show(extenso (total n)))

 -- Imprimir maior
linhaMaior :: Int -> IO ()
linhaMaior n = do
 putStrLn (espaco 5 ++ "Maior" ++ "\t" ++ show(linhamaior (lista n))++ "\t\t" ++ show(extenso (linhamaior (lista n))))

-- Imprimir maior
linhaMenor :: Int -> IO ()
linhaMenor n = do
 putStrLn (espaco 5 ++ "Menor" ++ "\t" ++ show(linhamenor (lista n))++ "\t\t" ++ show(extenso (linhamenor (lista n))))

-- Imprimir media
linhaMedia :: Int -> IO ()
linhaMedia n = do
 putStrLn (espaco 5 ++ "Média" ++ "\t" ++ show(linhamedia n)++ "\t\t" ++ show(extenso (linhamedia n)))
