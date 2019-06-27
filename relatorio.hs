module Relatorio when

texto :: String
texto = "Relat�rio de vendas \n Empresa xyz"

imprimirTracinhos :: Int -> String
imprimirTracinhos n
		 |n == 0 = " " 	
		 |n > 0 = "-" ++ imprimirTracinhos (n-1)

cabecalho :: String
cabecalho = imprimirTracinhos 50 ++ "\n" ++ texto ++ "\n" ++ imprimirTracinhos 50 ++ "Mes \t Quantidade" ++ "\n"

convert :: Int -> String
convert n
	|n == 1 = "Janeiro"
	|n == 2 = "Fevereiro"
	|n == 3 = "Mar�o"
	|n == 4 = "Abril"
	|n == 5 = "Maio"
	|n == 6 = "Junho"
	|n == 7 = "Julho"
	|n == 8 = "Agosto"
	|n == 9 = "Setembro"
	|n == 10 = "Outubro"
	|n == 11 = "novembro"
	|n == 12 = "Dezembro"

imprimirLinha :: Int -> String
imprimeLinha n = convert n ++ "\t" ++ show(vendas n)

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

rodape :: Int -> String
rodape n = imprimeSoma n

imprimeSoma :: Int -> String
imprimeSoma n = "soma" ++ show(soma n)

soma :: Int -> Int
soma n
     |n == 1 = vendas n
     |n > 1 = vendas n + soma(n-1)

corpo :: Int -> String
corpo n = imprimeLinhas n

relatorio :: Int -> String
relatorio n = cabecalho ++ corpo n ++ rodape n

// colocar centralizado
// maior venda
// menor venda
// media
// quantidadeVendas







