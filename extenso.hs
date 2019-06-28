module Extenso where
  
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

unidades :: [String]
unidades = ["zero","um","dois","tres","quatro","cinco","seis","sete","oito","nove"]

dezena :: [String]
dezena = ["dez","onze", "doze","treze","catorze","quinze","dezesseis","dezesete","dezoito","dezenove"]

dezenas :: [String]
dezenas = ["vinte","trinta","quarenta","cinquenta","sessenta","setenta","oitenta","noventa"]

centenas :: [String]
centenas = ["cem","cento", "duzentos","trezentos","quatrocentos","quinhentos","seissentos","setessentos","oitossentos","novessentos"]

extenso :: Int->String
extenso n
 |(n < 1000) = ordem n ++" reais"
 |(n == 1000) = "mil reais"
 |(n < 2000) = "mil "++ordem (n-1000)++" reais"
 |(n >= 2000) = ordem (div n 1000) ++" mil "++ordem (n-((div n 1000)*1000)) ++" reais"

ordem :: Int->String
ordem n = ordem1 c d u
 where
  c = div n 100
  d = div (n-(c*100)) 10
  u = (n-(c*100))-(d*10)

ordem1 :: Int->Int->Int->String
ordem1 0 0 u = unidades !! u 
ordem1 0 1 u = dezena !! u
ordem1 0 d 0 = dezenas !! (d-2)
ordem1 0 d u = dezenas !! (d-2) ++" e "++ unidades !! u
ordem1 1 0 0 = centenas !! 0
ordem1 c 0 0 = centenas !! c
ordem1 c 0 u = centenas !! c ++" e "++ unidades !! u 
ordem1 c 1 u = centenas !! c ++" e "++ dezena !! u
ordem1 c d 0 = centenas !! c ++" e "++ dezenas !! (d-2)
ordem1 c d u = centenas !! c ++" e "++ dezenas !! (d-2) ++" e "++ unidades !! u
