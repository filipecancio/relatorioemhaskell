-- Criar um módulo que imprima um relatório de vendas com:
-- a) Título centralizado
-- b) Contagem de vendas mensais
-- c) Valor total
-- d) Maior venda
-- e) Menor venda
-- f) Valor por extenso
module Relatorio where
import Formatacao

--Comando de execucao do relatorio
relatorio :: Int -> IO ()
relatorio n = do
 cabecalho "Relatório de vendas - Empresa xyz\n"
 cabecalho "Mes        Quantidade"
 corpo n



