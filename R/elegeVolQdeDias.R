# Geracao da base de dados e filtro de volume e
# quantidade de dias de negociação

# Instalando as funcoes do contabiliDados
source("https://raw.githubusercontent.com/kleberformiga/contabilidados/master/contabilidados.R")
cntdd.carregaPacotes("tidyverse")

# Montagem das bases de dados

# Ver dias de negociacao por periodo

ddNeg <- readRDS("G:/Meu Drive/criaCarteiras/R/dados/diasNeg.rds") %>%
  mutate(lagQdeDiasTot = shift(qdeDiasTot)) %>% na.omit

cntdd.uneMatrix(Arquivo = "G:/Meu Drive/criaCarteiras/R/dados/fechDiarioport.xlsx",
                SeqVarPlan = c("preco", "volume"),
                index = c("cod", "data"),
                clsPer = "date", clsVlr = "numeric")

# Parâmetros para filtro
percdias <- 80
parVolume <- 500000


  
  # Baixando a quantidade de dias de negociacao

  
  # Elegibilidade por:
  #     1) acao mais negociada (quando tiver duas)
  #     2) acao com mais de 80% de dias negociados no ano anterior
  #     3) acao com volume maior que 500.000, por dia, no ano anterior
  
  papeis <-
    bdPainel %>%  
    separate(data, c("ano", "mes", "dia")) %>%
    na.omit %>%
    group_by(cod, ano) %>% 
    summarise(qdeDias = n(), volumeMedio = mean(volume, na.rm = T)) %>%
    group_by(cod) %>% 
    mutate(lagQdeDias = shift(qdeDias), lagVolume = shift(volumeMedio)) %>% 
    # criar condicao para escolher, dentre dois ou mais papeis, o com maior liquidez
    mutate(cod2 = substr(cod, 1, 4)) %>% na.omit %>% 
    group_by(cod2, ano) %>% 
    mutate(elege = lagVolume == max(lagVolume, na.rm = T)) %>%
    ungroup %>% 
    left_join(select(diasNeg, ano, lagQdeDiasTot), by = "ano") %>%
    mutate(percDiasNeg = lagQdeDias / lagQdeDiasTot * 100) %>%
    filter(elege == T) %>% 
    filter(percDiasNeg >= percdias & volumeMedio >= parVolume) %>% 
    na.omit %>% select(cod, ano, elege, percDiasNeg, volumeMedio)
  