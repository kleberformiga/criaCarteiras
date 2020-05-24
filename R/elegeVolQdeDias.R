#########################################################################
# GERAÇÃO DA BASE DE DADOS                                              #
# Filtro por volume e liquidez (dias negociados)                        #
# Inputs:                                                               #
#        1) Planilha com quantitativo de dias negociados na B3          #
#        2) Planilha com preço de fechamento e volume diário            #
# Outputs:                                                              #
#        1) Banco de dados com código e ano dos papeis com maior volume #
#           por empresa que poderão compor o portfolio, baseado na      #
#           quantidade de dias negociados e volume médio anual          #
#########################################################################

# # Instalando as funcoes do contabiliDados
# 
#   cntdd <- file.path("https://raw.githubusercontent.com",
#                    "kleberformiga/contabilidados/master",
#                    "contabilidados.R")
# 
#   source(cntdd)
#   cntdd.carregaPacotes("tidyverse")
#   
#   setwd("G:/Meu Drive/criaCarteiras/R")
# 
# # Montagem das bases de dados ####
# 
#   # Quantidade de pregões na B3 por ano
# 
#   ddNeg <- read.csv("dados/numPregoes.csv",
#                     skip = 1, sep = ";") %>% 
#     mutate(lagQdeDiasTot = shift(qdeDiasTot)) %>%
#     select(ano, qdeDiasTot, lagQdeDiasTot) %>% na.omit
# 
#   # Baixa dados de preço (fechamento) e volume com função apropriada
#   # para matrix do Economatica
#   
#   # Arquivo 1994-1999
#   arquivo <- "dados/Fechvolumediario1994a1999.xlsx"
#   cntdd.uneMatrix(Arquivo = arquivo, SeqVarPlan = c("preco", "volume"),
#                   index = c("cod", "data"),
#                   clsPer = "date", clsVlr = "numeric")
#   bd1 <- bdPainel %>% filter(year(data) != 1993) %>%
#     na.omit %>% data.table
#   
#   # Arquivo 2000-2009
#   arquivo <- "dados/Fechvolumediario2000a2009.xlsx"
#   cntdd.uneMatrix(Arquivo = arquivo, SeqVarPlan = c("preco", "volume"),
#                   index = c("cod", "data"),
#                   clsPer = "date", clsVlr = "numeric")
#   bd2 <- bdPainel %>% filter(year(data) != 1999) %>% 
#     na.omit %>% data.table
#   
#   bdDiario <- merge.data.table(bd1, bd2, all = T)
#   attributes(bdDiario)$sorted <- NULL
#   
#   # Arquivo 2010-2019
#   arquivo <- "dados/Fechvolumediario2010a2019.xlsx"
#   cntdd.uneMatrix(Arquivo = arquivo, SeqVarPlan = c("preco", "volume"),
#                   index = c("cod", "data"),
#                   clsPer = "date", clsVlr = "numeric")
#   bd3 <- bdPainel %>% na.omit %>% data.table
#   attributes(bd3)$sorted <- NULL
# 
#   bdPainel <- merge.data.table(bdDiario, bd3, all = T)
#   
#   rm(bd1, bd2, bd3, bdDiario); gc()
#   
#   bdPainel <- bdPainel %>% arrange(cod, data)

# Processo de elegibilidade ####
  
  # Procedimentos Preliminares
  
  cntdd <- file.path("https://raw.githubusercontent.com",
                     "kleberformiga/contabilidados/master",
                     "contabilidados.R")
  
  source(cntdd)
  cntdd.carregaPacotes("tidyverse")
  
# <<<<<<< HEAD
#   setwd("G:/Meu Drive/criaCarteiras/R")
# =======
#   # setwd("G:/Meu Drive/criaCarteiras/R")
#   # setwd("C:/repos/criaCarteiras/R")
# 
# # Montagem das bases de dados ####
# 
#   # Quantidade de pregões na B3 por ano
# 
#   ddNeg <- read.csv("dados/numPregoes.csv",
#                     skip = 1, sep = ";") %>% 
#     mutate(lagQdeDiasTot = shift(qdeDiasTot)) %>%
#     select(ano, qdeDiasTot, lagQdeDiasTot) %>% na.omit
# 
#   # Baixa dados de preço (fechamento) e volume com função apropriada
#   # para matrix do Economatica
#   
#   # Arquivo 1994-1999
#   arquivo <- "dados/Fechvolumediario1994a1999.xlsx"
#   cntdd.uneMatrix(Arquivo = arquivo, SeqVarPlan = c("preco", "volume"),
#                   index = c("cod", "data"),
#                   clsPer = "date", clsVlr = "numeric")
#   bd1 <- bdPainel %>% filter(year(data) != 1993) %>%
#     na.omit %>% data.table
# >>>>>>> ajustes
  
  bd.ddPrecoVolume <- readRDS("dados/rds/basics/bd.ddPrecoVolume.rds")
  bd.yyQdeDiasNegoc <- readRDS("dados/rds/basics/bd.yyQdeDiasNegoc.rds")
  
  # Parâmetros
  
    param.volume <- 0 # valor absoluto (ver a unidade da base de dados)
    param.percdias <- 0.0  # valor percentual. Informar 0.1, se pretende 10%
  
  # Elegibilidade por:
  #  1) acao mais negociada (quando tiver duas por empresa)
  #  2) acao com mais de x% (param.percdias) de dias negociados no ano anterior
  #  3) acao com volume diário médio maior que $x (param.volume) no ano anterior
  
    # Indica o banco de dados
    bd.ddPrecoVolume %>%
    
    # Divide a data em três colunas: ano, mês e dia. Depois elimina a coluna dia
    separate(data, c("ano", "mes", "dia"), convert = T) %>% select(-dia) %>% 
    
    # Elimina os valores nulos
    na.omit %>%
    
    # Agrupa os dados por codigo e por ano
    group_by(cod, ano) %>%
      
    # Calcula a quantidade de dias negociados e o volume diário médio
    # por código e por ano (grupo)
    summarise(qdeDias = n(), volumeMedio = mean(volume, na.rm = T)) %>%
      
    # Une a base de dados com a base de dias de negociação da B3
    left_join(bd.yyQdeDiasNegoc, by = "ano") %>% 
      
    # Agrupa por empresa e por ano
    ungroup %>% group_by(empresa = substr(cod, 1, 4), ano) %>% 
    
    # Verifica qual dos papeis possui maior volume no ano anterior
    mutate(v.maxVol = volumeMedio == max(volumeMedio, na.rm = T)) %>%
    
    # Agrupa por papel
    ungroup %>% group_by(cod) %>% 
    
    # Verifica se o volume do ano anterior está dentro do parâmetro
    mutate(v.qdeVol = lag(volumeMedio) > param.volume) %>%
    
    # Verifica se o percentual de dias negociados no ano anterior
    # está dentro do parâmetro
    mutate(v.qdeDD = lag(qdeDias)/lag(qdeDiasTot) > param.percdias) %>% ungroup %>%
    
    # Idenfifica qual papel atende todos os parâmetros com o respectivo período
    mutate(atende = rowSums(select(., starts_with("v.")))) %>%
      
    # Filtra os papeis que atendem aos três parâmetros propostos
    filter(atende == 3) %>%
      
    # Seleciona código do papel e o ano
    select(cod, empresa, ano) -> bd.yyElegiveisQdeVol
    
    # Salva o banco de dados de papais elegíveis por quantidade de dias negociados
    # e volume, além de escolher a ação mais líquida - quando há mais de uma por empresa
    saveRDS(bd.yyElegiveisQdeVol,"dados/rds/bd.yyElegiveisQdeVol.rds")

# Gera lista com o ano e as empresas correspondentes
    bd.yyElegiveisQdeVol %>%
      group_split(ano) %>%
      setNames(sort(unique(bd.yyElegiveisQdeVol$ano))) -> lst.yyElegiveisQdeVol

    # Salva a lista Qde/Vol
    saveRDS(lst.yyElegiveisQdeVol, "dados/rds/lst.yyElegiveisQdeVol.rds") 
    sapply(lst.yyElegiveisQdeVol, nrow)
    
rm(cntdd, param.percdias, param.volume); gc()

# Fim