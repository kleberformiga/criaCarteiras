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

# Instalando as funcoes do contabiliDados

  cntdd <- file.path("https://raw.githubusercontent.com",
                   "kleberformiga/contabilidados/master",
                   "contabilidados.R")

  source(cntdd)
  cntdd.carregaPacotes("tidyverse")

# Montagem das bases de dados ####

  # Quantidade de pregões na B3 por ano

  ddNeg <- readRDS("G:/Meu Drive/criaCarteiras/R/dados/diasNeg.rds") %>% 
  mutate(ano = as.integer(ano))

  # Baixa dados de preço (fechamento) e volume com função apropriada
  # para matrix do Economatica
  
  cntdd.uneMatrix(
    Arquivo = "G:/Meu Drive/criaCarteiras/R/dados/fechDiarioport.xlsx",
    SeqVarPlan = c("preco", "volume"),
    index = c("cod", "data"),
    clsPer = "date", clsVlr = "numeric"
  )

  gc()
  
  
# Preparando o filtro com percentual de dias negociados no ano anterior
# e volume diário do ano anterior

  # Parâmetros
    param.volume <- 500000 # valor absoluto (ver a unidade da base de dados)
    param.percdias <- 0.8  # valor percentual. Informar 0.1, se pretende 10%
  
  # Elegibilidade por:
  #  1) acao mais negociada (quando tiver duas por empresa)
  #  2) acao com mais de x% (param.percdias) de dias negociados no ano anterior
  #  3) acao com volume diário médio maior que $x (param.volume) no ano anterior
  
    # Indica o banco de dados
    bdPainel %>%
    
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
    left_join(ddNeg, by = "ano") %>% 
      
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
    select(cod, empresa, ano) -> elege.qdeVol
    

# Gera lista com o ano e as empresas correspondentes
    elege.qdeVol %>%
      group_split(ano) %>%
      setNames(sort(unique(elege.qdeVol$ano))) -> listaQdeVol

# Remove objetos desnecessários (limpa ambiente)
rm(bdPainel, ddNeg, cntdd, param.percdias, param.volume)