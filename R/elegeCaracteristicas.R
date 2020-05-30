# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# GERAÇÃO DA BASE DE DADOS                                              #
# Definição das características a serem incorporadas nas carteiras      #
# Inputs:                                                               #
#        1) Planilha com dados referentes a tamanho, bm e momento       #
# Outputs:                                                              #
#        1) Banco de dados com código, ano, mes, tamanho e bm           #
#        2) Banco de dados com código, ano, mes, tamanho, bm e momento  #
# Observação:                                                           #
#        1) Outras caracteristicas poderão ser adicionadas, bastando    #
#        adicionar a fórmula correspondente ao código, bem como incluir #
#        a nova variável na lista                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Instalando as funcoes do contabiliDados


# Utilizando dados das etapas anteriores
    
  # source("R/elegeVolQdeDias.R")
  # source("R/elegeRetornos.R")

  # bd.retornos   <- readRDS("dados/rds/bd.retornos.rds")
  # elege.qdeVol  <- readRDS("dados/rds/elege.qdeVol.rds")
  # todosRetornos <- readRDS("dados/rds/todosRetornos.rds")

  
# Montagem das bases de dados ####

  # Baixa dados de planilha com características desejadas com
  # função apropriada para matrix do Economatica
  
  
#   # Arquivo 1994-1999
#   arquivo <- "dados/Datamensal1994a1999.xlsx"
#   cntdd.uneMatrix(Arquivo = arquivo,
#                   SeqVarPlan = c("patLiq" , "preco", "volume", "vrMerc"),
#                   index = c("cod", "data"),
#                   clsPer = "date", clsVlr = "numeric")
#   bd1 <- bdPainel %>% filter(year(data) != 1993) %>% data.table
#   
#   # Arquivo 2000-2009
#   arquivo <- "dados/Datamensal2000a2009.xlsx"
#   cntdd.uneMatrix(Arquivo = arquivo,
#                   SeqVarPlan = c("patLiq" , "preco", "volume", "vrMerc"),
#                   index = c("cod", "data"),
#                   clsPer = "date", clsVlr = "numeric")
#   bd2 <- bdPainel %>% filter(year(data) != 1999) %>% data.table
#   
#   bdDiario <- merge.data.table(bd1, bd2, all = T)
#   attributes(bdDiario)$sorted <- NULL
#   
#   # Arquivo 2010-2019
#   arquivo <- "dados/Datamensal2010a2019.xlsx"
#   cntdd.uneMatrix(Arquivo = arquivo,
#                   SeqVarPlan = c("patLiq" , "preco", "volume", "vrMerc"),
#                   index = c("cod", "data"),
#                   clsPer = "date", clsVlr = "numeric")
#   bd3 <- bdPainel %>% data.table
#   attributes(bd3)$sorted <- NULL
#   
#   bdPainel <- merge.data.table(bdDiario, bd3, all = T)
#   
#   rm(bd1, bd2, bd3, bdDiario); gc()
#   
#   gc()
#   
# # Cria banco com dados mensais das características ####
# 
#   # Procedimentos Preliminares
#   
#   cntdd <- file.path("https://raw.githubusercontent.com",
#                      "kleberformiga/contabilidados/master",
#                      "contabilidados.R")
#   
#   source(cntdd)
#   cntdd.carregaPacotes("tidyverse")
#   
#   setwd("G:/Meu Drive/criaCarteiras/R") 
  
#   bd.mmCaracFator <- readRDS("dados/rds/bd.mmCaracFator.rds")
#   
#   # Indica a base de dados
#   bd.mmCaracFator %>% 
#   
#   # Divide a data em três colunas: ano, mês e dia. Elimina a coluna Dia
#   separate(data, c("ano", "mes", "dia"), convert = T) %>% select(-dia) %>%
#   
#   # Cria a coluna trimestre com base nos meses
#   mutate(trim = mes/3) %>%
#   mutate(trim = ifelse(trim <= 1, 1,
#                          ifelse(trim <= 2, 2,
#                                 ifelse(trim <= 3, 3, 4)))) %>%
#   
#   # Classifica em ordem crescente por código, ano, trimestre e mês
#   arrange(cod, ano, trim, mes) %>%
#   
#   # Agrupa por Código, ano e trimestre
#   group_by(cod, ano, trim) %>%
#   
#   # Preenche os dois meses iniciais do trimestre com o valor do
#   # último mes do trimestre de cada empresa
#   mutate(patLiq = ifelse(is.infinite(max(patLiq, na.rm = T)), NA,
#                          max(patLiq, na.rm = T))) %>%
#   # fill(patLiq, .direction = "down") %>% # Demora muito
#   # fill(patLiq, .direction = "up") %>% 
#   
#   # Retirar valores nulos (NA) das variáveis cujas caracteristicas sao usadas
#   filter(patLiq > 0 & !is.na(vrMerc)) %>%
#   
#   # Gera a variável book-to-market (BM)
#   ungroup %>% 
#   mutate(tamanho = vrMerc/1000, bm = round(patLiq/vrMerc, 4)) %>%
# 
#   # Agrupa por código e por ano
#   group_by(cod, ano) %>%
#   
#   # Verifica a quantidade de observações mensais por empresa e por ano  
#   mutate(qdemes = n()) %>%
#   
#   # Filtra apenas as empresas com 12 meses de observações por ano
#   filter(qdemes == 12) %>%
#   
#   # Seleciona as variáveis de interesse
#   select(cod, ano, trim, mes, tamanho, bm) -> bd.mmTamBm
# 
# # Cria banco com momento ####
#   
#   bd.mmRetornoMomento <- readRDS("dados/rds/bd.mmRetornoMomento.rds")
# 
#   bd.mmTamBm %>%
#     inner_join(select(bd.mmRetornoMomento, cod, ano, mes, momento),
#              by = c("cod", "ano", "mes")) %>% 
#     na.omit -> bd.mmTamBmMom
#   
#   gc()
#   
#   
  
  
  
  # Parâmetros
  carteiras    <- c("c.tamanho", "c.bm")
  nomeVariavel <- c("tamanho", "bm")
  rotulo       <- list(c("Small", "Big"), c("Low", "Medium", "High"))
  # probCart     <- list(0:length(rotulo[[1]])/length(rotulo[[1]]),
  #                      0:length(rotulo[[2]])/length(rotulo[[2]]))
  probCart   <- list(c(0, 0.5, 1), c(0, 0.333333, 0.66666, 1)) # Manual
  mesCaracter  <- list(12, 12)
  
  length(carteiras) == mean(c(length(nomeVariavel), length(rotulo), length(probCart), length(mesCaracter)))


  mesRebalanc <- 6
  
  
  
  
  
  # Inicio da Função ####  
  
  cntdd <- file.path("https://raw.githubusercontent.com",
                     "kleberformiga/contabilidados/master",
                     "contabilidados.R")
  source(cntdd)
  cntdd.carregaPacotes("tidyverse")
  
  setwd("G:/Meu Drive/criaCarteiras/R") 
  
  
  # Classifica as empresas, conforme parâmetros
  
  bd.yyElegiveisQdeVol <- readRDS("dados/rds/bd.yyElegiveisQdeVol.rds")
  bd.mmTamBm <- readRDS("dados/rds/basics/bd.mmTamBm.rds")
  bd.mmPrecoRetorno <- readRDS("dados/rds/basics/bd.mmPrecoRetorno.rds")
  
  select(bd.yyElegiveisQdeVol,-empresa) %>%
    inner_join(bd.mmTamBm, by = c("cod", "ano")) %>%
    inner_join(select(bd.mmPrecoRetorno, cod, ano, mes, retorno),
               by = c("cod", "ano", "mes")) %>%
    arrange(cod, ano, trim, mes) %>% filter(ano > 1993) %>% 
    na.omit %>% setDT -> bd.mmFatoresRetorno
  
  bd.mmFatoresRetorno %>%
    group_by(cod, ano) %>% 
    summarise(tamanho = mean(tamanho)) %>%
    ungroup() %>%
    group_by(ano) %>% 
    mutate(filtro = cut(tamanho, quantile(tamanho, probs = c(0, 0.7, 1)),
        labels = c("menor30","maior30"), include.lowest = T)) %>% 
    select(-tamanho) %>% 
    inner_join(bd.mmFatoresRetorno, by = c("cod", "ano")) %>% 
    # filter(filtro == "maior30") %>% 
    setDT -> bd.mmFatoresRetorno
  
  bd.mmFatoresRetorno %>% 
    group_split(ano, mes) %>%
    setNames(sort(unique(bd.mmFatoresRetorno$ano))) -> lst.mmFatoresRetorno
  
  
  for (i in 1:length(lst.mmFatoresRetorno)) {
    names(lst.mmFatoresRetorno)[i] <- paste(as.character(unique(lst.mmFatoresRetorno[[i]][2])),
                                    as.character(unique(lst.mmFatoresRetorno[[i]][4])),
                                    sep = ".")
  }  

  bd.mmObsFatoresRetorno <- unlist(lapply( lst.mmFatoresRetorno, nrow ))
  
  saveRDS(lst.mmFatoresRetorno, "dados/rds/lst.mmFatoresRetorno.rds") 
  saveRDS(bd.mmObsFatoresRetorno, "dados/rds/bd.mmObsFatoresRetorno.rds") 
          
    
# Monta características do portfolio


  bd.caracteristicas <- list()
  
  for (i in seq_along(carteiras)) {
    sort <- c("ano", carteiras[-length(carteiras)])[1:i]

    bd.mmFatoresRetorno[mes == mesCaracter[[i]], (carteiras[i]) :=
        cut(get(nomeVariavel[i]), quantile(get(nomeVariavel[i]), probs = probCart[[i]]),
            labels = rotulo[[i]], include.lowest = T),
      by = sort]
    
    bd.mmFatoresRetorno <<-
      bd.mmFatoresRetorno %>% 
      group_by(cod, ano) %>% 
      #preenchendo os dois meses iniciais do trimestre com o valor do ultimo mes do trimestre
      fill(!! carteiras[i], .direction = "up") %>% 
      fill(!! carteiras[i], .direction = "down") %>% na.omit %>%  setDT
    
    bd.caracteristicas[[i]] <- bd.mmFatoresRetorno %>% unique
    
  }

  saveRDS(bd.mmFatoresRetorno, "dados/rds/bd.mmFatoresRetorno.rds")
  
# Define o período de portfolio vs balanceamento
  
  
  if(mesRebalanc == 12){
    bd.mmFatoresRetorno %>%
      mutate(anoport = ano+1, posReb = 1) %>%
      group_by(cod, anoport) %>% 
      mutate(qde = n()) %>% 
      filter(qde == 12) %>% 
      ungroup %>% 
      select(cod, ano, mes, starts_with("c."), retorno, tamanho) %>% 
      arrange(cod, ano, mes) -> bd.portfolio
  } else{
    bd.mmFatoresRetorno %>%
      mutate(anoport = ifelse(mes <= mesRebalanc, ano, ano+1),
             posReb  = ifelse(mes > mesRebalanc, 1, 0)) %>%
      group_by(cod, anoport) %>% 
      mutate(qde = n()) %>% 
      filter(qde == 12 | posReb == 1) %>% 
      ungroup %>% 
      select(cod, ano, mes, starts_with("c."), retorno, tamanho) %>% 
      arrange(cod, ano, mes) -> bd.portfolio
  }
  
  
  saveRDS(bd.portfolio, "dados/rds/bd.portfolio.rds")
      

# Cria lista de empresas por ano

  bd.portfolio %>%
    group_split(ano) %>%
    setNames(sort(unique(bd.portfolio$ano))) -> listaPortfAno
  
  lapply(listaPortfAno, nrow) %>% unlist
  

# Gera lista com o ano e as empresas correspondentes
  bd.portfolio %>% 
    group_split(ano, mes) -> listaPortAnoMes
  
  for (i in 1:length(listaPortAnoMes)) {
    names(listaPortAnoMes)[i] <- 
    paste(as.numeric(unique(listaPortAnoMes[[i]][2])),
          as.numeric(unique(listaPortAnoMes[[i]][3])),
          sep = "-")
    
  }

  
  # saveRDS(listaQdeRet, "dados/listaretornos.rds") # Salva lista de retornos
   sapply(listaPortAnoMes, nrow)
  
   