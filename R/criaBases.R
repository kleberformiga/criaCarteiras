# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# GERAÇÃO DA BASE DE DADOS                                              #
# Filtro por volume e liquidez (dias negociados)                        #
# Inputs:                                                               #
#        1) Planilha com quantitativo de dias negociados na B3          #
#        2) Planilha com preço de fechamento e volume diário            #
# Outputs:                                                              #
#        1) Banco de dados com código e ano dos papeis com maior volume #
#           por empresa que poderão compor o portfolio, baseado na      #
#           quantidade de dias negociados e volume médio anual          #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Instalando as funcoes do contabiliDados

cntdd <- file.path("https://raw.githubusercontent.com",
                   "kleberformiga/contabilidados/master",
                   "contabilidados.R")

source(cntdd)
cntdd.carregaPacotes("tidyverse")

setwd("G:/Meu Drive/criaCarteiras/R")


# 1) Quantidade de pregões anuais (bd.yyQdeDiasNegoc) ####

bd.yyQdeDiasNegoc <-
  read.csv("dados/numPregoes.csv", skip = 1, sep = ";") %>% 
  mutate(lagQdeDiasTot = shift(qdeDiasTot)) %>%
  select(ano, qdeDiasTot, lagQdeDiasTot) %>%
  tibble %>% na.omit

saveRDS(bd.yyQdeDiasNegoc, "dados/rds/basics/bd.yyQdeDiasNegoc.rds")



# 2) Preço e Volume diários (bd.ddPrecoVolume) ####

# Baixa dados de preço (fechamento) e volume com função apropriada
# para matrix do Economatica

# Arquivo 1994-1999
arquivo <- "dados/Fechvolumediario1994a1999.xlsx"
cntdd.uneMatrix(Arquivo = arquivo, SeqVarPlan = c("preco", "volume"),
                index = c("cod", "data"),
                clsPer = "date", clsVlr = "numeric")
bd1 <- bdPainel %>% filter(year(data) != 1993) %>%
  na.omit %>% data.table

# Arquivo 2000-2009
arquivo <- "dados/Fechvolumediario2000a2009.xlsx"
cntdd.uneMatrix(Arquivo = arquivo, SeqVarPlan = c("preco", "volume"),
                index = c("cod", "data"),
                clsPer = "date", clsVlr = "numeric")
bd2 <- bdPainel %>% filter(year(data) != 1999) %>% 
  na.omit %>% data.table

bdDiario <- merge.data.table(bd1, bd2, all = T)
attributes(bdDiario)$sorted <- NULL

# Arquivo 2010-2019
arquivo <- "dados/Fechvolumediario2010a2019.xlsx"
cntdd.uneMatrix(Arquivo = arquivo, SeqVarPlan = c("preco", "volume"),
                index = c("cod", "data"),
                clsPer = "date", clsVlr = "numeric")
bd3 <- bdPainel %>% na.omit %>% data.table
attributes(bd3)$sorted <- NULL

bd.ddPrecoVolume <- merge.data.table(bdDiario, bd3, all = T) %>% 
  arrange(cod, data) %>% tibble

rm(bd1, bd2, bd3, bdDiario, bdPainel); gc()

saveRDS(bd.ddPrecoVolume, "dados/rds/basics/bd.ddPrecoVolume.rds")



# 3) Retornos (bd.mmPrecoRetorno) ####

bd.ddPreco <- bd.ddPrecoVolume %>%
  select(-volume) %>% na.omit -> bd.ddPreco

# Os retornos mensais de cada empresa foram calculados a partir do
# preco do ultimo dia negociado em cada mes

# Indica a base de dados
bd.ddPreco %>%
  
  # Divide a data em três colunas: ano, mês e dia
  separate(data, c("ano", "mes", "dia"), convert = T) %>%
  
  # Classifica a base por código, ano, mês e dia. Elimina a coluna Dia
  arrange(cod, ano, mes, dia) %>% select(-dia) %>% 
  
  # Elimina os valores nulos
  na.omit %>%
  
  # Agrupa por código, ano e mês para selecionar apenas o último preço
  # disponível em cada mês, por empresa
  group_by(cod, ano, mes) %>%
  
  # Seleciona o último preço de cada mês, por empresa. Desagrupa a base de dados
  slice(n()) %>% ungroup %>%
  
  # Organiza a ordem das colunas
  select(cod, everything()) %>%
  
  # Agrupo por código para cálculo do retorno individual mensal
  group_by(cod) %>%
  
  # Calcula o retorno mensal de cada empresa
  mutate(retorno = (preco-lag(preco))/lag(preco)) %>%
  
  # Elimina a primeira observação de cada empresa perdida no
  # cálculo do retorno. Desagrupa a base de dados. Atribui o nome
  # bd.retornos ao objeto relativo ao banco de dados de retornos
  na.omit %>% ungroup -> bd.mmPrecoRetorno

saveRDS(bd.mmPrecoRetorno, "dados/rds/basics/bd.mmPrecoRetorno.rds") # Salva retornos

# 4) Retorno e Momento (bd.mmRetornoMomento) ####

# Partindo do banco de dados bd.mmPrecoRetorno criado na etapa anterior:
# Verifica quais empresas possuem retornos em todos os meses do ano, sendo
# calculado o momento de todas elas. A base com momento terá um ano a menos,
# pois o ano anterior de cada empresa será suprimido no cálculo da variável

# Indica a base de dados
bd.mmPrecoRetorno %>%
  
  # Agrupa por código e ano
  group_by(cod, ano) %>% 
  
  # Summariza os dados criando uma variável que conta quantos meses cada
  # empresa possui por ano
  summarise(qdeMes = n()) %>%
  
  # Filtra apenas as empresas cujos anos possuem todos os meses com retorno
  filter(qdeMes == 12) %>% ungroup %>% 
  
  # Agrupa por código
  group_by(cod) %>%
  
  # Verifica a existência de retornos nos anos anterior e posterior
  mutate(retornoPre = ifelse(lag(ano) == ano-1, ano, NA),
         retornoPos = ifelse(lead(ano) == ano+1, ano, NA)) %>%
  
  # Criado o banco com todos os códigos com 12 meses de retorno e identificada
  # a disponibilidade de retornos no ano anterior e posterior, une-se a base
  # de dados com todos os retornos
  left_join(select(bd.mmPrecoRetorno, cod, ano, mes, retorno),
            by = c("cod", "ano")) %>%
  
  # Calculo do momento
  # (caso o retorno anterior seja zero, assume valor do retorno igual a 0.01)
  mutate(
    momento =
      ifelse(lag(retorno, 12) == 0,
             ifelse(lag(ano, 12) == ano-1,
                    (lag(retorno, 2) - 0.01)/0.01, NA),
             ifelse(lag(ano, 12) == ano-1,
                    (lag(retorno, 2) - lag(retorno, 12))/abs(lag(retorno, 12)),
                    NA))) -> bd.mmRetornoMomento

# Salva retorno e momento
saveRDS(bd.mmRetornoMomento, "dados/rds/basics/bd.mmRetornoMomento.rds")


# Gera lista com o ano e as empresas correspondentes - Momento
bd.mmRetornoMomento %>% ungroup() %>% 
  group_split(ano) %>%
  setNames(sort(unique(bd.mmRetornoMomento$ano))) -> lst.mmRetornoMomento

saveRDS(lst.mmRetornoMomento, "dados/rds/basics/lst.mmRetornoMomento.rds") # Salva lista de retornos
sapply(lst.mmRetornoMomento, nrow)/12


# 5) Caracteristicas dos fatores (bd.mmCaracFator) ####

# Baixa dados de planilha com características desejadas com
# função apropriada para matrix do Economatica


# Arquivo 1994-1999
arquivo <- "dados/Datamensal1994a1999.xlsx"
cntdd.uneMatrix(Arquivo = arquivo,
                SeqVarPlan = c("patLiq" , "preco", "volume", "vrMerc"),
                index = c("cod", "data"),
                clsPer = "date", clsVlr = "numeric")
bd1 <- bdPainel %>% filter(year(data) != 1993) %>% data.table

# Arquivo 2000-2009
arquivo <- "dados/Datamensal2000a2009.xlsx"
cntdd.uneMatrix(Arquivo = arquivo,
                SeqVarPlan = c("patLiq" , "preco", "volume", "vrMerc"),
                index = c("cod", "data"),
                clsPer = "date", clsVlr = "numeric")
bd2 <- bdPainel %>% filter(year(data) != 1999) %>% data.table

bdDiario <- merge.data.table(bd1, bd2, all = T)
attributes(bdDiario)$sorted <- NULL

# Arquivo 2010-2019
arquivo <- "dados/Datamensal2010a2019.xlsx"
cntdd.uneMatrix(Arquivo = arquivo,
                SeqVarPlan = c("patLiq" , "preco", "volume", "vrMerc"),
                index = c("cod", "data"),
                clsPer = "date", clsVlr = "numeric")
bd3 <- bdPainel %>% data.table
attributes(bd3)$sorted <- NULL

bd.mmCaracFator <- merge.data.table(bdDiario, bd3, all = T) %>%
  arrange(cod, data) %>% 
  tibble

rm(bd1, bd2, bd3, bdDiario, bdPainel); gc()


saveRDS(bd.mmCaracFator, "dados/rds/basics/bd.mmCaracFator.rds")



# 6) Cria banco com dados mensais das características ####

# . 6.1 Tam-BM (bd.mmTamBm) ####
bd.mmCaracFator %>% 
  
  # Divide a data em três colunas: ano, mês e dia. Elimina a coluna Dia
  separate(data, c("ano", "mes", "dia"), convert = T) %>% select(-dia) %>%
  
  # Cria a coluna trimestre com base nos meses
  mutate(trim = mes/3) %>%
  mutate(trim = ifelse(trim <= 1, 1,
                       ifelse(trim <= 2, 2,
                              ifelse(trim <= 3, 3, 4)))) %>%
  
  # Classifica em ordem crescente por código, ano, trimestre e mês
  arrange(cod, ano, trim, mes) %>%
  
  # Agrupa por Código, ano e trimestre
  group_by(cod, ano, trim) %>%
  
  # Preenche os dois meses iniciais do trimestre com o valor do
  # último mes do trimestre de cada empresa
  mutate(patLiq = ifelse(is.infinite(max(patLiq, na.rm = T)), NA,
                         max(patLiq, na.rm = T))) %>%
  # fill(patLiq, .direction = "down") %>% # Demora muito
  # fill(patLiq, .direction = "up") %>% 
  
  # Retirar valores nulos (NA) das variáveis cujas caracteristicas sao usadas
  filter(patLiq > 0 & !is.na(vrMerc)) %>%
  
  # Gera a variável book-to-market (BM)
  ungroup %>% 
  mutate(tamanho = vrMerc/1000, bm = round(patLiq/vrMerc, 4)) %>%
  
  # Agrupa por código e por ano
  group_by(cod, ano) %>%
  
  # Verifica a quantidade de observações mensais por empresa e por ano  
  mutate(qdemes = n()) %>%
  
  # Filtra apenas as empresas com 12 meses de observações por ano
  filter(qdemes == 12) %>%
  
  # Seleciona as variáveis de interesse
  select(cod, ano, trim, mes, tamanho, bm) -> bd.mmTamBm

  saveRDS(bd.mmTamBm, "dados/rds/basics/bd.mmTamBm.rds")

  # . 6.2 Tam-BM-M0m (bd.mmTamBmMom) ####

bd.mmTamBm %>%
  inner_join(select(bd.mmRetornoMomento, cod, ano, mes, momento),
             by = c("cod", "ano", "mes")) %>% 
  na.omit -> bd.mmTamBmMom

  saveRDS(bd.mmTamBmMom, "dados/rds/basics/bd.mmTamBmMom.rds")

gc()
