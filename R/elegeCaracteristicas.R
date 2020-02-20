#########################################################################
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
#########################################################################

# Instalando as funcoes do contabiliDados

  cntdd <- file.path("https://raw.githubusercontent.com",
                     "kleberformiga/contabilidados/master",
                     "contabilidados.R")
  source(cntdd)
  cntdd.carregaPacotes("tidyverse")

# Montagem das bases de dados ####

  # Baixa dados de planica com características desejadas com
  # função apropriada para matrix do Economatica
  
  cntdd.uneMatrix("G:/Meu Drive/criaCarteiras/R/dados/Data.xlsx",
                  SeqVarPlan = c("patLiq" , "preco", "volume", "vrMerc"),
                  index = c("cod", "data"), clsPer = "date", clsVlr = "numeric")

  
  gc()
  
# Gera planilha com dados mensais das características
  
  # Indica a base de dados
  bdPainel %>%
  
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
  fill(patLiq, .direction = "up") %>% 
  
  # Retirar valores nulos (NA) das variáveis cujas caracteristicas sao usadas
  filter(patLiq > 0 & !is.na(vrMerc)) %>%
  
  # Gera a variável book-to-market (BM)
  mutate(tamanho = round(log(vrMerc), 4), bm = round(patLiq/vrMerc, 4)) %>%
  
  # Agrupa por código e por ano
  group_by(cod, ano) %>%
  
  # Verifica a quantidade de observações mensais por empresa e por ano  
  mutate(qdemes = n()) %>%
  
  # Filtra apenas as empresas com 12 meses de observações por ano
  filter(qdemes == 12) %>%
  
  # Seleciona as variáveis de interesse
  select(cod, ano, trim, mes, tamanho, bm) -> bd.TamBm
  
  gc()

# Cria banco com momento

  bd.TamBm %>%
    left_join(
      select(bd.retornos, cod, ano, mes, momento),
             by = c("cod", "ano", "mes")) %>% 
    na.omit -> bd.TamBmMom