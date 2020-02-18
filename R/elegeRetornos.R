#########################################################################
# GERAÇÃO DA BASE DE DADOS                                              #
# Filtro por retornos disponiveis nos 12 meses anteriores e posteriores #
# Inputs:                                                               #
#        1) Planilha com preço de fechamento e volume diário            #
# Outputs:                                                              #
#        1) Banco de dados com código, ano, mes e retorno de cada acao  #
#        2) Banco de dados com calculo do momento de cada acao          #
# Observação:                                                           #
#           Usar o banco de dados de momento implica em perder 1 (um)   #
#           ano de dados referente ao ano anterior necessario para o    #
#           calculo da variável momento. Caso a carteira seja formada   #
#           por outras variaveis, tem-se aumento de um ano de dados     #
#########################################################################

# Instalando as funcoes do contabiliDados

  cntdd <- file.path("https://raw.githubusercontent.com",
                     "kleberformiga/contabilidados/master",
                     "contabilidados.R")
  source(cntdd)
  cntdd.carregaPacotes("tidyverse")

# Montagem das bases de dados ####

  # Baixa dados de preço (fechamento) com função apropriada
  # para matrix do Economatica

  cntdd.BaixaDados("bdPainel",
    PathFile = "G:/Meu Drive/criaCarteiras/R/dados/fechDiarioport.xlsx",
    Periodo = "dia",
    Planilha =  "Fechamento", ClassPeriodo = "date", ClassValue = "numeric"
  )

  gc()
  
  
  # Os retornos mensais de cada empresa foram calculados a partir do
  # preco do ultimo dia negociado em cada mes
  
    # Indica a base de dados
    bdPainel %>%
    
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
    mutate(retorno = log(preco/lag(preco))) %>%
  
    # Elimina a primeira observação de cada empresa perdida no
    # cálculo do retorno. Desagrupa a base de dados. Atribui o nome
    # bd.retornos ao objeto relativo ao banco de dados de retornos
    na.omit %>% ungroup -> bd.retornos
    
    
  # filtro de empresas com retornos em todos os meses do ano e com informacoes
  # disponiveis no anterior e seguinte
  todosRetornos <<-
    retornos %>%
    group_by(cod, ano) %>%
    summarise(qdeMes = n()) %>%
    filter(qdeMes == 12) %>% #filtramos a qtd de meses igual a 12 para ter todos os meses do ano
    group_by(cod) %>% 
    #verificamos se exista valores no ano anterior e ano posterior ao ano avaliado
    mutate(portf = ifelse(shift(ano) == ano-1 & shift(ano, type = "lead") == ano+1, ano, NA)) %>% 
    # banco de dados com cod das empresas e anos que possuem dados do ano anterior e posterior 
    # buscar os retornos mensais dos anos validos e correspondentes
    left_join(select(retornos, cod, ano, mes, retorno), by = c("cod", "ano")) %>% 
    # calculo do momento
    mutate(momento = (shift(retorno, 2) / shift(retorno, 12))-1)
  