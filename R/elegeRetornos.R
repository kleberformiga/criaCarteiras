#########################################################################
# GERAÇÃO DA BASE DE DADOS                                              #
# Filtro por retornos disponiveis nos 12 meses anteriores e posteriores #
# Inputs:                                                               #
#        1) Planilha com preço de fechamento                            #
# Outputs:                                                              #
#        1) Banco de dados com código, ano, mes e retorno de cada acao  #
#        2) Banco de dados com calculo do momento de cada acao          #
# Observação:                                                           #
#           Usar o banco de dados de momento implica em perder 1 (um)   #
#           ano de dados referente ao ano anterior necessario para o    #
#           calculo da variável momento. Caso a carteira seja formada   #
#           por outras variaveis, tem-se aumento de um ano de dados     #
#########################################################################

# Processo de elegibilidade ####
  
  # saveRDS(bdPainel, "dados/FechVolDiario.rds")
  bdPainel %>% select(-volume) %>% na.omit -> BDFECHAMENTO

    
# Gera planilha com retornos mensais de todas as empresas da base
  
  # Os retornos mensais de cada empresa foram calculados a partir do
  # preco do ultimo dia negociado em cada mes
  
    # Indica a base de dados
    BDFECHAMENTO %>%
    
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
    na.omit %>% ungroup -> todosRetornos
    
    # saveRDS(todosRetornos, "dados/todosRetornos.rds") # Salva BD TodosRetornos
    
# Partindo do banco de dados bd.retornos criado na etapa anterior:
    # Verifica quais empresas possuem retornos em todos os meses do ano, sendo
    # calculado o momento de todas elas. A base com momento terá um ano a menos,
    # pois o ano anterior de cada empresa será suprimido no cálculo da variável
    
    # Indica a base de dados
    todosRetornos %>%
    
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
    left_join(select(todosRetornos, cod, ano, mes, retorno), by = c("cod", "ano")) %>%
    
    # Calculo do momento
    # (caso o retorno anterior seja zero, assume valor do retorno igual a 0.01)
    mutate(
      momento =
        ifelse(lag(retorno, 12) == 0,
               ifelse(lag(ano, 12) == ano-1,
                      (lag(retorno, 2) - 0.01)/0.01, NA),
               ifelse(lag(ano, 12) == ano-1,
                      (lag(retorno, 2) - lag(retorno, 12))/abs(lag(retorno, 12)),
                      NA))) -> bd.retornos
    
    # saveRDS(bd.retornos, "dados/bd.retornos.rds") # Salva BD retornos


# Gera lista com o ano e as empresas correspondentes
    bd.retornos %>% ungroup() %>% 
      group_split(ano) %>%
      setNames(sort(unique(bd.retornos$ano))) -> listaQdeRet
    
    # saveRDS(listaQdeRet, "dados/listaretornos.rds") # Salva lista de retornos
    sapply(listaQdeRet, nrow)/12

# Remove objetos desnecessários (limpa ambiente)
rm(BDFECHAMENTO)
