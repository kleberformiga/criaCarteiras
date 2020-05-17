# Gera carteiras e fator de risco


# Carteiras

  # Mercado
  bd.portfolio %>% 
    group_by(anoport, mes) %>%
    summarise(retMercado = mean(retorno)) %>%
    arrange(anoport, mes) -> portMercado


  # Tamanho
  bd.portfolio %>%
    group_by(c.tamanho, anoport, mes) %>%
    summarise(retTamanho = mean(retorno)) %>%
    arrange(anoport, mes) -> portTamanho

  
  # book-to-market
  bd.portfolio %>%
    group_by(c.bm, anoport, mes) %>%
    summarise(retBm = mean(retorno)) %>%
    arrange(anoport, mes) -> portBm
  
  
  # Momento
  bd.portfolio %>%
    group_by(c.momento, anoport, mes) %>%
    summarise(retMomento = mean(retorno)) %>%
    arrange(anoport, mes) -> portMomento
  
  
  # Tamanho e BM (2x2)
  bd.portfolio %>%
    filter(c.tamanho != "Medium" & c.bm != "Medium") %>% 
    group_by(c.tamanho, c.bm, anoport, mes) %>%
    summarise(retTamBm = mean(retorno)) %>%
    arrange(anoport, mes) -> portTamBm
  
  
  # Tamanho e Momento (2x2)
  bd.portfolio %>%
    filter(c.tamanho != "Medium" & c.momento != "Medium") %>% 
    group_by(c.tamanho, c.momento, anoport, mes) %>%
    summarise(retTamMom = mean(retorno)) %>%
    arrange(anoport, mes) -> portTamMom
  
# Fatores de risco

  # Small minus Big (SMB)
  bd.portfolio %>%
    filter(c.tamanho != "Medium") %>%
    mutate(retFator = ifelse(c.tamanho == "Big", retorno*-1, retorno)) %>% 
    group_by(anoport, mes) %>%
    summarise(SMB = sum(retFator)) %>%
    arrange(anoport, mes) -> fatorTamanho
  
  ggplot(fatorTamanho, aes(x = 1:nrow(fatorTamanho), y = SMB)) +
    geom_line(color = "blue") +
    geom_line(aes(x = 1:nrow(fatorTamanho), y = 0), color = "red") +
    labs(title = "Evolução do Fator Tamanho (SMB)", subtitle = "Período de 2010 a 2017",
         caption = paste0("@2020 contabiliDados \n", "Fonte: Economática")) + cntdd.theme

    

    


  # High minus Low (HML)
  bd.portfolio %>%
    filter(c.bm != "Medium") %>%
    mutate(retFator = ifelse(c.bm == "Low", retorno*-1, retorno)) %>% 
    group_by(anoport, mes) %>%
    summarise(HML = sum(retFator)) %>%
    arrange(anoport, mes) -> fatorBm
  
  ggplot(fatorBm, aes(x = 1:nrow(fatorBm), y = HML)) +
    geom_line() +
    geom_line(aes(x = 1:nrow(fatorBm), y = 0), color = "red")

  
  # Winners minus Losers (WML)
  bd.portfolio %>%
    filter(c.momento != "Medium") %>%
    mutate(retFator = ifelse(c.momento == "Losers", retorno*-1, retorno)) %>% 
    group_by(anoport, mes) %>%
    summarise(WML = sum(retFator)) %>%
    arrange(anoport, mes) -> fatorMomento
  
  ggplot(fatorMomento, aes(x = 1:nrow(fatorMomento), y = WML)) +
    geom_line() +
    geom_line(aes(x = 1:nrow(fatorMomento), y = 0), color = "red")

# Reune fatores
  fatorTamanho %>%
    inner_join(fatorBm, by = c("anoport", "mes")) %>%
    inner_join(fatorMomento, by = c("anoport", "mes")) %>%
    ungroup -> bd.fatores
  
  bd.fatores %>% 
    select(-anoport, -mes) %>% cor %>% as.dist
  