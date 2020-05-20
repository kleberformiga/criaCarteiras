# Gera carteiras e fator de risco


# Carteiras

  # Mercado
  bd.portfolio %>% 
    group_by(anoport, mes) %>%
    summarise(retMercado = mean(retorno),
              wRetMerc = weighted.mean(retorno, tamanho)) %>%
    arrange(anoport, mes) -> portMercado


  # Tamanho
  bd.portfolio %>%
    group_by(c.tamanho, anoport, mes) %>%
    summarise(retTamanho = mean(retorno),
              wRetTamanho = weighted.mean(retorno, tamanho)) %>%
    arrange(anoport, mes) -> portTamanho

  
  # book-to-market
  bd.portfolio %>%
    group_by(c.bm, anoport, mes) %>%
    summarise(retBm = mean(retorno),
              wRetBm = weighted.mean(retorno, tamanho)) %>%
    arrange(anoport, mes) -> portBm
  
  
  # Momento
  bd.portfolio %>%
    group_by(c.momento, anoport, mes) %>%
    summarise(retMomento = mean(retorno),
              wRetMomento = weighted.mean(retorno, tamanho)) %>%
    arrange(anoport, mes) -> portMomento
  
  
  # Tamanho e BM (2x2)
  bd.portfolio %>%
    filter(c.tamanho != "Medium" & c.bm != "Medium") %>% 
    group_by(c.tamanho, c.bm, anoport, mes) %>%
    summarise(retTamBm = mean(retorno),
              wRetTamBm = weighted.mean(retorno, tamanho)) %>%
    arrange(anoport, mes) -> portTamBm
  
  
  # Tamanho e Momento (2x2)
  bd.portfolio %>%
    filter(c.tamanho != "Medium" & c.momento != "Medium") %>% 
    group_by(c.tamanho, c.momento, anoport, mes) %>%
    summarise(retTamMom = mean(retorno),
              wRetTamMom = weighted.mean(retorno, tamanho)) %>%
    arrange(anoport, mes) -> portTamMom
  
# Fatores de risco

  # Small minus Big (SMB)
  portTamanho %>%
    filter(c.tamanho != "Medium") %>%
    mutate(retFator = ifelse(c.tamanho == "Big", wRetTamanho*-1, wRetTamanho)) %>% 
    group_by(anoport, mes) %>%
    summarise(SMB = sum(retFator)) %>%
    arrange(anoport, mes) -> fatorTamanho
  
  ggplot(fatorTamanho, aes(x = 1:nrow(fatorTamanho), y = SMB)) +
    geom_line(color = "blue") +
    geom_line(aes(x = 1:nrow(fatorTamanho), y = 0), color = "red") +
    labs(title = "Evolução do Fator Tamanho (SMB)", subtitle = "Período de 2010 a 2017",
         caption = paste0("@2020 contabiliDados \n", "Fonte: Economática")) + cntdd.theme

    

    


  # High minus Low (HML)
  portBm %>%
    filter(c.bm != "Medium") %>%
    mutate(retFator = ifelse(c.bm == "Low", wRetBm*-1, wRetBm)) %>% 
    group_by(anoport, mes) %>%
    summarise(HML = sum(retFator)) %>%
    arrange(anoport, mes) -> fatorBm
  
  ggplot(fatorBm, aes(x = 1:nrow(fatorBm), y = HML)) +
    geom_line() +
    geom_line(aes(x = 1:nrow(fatorBm), y = 0), color = "red")

  
  # Winners minus Losers (WML)
  portMomento %>%
    filter(c.momento != "Medium") %>%
    mutate(retFator = ifelse(c.momento == "Losers", wRetMomento*-1, wRetMomento)) %>% 
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
  