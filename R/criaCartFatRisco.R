# Gera carteiras e fator de risco
library(xts)

# Carteiras

  # Tamanho
  bd.portfolio %>%
    group_by(c.tamanho, ano, mes) %>%
    summarise(retTamanho = mean(retorno),
              wRetTamanho = weighted.mean(retorno, tamanho)) %>%
    arrange(ano, mes) -> portTamanho

  
  # book-to-market
  bd.portfolio %>%
    group_by(c.bm, ano, mes) %>%
    summarise(retBm = mean(retorno),
              wRetBm = weighted.mean(retorno, tamanho)) %>%
    arrange(ano, mes) -> portBm
  
  
  # Momento
  bd.portfolio %>%
    group_by(c.momento, ano, mes) %>%
    summarise(retMomento = mean(retorno),
              wRetMomento = weighted.mean(retorno, tamanho)) %>%
    arrange(ano, mes) -> portMomento
  
  
  # Tamanho e BM (2x2)
  bd.portfolio %>%
    filter(c.tamanho != "Medium" & c.bm != "Medium") %>% 
    group_by(c.tamanho, c.bm, ano, mes) %>%
    summarise(retTamBm = mean(retorno),
              wRetTamBm = weighted.mean(retorno, tamanho)) %>%
    arrange(ano, mes) -> portTamBm
  
  
  # Tamanho e Momento (2x2)
  bd.portfolio %>%
    filter(c.tamanho != "Medium" & c.momento != "Medium") %>% 
    group_by(c.tamanho, c.momento, ano, mes) %>%
    summarise(retTamMom = mean(retorno),
              wRetTamMom = weighted.mean(retorno, tamanho)) %>%
    arrange(ano, mes) -> portTamMom
  
# Fatores de risco
  
  # Mercado
  bd.portfolio %>% 
    group_by(ano, mes) %>%
    summarise(retMercado = mean(retorno),
              wRetMerc = weighted.mean(retorno, tamanho)) %>%
    arrange(ano, mes) -> fatorMercado

  # Small minus Big (SMB)
  portTamanho %>%
    filter(c.tamanho != "Medium") %>%
    mutate(retFator = ifelse(c.tamanho == "Big", wRetTamanho*-1, wRetTamanho)) %>% 
    group_by(ano, mes) %>%
    summarise(SMB = sum(retFator)) %>%
    arrange(ano, mes) %>% left_join(select(meses, mes.num, mes.chr),
                                        by = c("mes" = "mes.num")) %>% 
    mutate(data = as.Date(paste(ano, mes.chr, "01", sep = "-" ))) %>%
    ungroup %>% select(data, SMB) -> fatorTamanho
  
  fatorTamanho <- as.xts(fatorTamanho[, -1], order.by = as.yearmon(as.Date(fatorTamanho$data)))
  
  plot(fatorBm[ "2005/"], main =  "Fator Tamanho")

    
  # High minus Low (HML)
  portBm %>%
    filter(c.bm != "Medium") %>%
    mutate(retFator = ifelse(c.bm == "Low", wRetBm*-1, wRetBm)) %>% 
    group_by(ano, mes) %>%
    summarise(HML = sum(retFator)) %>%
    arrange(ano, mes) %>% left_join(select(meses, mes.num, mes.chr),
                                        by = c("mes" = "mes.num")) %>% 
    mutate(data = as.Date(paste(ano, mes.chr, "01", sep = "-" ))) %>%
    ungroup %>% select(data, HML) -> fatorBm
  
  fatorBm <- as.xts(fatorBm[, -1], order.by = as.yearmon(as.Date(fatorBm$data)))
  
  plot(fatorBm[ "2005/"], main =  "Fator BM")
  

  # Winners minus Losers (WML)
  portMomento %>%
    filter(c.momento != "Medium") %>%
    mutate(retFator = ifelse(c.momento == "Losers", wRetMomento*-1, wRetMomento)) %>% 
    group_by(ano, mes) %>%
    summarise(WML = sum(retFator)) %>%
    arrange(ano, mes) -> fatorMomento
  
  ggplot(fatorMomento, aes(x = 1:nrow(fatorMomento), y = WML)) +
    geom_line() +
    geom_line(aes(x = 1:nrow(fatorMomento), y = 0), color = "red")

# Reune fatores
  fatorTamanho %>%
    inner_join(fatorBm, by = c("ano", "mes")) %>%
    # inner_join(fatorMomento, by = c("ano", "mes")) %>%
    ungroup -> bd.fatores
  
  bd.fatores %>% 
    select(-ano, -mes) %>% cor %>% as.dist
  