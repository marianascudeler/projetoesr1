library(readxl)

media_k8 <- read_xlsx('ancestralidade_k8_certo.xlsx', 
                      sheet = 'MEDIA_POP')                      
freqalelicamutantetodaspop001 <- read_xlsx('tabela_frequencia_cyp2c9.xlsx', 
                  sheet = 'FREQUENCIA_POR_POPULACAO')


alelos <- colnames(freqalelicamutantetodaspop001)
alelos <- alelos[-1]

colunas <- c("WEAS", "NEUR", "WAFR", 
             "NAT", "SEUR", "EAFR", 
             "EEAS", "SAS")

resultado_df <- data.frame(
  Frequencia = character(),
  Coluna = character(),
  Valor_p = numeric(),
  Valor_r = numeric(),
  stringsAsFactors = FALSE
)


#Loop para alterar sobre as frequências
for (frequencia in alelos) {
  Freq_column <- as.numeric(freqalelicamutantetodaspop001[[frequencia]])
  
  #Loop para alterar sobre as colunas
  for (coluna in colunas) {
    media_k8_column <- as.numeric(media_k8[[coluna]])
    
    #Realiza o teste de correlação para freqalelicamutantetodaspop001[[frequencia]] e media_k8[[coluna]]
    cor_test_2 <- cor.test(Freq_column, media_k8_column, method = "pearson")
    
    
    resultado_df <- rbind(resultado_df, data.frame(
      Frequencia = frequencia,
      Coluna = coluna,
      Valor_p = cor_test_2$p.value,
      Valor_r = cor_test_2$estimate
    ))
  }
}

library(clipr)
write_clip(resultado_df, dec = ",")