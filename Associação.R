library(openxlsx)
library(clipr)
library(dplyr)

#Lê o arquivo da Frequência Alélica e cria uma lista com os SNPs
#arranjar uma lista de SNPs
df <- read.xlsx(file.choose())

nomes_colunas <- names(df)
nomes_colunas <- gsub("\\.1$", "", nomes_colunas)
# Excluir colunas que terminam com .0
colunas_para_excluir <- grep("\\.0$", nomes_colunas, value = TRUE)
df <- df[, !names(df) %in% colunas_para_excluir]
# Renomear colunas que terminam com .1
nomes_colunas <- names(df)
nomes_colunas <- gsub("\\.1$", "", nomes_colunas)
# Atualizar os nomes das colunas no dataframe 'df'
names(df) <- nomes_colunas


lista_snps.1 <- unique(grep("^.+(.)$",names(df), value=TRUE, perl = TRUE))
lista_snps <- lista_snps.1[lista_snps.1 != c("POP", "NAT2","AFRL", "EASL", "SAS", "NAT", "EURN", "AFRO", "EURS", "EASO")]
lista_snps <- gsub("\\.1$", "", lista_snps)


calcular_correlacoes <- function(arquivo_excel, lista_snps) {
  
  
  # cria uma lista vazia para armazenar os resultados
  resultados <- list()
  
  # loop pelos SNPs na lista
  for (snp in lista_snps) {
    # converte a coluna do SNP para numérico
    df[[snp]] <- as.numeric(df[[snp]])
    media_k9 <- data.frame(df$NAT2, df$AFRL, df$EASL, df$SAS,
                           df$NAT, df$EURN, df$AFRO, df$EURS, df$EASO)
    NAT2 <- as.numeric(media_k9$df.NAT2)
    AFRL <- as.numeric(media_k9$df.AFRL)
    EASL <- as.numeric(media_k9$df.EASL)
    SAS <- as.numeric(media_k9$df.SAS)
    NAT <- as.numeric(media_k9$df.NAT)
    EURN <- as.numeric(media_k9$df.EURN)
    AFRO <- as.numeric(media_k9$df.AFRO)
    EURS <- as.numeric(media_k9$df.EURS)
    EASO <- as.numeric(media_k9$df.EASO)
    
    
    
    
    # realiza as correlações com cada ancestralidade
    
    cNAT2 <- cor.test(df[[snp]], NAT2, method = "spearman")
    cAFRL <- cor.test(df[[snp]], AFRL, method = "spearman")
    cEASL <- cor.test(df[[snp]], EASL, method = "spearman")
    cSAS <- cor.test(df[[snp]], SAS, method = "spearman")
    cNAT <- cor.test(df[[snp]], NAT, method = "spearman")
    cEURN <- cor.test(df[[snp]], EURN, method = "spearman")
    cAFRO <- cor.test(df[[snp]], AFRO, method = "spearman")
    cEURS <- cor.test(df[[snp]], EURS, method = "spearman")
    cEASO <- cor.test(df[[snp]], EASO, method = "spearman")
    
    
    # realiza as regressões com cada ancestralidade
    reg_NAT2 <- lm(df[[snp]] ~ NAT2)
    reg_AFRL <- lm(df[[snp]] ~ AFRL)
    reg_EASL <- lm(df[[snp]] ~ EASL)
    reg_SAS <- lm(df[[snp]] ~ SAS)
    reg_NAT <- lm(df[[snp]] ~ NAT)
    reg_EURN <- lm(df[[snp]] ~ EURN)
    reg_AFRO <- lm(df[[snp]] ~ AFRO)
    reg_EURS <- lm(df[[snp]] ~ EURS)
    reg_EASO <- lm(df[[snp]] ~ EASO)
   
     lista_ANCES <- unique(grep("^.+()$",names(media_k9), value=TRUE, perl = TRUE))
     lista_ANCES <- gsub("^df\\.", "", lista_ANCES)
     
     resultados_df <- data.frame(beta = numeric(), p_value = numeric(), 
                                 adjusted_r_squared = numeric(), ancestralidade = character())
     
     for(nome in lista_ANCES) {
       summary_reg <- summary(get(paste0("reg_", nome)))
       beta <- summary_reg$coefficients[nome, "Estimate"]
       p_value <- summary_reg$coefficients[nome, "Pr(>|t|)"]
       adjusted_r_squared <- summary_reg$adj.r.squared
       
       resultados_df <- rbind(resultados_df, 
                              data.frame(Ancestralidade = nome, Beta = beta, p_regr = p_value, 
                                         R_sqr_ajustado = adjusted_r_squared))
     }
     

     
     mariscudelersentiremossaudades <- data.frame(
       SNP = lista_snps <- gsub('\\.1','',snp),
       resultados_df,
       p_corr = c(cNAT2$p.value, cAFRL$p.value, cEASL$p.value, cSAS$p.value,
                   cNAT$p.value, cEURN$p.value, cAFRO$p.value, cEURS$p.value,
                   cEASO$p.value),
       R_corr = c(cNAT2$estimate, cAFRL$estimate, cEASL$estimate, cSAS$estimate,
                   cNAT$estimate, cEURN$estimate, cAFRO$estimate, cEURS$estimate,
                   cEASO$estimate)
     )

      # adiciona o data frame à lista de resultados
      resultados[[snp]] <- mariscudelersentiremossaudades

  }
  
  # retorna a lista de resultados
return(resultados)
}

# Chama a função e armazena o resultado
resultados <- calcular_correlacoes(arquivo_excel = "caminho_para_o_arquivo.xlsx", lista_snps = lista_snps)

# Combina os dataframes individuais em um único dataframe
CorLista <- dplyr::bind_rows(resultados)

# Exporta os resultados para um arquivo Excel
write.xlsx(CorLista, "C:/Users/guilh/OneDrive/Área de Trabalho/PGx/MariESR1/ESR1_Corr_Regr_Spearman.xlsx", rowNames = FALSE)
