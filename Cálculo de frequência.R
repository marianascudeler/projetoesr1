#este comando chama o arquivo vcf, onde tem os genotipos
library(vcfR)
library(adegenet)
library(dplyr)
library(readxl)
vcf <- read.vcfR(file.choose())

 gt <- vcf@gt
 id <-vcf@fix

#o comando acima chamou o elemento fix pois precisamos da coluna de ID, mas o elemento fix tem várias outras colunas. para ficar só a coluna de ID: 
 id <- as.data.frame(id)
 id <- id$ID
#o comando abaixo foi utilizado para que a coluna ID ficasse como coluna 1 e os dados de gt ficassem depois
df <- cbind(id, gt)

#eh necessario fazer isso porque o comando faz o calculo das freq a partir de uma unica coluna
df <- as.data.frame(df)
transposto <- as.data.frame(t(df))
#esse comando colocou como nome das colunas a primeira linha do dataframe transposto
colnames(transposto) <- transposto[1, ]
#para excluir as duas primeiras linhas (onde tem os rs e a linha format)
transposto <- transposto[c(-1,-2),]

IDPOP <-read_xlsx(choose.files(),sheet = "ID_POP", col_names = TRUE)
#abaixo para que receba só a coluna cod
IDPOP <- IDPOP$cod
#agora para juntar cod de IDPOP com transposto:
transposto <- cbind(IDPOP, transposto)
#os genotipos tem um | entre os alelos. alem disso, tem uma populacao com duas pop, pra corrigir isso:
transposto[transposto == 'IBS,MSL'] <- "IBS"
transposto[transposto == '1|0'] <- "01"
transposto[transposto == '1|1'] <- "11"
transposto[transposto == '0|1'] <- "01"
transposto[transposto == '0|0'] <- "00"
transposto[transposto == '2|0'] <- "20"
transposto[transposto == '0|2'] <- "02"
transposto[transposto == '2|1'] <- "21"
transposto[transposto == '1|2'] <- "12"
transposto[transposto == '2|2'] <- "22"
#abaixo, correcao de um erro que apareceu, pedindo pra trocar . por _
colnames(transposto) <- gsub("\\.", "_", colnames(transposto))

lista_pop <- c ('LWK', 'ESN', 'YRI', 'MSL', 'GWD', 
                'ACB', 'ASW', 'CLM', 'MXL', 'PUR', 'PEL',
                'TSI', 'IBS', 'GBR', 'CEU', 'FIN',
                'PJL', 'GIH', 'ITU', 'STU', 'BEB',
                'CDX', 'KHV', 'CHS', 'CHB', 'JPT')

# Renomear as colunas duplicadas
colnames(transposto) <- make.unique(colnames(transposto))

#Loop  para filtrar os individuos de um pop e depois calcular a frequencia e salvar em objeto com nome da população
for (i in lista_pop) {
  df <- transposto %>%
    select(IDPOP, starts_with("rs")) %>% #Seleciona as colunas
    filter(IDPOP == i)#Seleciona as linhas
  
  pop_genind <- df2genind(X = df, sep = ' ', ploidy = 2, ncode = 2)#converte o banco de dados que temos em genind 
  pop_genpop <- genind2genpop(pop_genind)#converte o genind em genpop 
  freq <- makefreq (pop_genpop, quiet = FALSE, missing = NA, truenames = TRUE)#calcula a frequencia da populaçao
  #Cria um objeto que armazena a frequencia e recebe o nome da pop
  nome_df <- paste("freq_", i, sep = "") 
  assign(nome_df, freq)
  
}

#criação de uma lista com todos as matrizes
objects <- list(freq_ACB, freq_LWK, freq_ESN, 
                freq_YRI, freq_MSL, freq_GWD, 
                freq_ASW, freq_CLM, freq_MXL, 
                freq_PUR, freq_PEL, freq_TSI, 
                freq_IBS, freq_GBR, freq_CEU, 
                freq_FIN, freq_PJL, freq_GIH, 
                freq_ITU, freq_STU, freq_BEB, 
                freq_CDX, freq_KHV, freq_CHS, 
                freq_CHB, freq_JPT)

#transformação de matriz em dataframe
freq_ACB <- as.data.frame(freq_ACB)
freq_ASW <- as.data.frame(freq_ASW)
freq_BEB <- as.data.frame(freq_BEB)
freq_CDX <- as.data.frame(freq_CDX)
freq_CEU <- as.data.frame(freq_CEU)
freq_CHB <- as.data.frame(freq_CHB)
freq_CHS <- as.data.frame(freq_CHS)
freq_CLM <- as.data.frame(freq_CLM)
freq_ESN <- as.data.frame(freq_ESN)
freq_FIN <- as.data.frame(freq_FIN)
freq_GBR <- as.data.frame(freq_GBR)
freq_GIH <- as.data.frame(freq_GIH)
freq_GWD <- as.data.frame(freq_GWD)
freq_IBS <- as.data.frame(freq_IBS)
freq_ITU <- as.data.frame(freq_ITU)
freq_JPT <- as.data.frame(freq_JPT)
freq_KHV <- as.data.frame(freq_KHV)
freq_LWK <- as.data.frame(freq_LWK)
freq_MSL <- as.data.frame(freq_MSL)
freq_MXL <- as.data.frame(freq_MXL)
freq_PEL <- as.data.frame(freq_PEL)
freq_PJL <- as.data.frame(freq_PJL)
freq_PUR <- as.data.frame(freq_PUR)
freq_STU <- as.data.frame(freq_STU)
freq_TSI <- as.data.frame(freq_TSI)
freq_YRI <- as.data.frame(freq_YRI)

#criação da lista de dataframes
lista_dataframes <- c('freq_LWK','freq_ESN', 'freq_YRI', 'freq_MSL','freq_GWD',
                      'freq_ACB', 'freq_ASW','freq_CLM', 'freq_MXL','freq_PUR', 'freq_PEL', 
                      'freq_TSI', 'freq_IBS', 'freq_GBR', 'freq_CEU', 'freq_FIN',
                      'freq_PJL', 'freq_GIH', 'freq_ITU', 'freq_STU', 'freq_BEB',
                      'freq_CDX', 'freq_KHV', 'freq_CHS', 'freq_CHB', 'freq_JPT')




}

#criação do dataframe final, que soma todos os dataframes
df_final <- data.frame(teste = character(length(lista_dataframes)), 
                       rs = numeric(length(lista_dataframes)))
#adicionando os nomes de cada população, 26 linhas
df_final$nome_df <- lista_dataframes

#criação de vetor com os nomes de todas as colunas de todos os dataframes
colunas_ACB <- colnames(freq_ACB)

colunas_ASW <- colnames(freq_ASW)

colunas_BEB <- colnames(freq_BEB)

colunas_CDX <- colnames(freq_CDX)

colunas_CEU <- colnames(freq_CEU)

colunas_CHB <- colnames(freq_CHB)

colunas_CHS <- colnames(freq_CHS)

colunas_CLM <- colnames(freq_CLM)

colunas_ESN <- colnames(freq_ESN)

colunas_FIN <- colnames(freq_FIN)

colunas_GBR <- colnames(freq_GBR)

colunas_GIH <- colnames(freq_GIH)

colunas_GWD <- colnames(freq_GWD)

colunas_IBS <- colnames(freq_IBS)

colunas_ITU <- colnames(freq_ITU)

colunas_JPT <- colnames(freq_JPT)

colunas_KHV <- colnames(freq_KHV)

colunas_LWK <- colnames(freq_LWK)

colunas_MSL <- colnames(freq_MSL)

colunas_MXL <- colnames(freq_MXL)

colunas_PEL <- colnames(freq_PEL)

colunas_PJL <- colnames(freq_PJL)

colunas_PUR <- colnames(freq_PUR)

colunas_STU <- colnames(freq_STU)

colunas_TSI <- colnames(freq_TSI)

colunas_YRI <- colnames(freq_YRI)

#remoção do primeiro elemento do vetor, que era um IDPOP de cada pop
colunas_ACB <- colunas_ACB[-1]
colunas_ASW <- colunas_ASW[-1]
colunas_BEB <- colunas_BEB[-1]
colunas_CDX <- colunas_CDX[-1]
colunas_CEU <- colunas_CEU[-1]
colunas_CHB <- colunas_CHB[-1]
colunas_CHS <- colunas_CHS[-1]
colunas_CLM <- colunas_CLM[-1]
colunas_ESN <- colunas_ESN[-1]
colunas_FIN <- colunas_FIN[-1]
colunas_GBR <- colunas_GBR[-1]
colunas_GIH <- colunas_GIH[-1]
colunas_GWD <- colunas_GWD[-1]
colunas_IBS <- colunas_IBS[-1]
colunas_ITU <- colunas_ITU[-1]
colunas_JPT <- colunas_JPT[-1]
colunas_KHV <- colunas_KHV[-1]
colunas_LWK <- colunas_LWK[-1]
colunas_MSL <- colunas_MSL[-1]
colunas_MXL <- colunas_MXL[-1]
colunas_PEL <- colunas_PEL[-1]
colunas_PJL <- colunas_PJL[-1]
colunas_PUR <- colunas_PUR[-1]
colunas_STU <- colunas_STU[-1]
colunas_TSI <- colunas_TSI[-1]
colunas_YRI <- colunas_YRI[-1]
#junção dos vetores de cada pop
ateste <- c(colunas_ACB, colunas_ASW, colunas_BEB, colunas_CDX, 
            colunas_CEU, colunas_CHB, colunas_CHS, colunas_CLM, 
            colunas_ESN, colunas_FIN, colunas_GBR, colunas_GIH, 
            colunas_GWD, colunas_IBS, colunas_ITU, colunas_JPT, 
            colunas_KHV, colunas_LWK, colunas_MSL, colunas_MXL, 
            colunas_PEL, colunas_PJL, colunas_PUR, colunas_STU, 
            colunas_TSI, colunas_YRI)

#eliminar elementos duplicados
avetor_sem_repeticao <- unique(ateste)

#resultado
print(avetor_sem_repeticao)

#criação do dataframe final, gerando um banco de dados novo no final
for (i in 1:length(lista_dataframes)) {
  for (col in geno_columns) {
    print(avetor_sem_repeticao)
    print(col)
    if (col %in% colnames(get(lista_dataframes[i]))) {  #If usado pra verificar se a dataframe possui aquela coluna.
      df_final[[col]][i] <- get(lista_dataframes[i])[[col]]#Coluna existe se atribui o valor dessa coluna no df_final
    } else { #Se não tiver a coluna atribui o valor de 0 
      df_final[[col]][i] <- 0
    }
  }
}
# Vetor com os sufixos para os novos nomes de variáveis
sufixos <- sapply(objects, function(x) gsub(".0$", "", x))

# Loop para processar cada arquivo
for (i in seq_along(objects)) {
  # Lê o arquivo
  varFreq <- read.csv(objects[i])
  
  # Remove as colunas que terminam com ".0"
  varFreq <- varFreq %>%
    select(-ends_with(".0"))
  
  # Cria o nome da nova variável
  nome_df <- paste("sempontozero_", sufixos[i], sep = "")
  
  # Atribui a variável ao ambiente global
  assign(nome_df, varFreq)
}

varFreqACBsoponto1 <- freq_ACB %>%
  select(-ends_with(".0"))

varFreqASWsoponto1 <- freq_ASW %>%
  select(-ends_with(".0"))

varFreqBEBsoponto1 <- freq_BEB %>%
  select(-ends_with(".0"))

varFreqCDXsoponto1 <- freq_CDX %>%
  select(-ends_with(".0"))

varFreqCEUsoponto1 <- freq_CEU %>%
  select(-ends_with(".0"))

varFreqCHBsoponto1 <- freq_CHB %>%
  select(-ends_with(".0"))

varFreqCHSsoponto1 <- freq_CHS %>%
  select(-ends_with(".0"))

varFreqCLMsoponto1 <- freq_CLM %>%
  select(-ends_with(".0"))

varFreqESNsoponto1 <- freq_ESN %>%
  select(-ends_with(".0"))

varFreqFINsoponto1 <- freq_FIN %>%
  select(-ends_with(".0"))

varFreqGBRsoponto1 <- freq_GBR %>%
  select(-ends_with(".0"))

varFreqGIHsoponto1 <- freq_GIH %>%
  select(-ends_with(".0"))

varFreqGWDsoponto1 <- freq_GWD %>%
  select(-ends_with(".0"))

varFreqIBSsoponto1 <- freq_IBS %>%
  select(-ends_with(".0"))

varFreqITUsoponto1 <- freq_ITU %>%
  select(-ends_with(".0"))

varFreqJPTsoponto1 <- freq_JPT %>%
  select(-ends_with(".0"))

varFreqKHVsoponto1 <- freq_KHV %>%
  select(-ends_with(".0"))

varFreqLWKsoponto1 <- freq_LWK %>%
  select(-ends_with(".0"))

varFreqMSLsoponto1 <- freq_MSL %>%
  select(-ends_with(".0"))

varFreqMXLsoponto1 <- freq_MXL %>%
  select(-ends_with(".0"))

varFreqPELsoponto1 <- freq_PEL %>%
  select(-ends_with(".0"))

varFreqPJLsoponto1 <- freq_PJL %>%
  select(-ends_with(".0"))

varFreqPURsoponto1 <- freq_PUR %>%
  select(-ends_with(".0"))

varFreqSTUsoponto1 <- freq_STU %>%
  select(-ends_with(".0"))

varFreqTSIsoponto1 <- freq_TSI %>%
  select(-ends_with(".0"))

varFreqYRIsoponto1 <- freq_YRI %>%
  select(-ends_with(".0"))

lista_dataframes_soalelomutante <- c('varFreqLWKsoponto1','varFreqESNsoponto1', 
                                     'varFreqYRIsoponto1','varFreqMSLsoponto1',
                                     'varFreqGWDsoponto1', 'varFreqACBsoponto1',
                                     'varFreqASWsoponto1','varFreqCLMsoponto1', 
                                     'varFreqMXLsoponto1', 'varFreqPURsoponto1',
                                     'varFreqPELsoponto1','varFreqTSIsoponto1', 
                                     'varFreqIBSsoponto1', 'varFreqGBRsoponto1',
                                     'varFreqCEUsoponto1','varFreqFINsoponto1',
                                     'varFreqPJLsoponto1', 'varFreqGIHsoponto1', 
                                     'varFreqITUsoponto1', 'varFreqSTUsoponto1',
                                     'varFreqBEBsoponto1','varFreqCDXsoponto1', 
                                     'varFreqKHVsoponto1', 'varFreqCHSsoponto1',
                                     'varFreqCHBsoponto1', 'varFreqJPTsoponto1')

#Cria um dataframe pra armazenar os resultados
df_final <- data.frame(teste = character(length(lista_dataframes_soalelomutante)), 
                       geno.01 = numeric(length(lista_dataframes_soalelomutante)))
#Preeche o o dataframe com os nomes das frequencia
df_final$nome_df <- lista_dataframes_soalelomutante

colunas_ACB <- colnames(varFreqACBsoponto1)

colunas_ASW <- colnames(varFreqASWsoponto1)

colunas_BEB <- colnames(varFreqBEBsoponto1)

colunas_CDX <- colnames(varFreqCDXsoponto1)

colunas_CEU <- colnames(varFreqCEUsoponto1)

colunas_CHB <- colnames(varFreqCHBsoponto1)

colunas_CHS <- colnames(varFreqCHSsoponto1)

colunas_CLM <- colnames(varFreqCLMsoponto1)

colunas_ESN <- colnames(varFreqESNsoponto1)

colunas_FIN <- colnames(varFreqFINsoponto1)

colunas_GBR <- colnames(varFreqGBRsoponto1)

colunas_GIH <- colnames(varFreqGIHsoponto1)

colunas_GWD <- colnames(varFreqGWDsoponto1)

colunas_IBS <- colnames(varFreqIBSsoponto1)

colunas_ITU <- colnames(varFreqITUsoponto1)

colunas_JPT <- colnames(varFreqJPTsoponto1)

colunas_KHV <- colnames(varFreqKHVsoponto1)

colunas_LWK <- colnames(varFreqLWKsoponto1)

colunas_MSL <- colnames(varFreqMSLsoponto1)

colunas_MXL <- colnames(varFreqMXLsoponto1)

colunas_PEL <- colnames(varFreqPELsoponto1)

colunas_PJL <- colnames(varFreqPJLsoponto1)

colunas_PUR <- colnames(varFreqPURsoponto1)

colunas_STU <- colnames(varFreqSTUsoponto1)

colunas_TSI <- colnames(varFreqTSIsoponto1)

colunas_YRI <- colnames(varFreqYRIsoponto1)

#junção dos vetores de cada pop
juncaotodossnps.1 <- c(colunas_ACB, colunas_ASW, colunas_BEB, colunas_CDX, 
            colunas_CEU, colunas_CHB, colunas_CHS, colunas_CLM, 
            colunas_ESN, colunas_FIN, colunas_GBR, colunas_GIH, 
            colunas_GWD, colunas_IBS, colunas_ITU, colunas_JPT, 
            colunas_KHV, colunas_LWK, colunas_MSL, colunas_MXL, 
            colunas_PEL, colunas_PJL, colunas_PUR, colunas_STU, 
            colunas_TSI, colunas_YRI)

testandoestecaralho <- as.data.frame(juncaotodossnps.1) 
#eliminar elementos duplicados
juncao_sem_repeticao <- unique(testandoestecaralho)


# Inicializa o df_final com todas as colunas necessárias
df_final3 <- data.frame()

# Define as colunas desejadas no df_final
df_final3 <- data.frame(juncao_sem_repeticao)

# Renomeia as colunas do df_final
colnames(df_final3) <- juncao_sem_repeticao

juncao_sem_repeticao_teste <- unlist(juncao_sem_repeticao)

# Inicializa todas as colunas do df_final com 0
df_final3[] <- 0

# Loop para percorrer cada dataframe
for (i in 1:length(lista_dataframes_soalelomutante)) {
  print(i)
  for (col in juncao_sem_repeticao_teste) {
    print(col)
    if (col %in% colnames(get(lista_dataframes_soalelomutante[i]))) {  #If usado pra verificar se a dataframe possui aquela coluna.
      df_final[[col]][i] <- get(lista_dataframes_soalelomutante[i])[[col]]#Coluna existe se atribui o valor dessa coluna no df_final
    } else { #Se não tiver a coluna atribui o valor de 0 
      df_final[[col]][i] <- 0
    }
  }
}


write.table(df_final, file = "FREQUENCIASALELICASMUTANTESTODASPOPS.txt", sep = ";", col.names = TRUE, quote = FALSE)
#Copiar o df_final para uma planilha do excel. 
library(clipr)
write_clip(df_final)

#pra filtrar somente acima de 1% ou igual
freqalelicamutantetodaspop001 <- df_final[, apply(df_final, 2, function(col) any(col >= 0.01))]
write.table(freqalelicamutantetodaspop001, file = "freqalelicamutantetodaspop001.txt", sep = " ", col.names = TRUE, quote = FALSE)