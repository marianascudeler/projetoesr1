freq <- read.table(choose.files())
library(readxl)
library(dplyr)
k3 <- read_xlsx('ancestralidade_k8 (1).xlsx', 
                sheet = 'ID_POP')
vetor_pop <- k3$cod
vetor_pop <- unique(vetor_pop)
vetor_pop <- vetor_pop[-11]


k8 <- read_xlsx('ancestralidade_k8 (1).xlsx', 
                sheet = 'ANCESTRALIDADE_K8')
tabela_resultados <- data.frame()

for (i in vetor_pop) {
  a <- k8 %>%
    select(POP,EUR,AFR,NAT,EAS,EUR2,AFR2,EAS2,SAS) %>%
    filter(POP == i)
  
  a$EUR <- as.numeric(a$EUR)
  EUR <- mean(a$EUR)
  
  a$AFR <- as.numeric(a$AFR)
  AFR <- mean(a$AFR)
  
  a$NAT <- as.numeric(a$NAT)
  NAT <- mean(a$NAT)
  
  a$EAS <- as.numeric(a$EAS)
  EAS <- mean(a$EAS)
  
  a$EUR2 <- as.numeric(a$EUR2)
  EUR2 <- mean(a$EUR2)
  
  a$AFR2 <- as.numeric(a$AFR2)
  AFR2 <- mean(a$AFR2)
  
  a$EAS2 <- as.numeric(a$EAS2)
  EAS2 <- mean(a$EAS2)
  
  a$SAS <- as.numeric(a$SAS)
  SAS <- mean(a$SAS)
  
  linha_resultado <- data.frame(POP = i, EUR = EUR, AFR = AFR, NAT = NAT,
                                EAS = EAS, EUR2 = EUR2, AFR2 = AFR2, EAS2 = EAS2,
                                SAS = SAS, stringsAsFactors = FALSE)
  tabela_resultados <- rbind(tabela_resultados, linha_resultado)
}


df <- data.frame(POP = character(),
                 EUR = numeric(),AFR = numeric(),NAT = numeric(),EAS = numeric(),EUR2 = numeric(),AFR2 = numeric(),EAS2 = numeric(),SAS = numeric(), stringsAsFactors = FALSE)


# Remove a coluna "POP" antes de arredondar os valores
tabela_sem_pop <- select(tabela_resultados, -POP)

# Função para arredondar os valores para 6 casas decimais
arredondar_decimal <- function(valor) {
  round(valor, digits = 6)
}

# Aplica a função em todas as colunas numéricas
tabela_decimal <- tabela_sem_pop %>%
  mutate(across(everything(), arredondar_decimal))

# Adiciona novamente a coluna "POP" à tabela decimal
tabela_decimal <- cbind(tabela_resultados$POP, tabela_decimal)

media_k8 <- tabela_decimal
# Resultado
print(tabela_decimal)
