library(ggrepel)
library(ggplot2)
library(readxl)
library(tidyverse)

ances <- read_xlsx(file.choose(), sheet = "MEDIA_POP") #media das ancestralidade

freq_esr1 <- read.delim2(file.choose(), sep = " ", header = TRUE) #frequencia dos rs

#Adicionando uma nova coluna com os continentes
ances["CONT"]<- c('Africanos', 'Africanos', 'Africanos', 'Africanos', 'Africanos', 
                    'Miscigenados', 'Miscigenados', 'Miscigenados', 'Miscigenados', 'Miscigenados', 'Miscigenados', 
                    'Europeus', 'Europeus', 'Europeus', 'Europeus', 'Europeus', 
                    'Sul-asiático', 'Sul-asiático', 'Sul-asiático', 'Sul-asiático', 'Sul-asiático', 
                    'Leste-asiático', 'Leste-asiático', 'Leste-asiático', 'Leste-asiático', 'Leste-asiático')

#adicionanto uma nova colunas com as codigo das populações
freq_esr1["POP"] <- c('LWK', 'ESN', 'YRI', 'MSL', 'GWD', 
         'ACB', 'ASW', 'CLM', 'MXL', 'PUR', 'PEL',
         'TSI', 'IBS', 'GBR', 'CEU', 'FIN',
         'PJL', 'GIH', 'ITU', 'STU', 'BEB',
         'CDX', 'KHV', 'CHS', 'CHB', 'JPT')

#Juntando os dois dataframe em um só usando a coluna POP 
freq_esr1  <- freq_esr1 %>%
  left_join(ances, by = "POP")

#Para que todos label apareça mesmo se tiver overlap
options(ggrepel.max.overlaps = Inf)

#Graficos para cada rs 

rs191796612.C <- ggplot(freq_esr1, aes(as.numeric(EASW), as.numeric(rs191796612.1), #dados que serão utilizados para fazer o grafico
                    label = POP, color = CONT)) +  #label separa pelas populações e color colori pelos continentes 
  geom_point(shape = 19, size = 9) + #adiciona os pontos, shape é forma dos pontos e size o tamanho 
  geom_label_repel(box.padding = 0.5, size = 8,  show.legend = FALSE ) + #adicona a caixa do label. size o tamanho fonte na box e show.legend tira essa legenda  
  labs(y = "Frequência rs191796612.C", #modificar legendo do eixo y
       x = "Ancestralidade Leste Asiática - Oeste", #modificar legendo do eixo x
       color = "Regiões", #vai modificar legenda da legenda
       shape = 19,
       title = "Ancestralidade Leste Asiática - Oeste x Frequência do rs191796612.C") +
  theme_classic() +
  theme(axis.title.y = element_text(size = 15, #para fazer mudanças nas legendas e eixo y
                                    margin = margin(r = 14)),#funçao que explica como os elementos que nao sao os dados sao representados
        axis.title.x = element_text(size = 15, 
                                    margin = margin(t = 15)),#para fazer mudanças nas legendas e eixo y
        legend.title = element_text(face = "bold", size = 14), #colaca o legenda em negrito e muda o tamanho da fonte
        title = element_text(size = 14, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14)) +
  scale_color_manual(values = paleta_cores_cont,
                     breaks = vetor_cont)

####################################



rs138788168.T <- ggplot(freq_esr1, aes(as.numeric(EASE), as.numeric(rs138788168.1), 
                                       label = POP, color = CONT)) +  
  geom_point(shape = 19, size = 9) +  
  geom_label_repel(box.padding = 0.5, size = 8,  show.legend = FALSE ) + 
  labs(y = "Frequência rs138788168.T", 
       x = "Ancestralidade Leste Asiática - Leste",
       color = "Regiões", 
       shape = 19,
       title = "Ancestralidade Leste Asiática - Leste x Frequência do rs138788168.T") +
  theme_classic() +
  theme(axis.title.y = element_text(size = 15, 
                                    margin = margin(r = 14)),
        axis.title.x = element_text(size = 15, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 14), 
        title = element_text(size = 14, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14)) +
  scale_color_manual(values = paleta_cores_cont,
                     breaks = vetor_cont)

################################################################################



rs114659844.A <- ggplot(freq_esr1, aes(as.numeric(EAFR), as.numeric(rs114659844.1), 
                                       label = POP, color = CONT)) +  
  geom_point(shape = 19, size = 9) +  
  geom_label_repel(box.padding = 0.5, size = 8,  show.legend = FALSE ) + 
  labs(y = "Frequência rs114659844.A", 
       x = "Ancestralidade Leste Africana",
       color = "Regiões", 
       shape = 19,
       title = "Ancestralidade Leste Africana x Frequência do rs114659844.A") +
  theme_classic() +
  theme(axis.title.y = element_text(size = 15, 
                                    margin = margin(r = 14)),
        axis.title.x = element_text(size = 15, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 14), 
        title = element_text(size = 14, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14)) +
  scale_color_manual(values = paleta_cores_cont,
                     breaks = vetor_cont)


###################################################################################

rs77346468.A <- ggplot(freq_esr1, aes(as.numeric(WAFR), as.numeric(rs77346468.1), 
                                       label = POP, color = CONT)) +  
  geom_point(shape = 19, size = 9) +  
  geom_label_repel(box.padding = 0.5, size = 8,  show.legend = FALSE ) + 
  labs(y = "Frequência rs77346468.A", 
       x = "Ancestralidade Oeste Africana",
       color = "Regiões", 
       shape = 19,
       title = "Ancestralidade Oeste Africana x Frequência do rs77346468.A") +
  theme_classic() +
  theme(axis.title.y = element_text(size = 15, 
                                    margin = margin(r = 14)),
        axis.title.x = element_text(size = 15, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 14), 
        title = element_text(size = 14, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14)) +
  scale_color_manual(values = paleta_cores_cont,
                     breaks = vetor_cont)

###################################################################################

rs28457010.T <- ggplot(freq_esr1, aes(as.numeric(NAT), as.numeric(rs28457010.1), 
                                      label = POP, color = CONT)) +  
  geom_point(shape = 19, size = 9) +  
  geom_label_repel(box.padding = 0.5, size = 8,  show.legend = FALSE ) + 
  labs(y = "Frequência rs28457010.T", 
       x = "Ancestralidade Nativo Americana",
       color = "Regiões", 
       shape = 19,
       title = "Ancestralidade Nativo America x Frequência do rs28457010.T") +
  theme_classic() +
  theme(axis.title.y = element_text(size = 15, 
                                    margin = margin(r = 14)),
        axis.title.x = element_text(size = 15, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 14), 
        title = element_text(size = 14, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14)) +
  scale_color_manual(values = paleta_cores_cont,
                     breaks = vetor_cont)

###################################################################################

rs572442048.T <- ggplot(freq_esr1, aes(as.numeric(SAS), as.numeric(rs572442048.1), 
                                      label = POP, color = CONT)) +  
  geom_point(shape = 19, size = 9) +  
  geom_label_repel(box.padding = 0.5, size = 8,  show.legend = FALSE ) + 
  labs(y = "Frequência rs572442048.T", 
       x = "Ancestralidade Sul Asiática",
       color = "Regiões", 
       shape = 19,
       title = "Ancestralidade Sul Asiática x Frequência do rs572442048.T") +
  theme_classic() +
  theme(axis.title.y = element_text(size = 15, 
                                    margin = margin(r = 14)),
        axis.title.x = element_text(size = 15, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 14), 
        title = element_text(size = 14, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14)) +
  scale_color_manual(values = paleta_cores_cont,
                     breaks = vetor_cont)



###################################################################################


rs192973296.A <- ggplot(freq_esr1, aes(as.numeric(NEUR), as.numeric(rs192973296.1), 
                                       label = POP, color = CONT)) +  
  geom_point(shape = 19, size = 9) +  
  geom_label_repel(box.padding = 0.5, size = 8,  show.legend = FALSE ) + 
  labs(y = "Frequência rs192973296.A", 
       x = "Ancestralidade Norte Europeu",
       color = "Regiões", 
       shape = 19,
       title = "Ancestralidade Norte Europeu x Frequência do rs192973296.A") +
  theme_classic() +
  theme(axis.title.y = element_text(size = 15, 
                                    margin = margin(r = 14)),
        axis.title.x = element_text(size = 15, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 14), 
        title = element_text(size = 14, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14)) +
  scale_color_manual(values = paleta_cores_cont,
                     breaks = vetor_cont)


###################################################################################


rs144594234.A <- ggplot(freq_esr1, aes(as.numeric(SEUR), as.numeric(rs144594234.1), 
                                       label = POP, color = CONT)) +  
  geom_point(shape = 19, size = 9) +  
  geom_label_repel(box.padding = 0.5, size = 8,  show.legend = FALSE ) + 
  labs(y = "Frequência rs144594234.A", 
       x = "Ancestralidade Sul Europeu",
       color = "Regiões", 
       shape = 19,
       title = "Ancestralidade Sul Europeu  x Frequência do rs144594234.A") +
  theme_classic() +
  theme(axis.title.y = element_text(size = 15, 
                                    margin = margin(r = 14)),
        axis.title.x = element_text(size = 15, 
                                    margin = margin(t = 15)),
        legend.title = element_text(face = "bold", size = 14), 
        title = element_text(size = 14, 
                             face = "bold",
                             margin = margin(b = 15)),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14)) +
  scale_color_manual(values = paleta_cores_cont,
                     breaks = vetor_cont)
