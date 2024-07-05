library(ggplot2)
library(ggimage)
library(readxl)

img_urls <- c("C:\\Users\\gppgx\\Downloads\\grossura80\\95.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\93.png", #CHOPCCAS
              "C:\\Users\\gppgx\\Downloads\\grossura80\\84.png", #CUSCO
              "C:\\Users\\gppgx\\Downloads\\grossura80\\105.png", #IQUITOS
              "C:\\Users\\gppgx\\Downloads\\grossura80\\92.png", #MATZES
              "C:\\Users\\gppgx\\Downloads\\grossura80\\91.png", #MOCHES
              "C:\\Users\\gppgx\\Downloads\\grossura80\\90.png", #TRUJILLO
              "C:\\Users\\gppgx\\Downloads\\grossura80\\107.png" ) #UROS

freq_ances <- read_excel(choose.files())

freq_reserva <- freq_ances

vetor_cont_NAT <- c('Africanos', 'Miscigenados', 'Europeus', 'Sul Asiático', 'Leste Asiático',
                    'CHOPCCAS', 'CUSCO', 'IQUITOS', 'MATZES', 'MOCHES', 'TRUJILLO', 'UROS')

freq_ances$rs9340799.1 <- as.numeric(freq_ances$rs9340799.1)
freq_ances$NAT <- as.numeric(freq_ances$NAT)

ggplot(freq_ances, aes(NAT, rs9340799.1)) +
  geom_image(aes(image = img_urls), size = 0.05) +
  labs(y = "Frequência Alelo *2", #modificar legendo do eixo y
       x = "Ancestralidade Nativo America", #modificar legendo do eixo x
       color = "População", #vai modificar legenda da legenda
       title = "Ancestralidade Nativo Americana x Frequência do Alelo rs9340799.1")+ #Coloca um titulo para o grafico
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
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +  # Ajustando os intervalos do eixo x
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.1)) +  # Ajustando os intervalos do eixo y
  theme(
    axis.text.x = element_text(size = 12),  # Ajustando o tamanho do texto do eixo x
    axis.text.y = element_text(size = 12)   # Ajustando o tamanho do texto do eixo y
  )

###############################################################

img_urls <- c("C:\\Users\\gppgx\\Downloads\\grossura80\\95.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\93.png", #CHOPCCAS
              "C:\\Users\\gppgx\\Downloads\\grossura80\\84.png", #CUSCO
              "C:\\Users\\gppgx\\Downloads\\grossura80\\105.png", #IQUITOS
              "C:\\Users\\gppgx\\Downloads\\grossura80\\92.png", #MATZES
              "C:\\Users\\gppgx\\Downloads\\grossura80\\91.png", #MOCHES
              "C:\\Users\\gppgx\\Downloads\\grossura80\\90.png", #TRUJILLO
              "C:\\Users\\gppgx\\Downloads\\grossura80\\107.png" ) #uros

freq_ances$rs2234693.1 <- as.numeric(freq_ances$rs2234693.1)
freq_ances$NAT <- as.numeric(freq_ances$NAT)

ggplot(freq_ances, aes(NAT, rs2234693.1)) +
  geom_image(aes(image = img_urls), size = 0.05) +
  labs(y = "Frequência rs2234693.1", #modificar legendo do eixo y
       x = "Ancestralidade Nativo America", #modificar legendo do eixo x
       color = "População", #vai modificar legenda da legenda
       title = "Ancestralidade Nativo Americana x Frequência do Alelo rs2234693.1")+ #Coloca um titulo para o grafico
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
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +  # Ajustando os intervalos do eixo x
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.1)) +  # Ajustando os intervalos do eixo y
  theme(
    axis.text.x = element_text(size = 12),  # Ajustando o tamanho do texto do eixo x
    axis.text.y = element_text(size = 12)   # Ajustando o tamanho do texto do eixo y
  )

##################################################################

img_urls <- c("C:\\Users\\gppgx\\Downloads\\grossura80\\86.png", #LWK
              "C:\\Users\\gppgx\\Downloads\\grossura80\\87.png", #ESN
              "C:\\Users\\gppgx\\Downloads\\grossura80\\88.png", #YRI
              "C:\\Users\\gppgx\\Downloads\\grossura80\\94.png", #MSL
              "C:\\Users\\gppgx\\Downloads\\grossura80\\96.png", #GWD
              "C:\\Users\\gppgx\\Downloads\\grossura80\\100.png", #ACB
              "C:\\Users\\gppgx\\Downloads\\grossura80\\103.png", #ASW
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #CHOPCCAS
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #CUSCO
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #IQUITOS
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #MATZES
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #MOCHES
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #TRUJILLO
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png" ) #UROS

freq_ances$rs2234693.1 <- as.numeric(freq_ances$rs2234693.1)
freq_ances$AFRL <- as.numeric(freq_ances$AFRL)

ggplot(freq_ances, aes(AFRL, rs2234693.1)) +
  geom_image(aes(image = img_urls), size = 0.05) +
  labs(y = "Frequência rs2234693.1", #modificar legendo do eixo y
       x = "Ancestralidade AFRL", #modificar legendo do eixo x
       color = "População", #vai modificar legenda da legenda
       title = "Ancestralidade AFRL x Frequência do Alelo rs2234693.1")+ #Coloca um titulo para o grafico
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
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +  # Ajustando os intervalos do eixo x
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.1)) +  # Ajustando os intervalos do eixo y
  theme(
    axis.text.x = element_text(size = 12),  # Ajustando o tamanho do texto do eixo x
    axis.text.y = element_text(size = 12)   # Ajustando o tamanho do texto do eixo y
  )

##############################################################
img_urls <- c("C:\\Users\\gppgx\\Downloads\\grossura80\\86.png", #LWK
              "C:\\Users\\gppgx\\Downloads\\grossura80\\87.png", #ESN
              "C:\\Users\\gppgx\\Downloads\\grossura80\\88.png", #YRI
              "C:\\Users\\gppgx\\Downloads\\grossura80\\94.png", #MSL
              "C:\\Users\\gppgx\\Downloads\\grossura80\\96.png", #GWD
              "C:\\Users\\gppgx\\Downloads\\grossura80\\100.png", #ACB
              "C:\\Users\\gppgx\\Downloads\\grossura80\\103.png", #ASW
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\85.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #CHOPCCAS
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #CUSCO
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #IQUITOS
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #MATZES
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #MOCHES
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #TRUJILLO
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png" ) #UROS

freq_ances$rs2234693.1 <- as.numeric(freq_ances$rs2234693.1)
freq_ances$AFRO <- as.numeric(freq_ances$AFRO)

ggplot(freq_ances, aes(AFRO, rs2234693.1)) +
  geom_image(aes(image = img_urls), size = 0.05) +
  labs(y = "Frequência rs2234693.1", #modificar legendo do eixo y
       x = "Ancestralidade AFRO", #modificar legendo do eixo x
       color = "População", #vai modificar legenda da legenda
       title = "Ancestralidade AFRO x Frequência do Alelo rs2234693.1")+ #Coloca um titulo para o grafico
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
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +  # Ajustando os intervalos do eixo x
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.1)) +  # Ajustando os intervalos do eixo y
  theme(
    axis.text.x = element_text(size = 12),  # Ajustando o tamanho do texto do eixo x
    axis.text.y = element_text(size = 12)   # Ajustando o tamanho do texto do eixo y
  )

################################################################
img_urls <- c("C:\\Users\\gppgx\\Downloads\\grossura80\\95.png", #LWK
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png", #ESN
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png", #YRI
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png", #MSL
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png", #GWD
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png", #ACB
              "C:\\Users\\gppgx\\Downloads\\grossura80\\95.png", #ASW
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\99.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\101.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\89.png", #PJL
              "C:\\Users\\gppgx\\Downloads\\grossura80\\97.png", #GIH
              "C:\\Users\\gppgx\\Downloads\\grossura80\\102.png", #ITU
              "C:\\Users\\gppgx\\Downloads\\grossura80\\106.png", #STU
              "C:\\Users\\gppgx\\Downloads\\grossura80\\108.png", #BEB
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\83.png",
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #CHOPCCAS
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #CUSCO
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #IQUITOS
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #MATZES
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #MOCHES
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png", #TRUJILLO
              "C:\\Users\\gppgx\\Downloads\\grossura80\\104.png" ) #UROS

freq_ances$rs9340799.1 <- as.numeric(freq_ances$rs9340799.1)
freq_ances$SAS <- as.numeric(freq_ances$SAS)

ggplot(freq_ances, aes(SAS, rs9340799.1)) +
  geom_image(aes(image = img_urls), size = 0.05) +
  labs(y = "Frequência rs9340799.1", #modificar legendo do eixo y
       x = "Ancestralidade SAS", #modificar legendo do eixo x
       color = "População", #vai modificar legenda da legenda
       title = "Ancestralidade SAS x Frequência do Alelo rs9340799.1")+ #Coloca um titulo para o grafico
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
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +  # Ajustando os intervalos do eixo x
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.1)) +  # Ajustando os intervalos do eixo y
  theme(
    axis.text.x = element_text(size = 12),  # Ajustando o tamanho do texto do eixo x
    axis.text.y = element_text(size = 12)   # Ajustando o tamanho do texto do eixo y
  )

