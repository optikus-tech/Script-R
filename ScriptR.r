
library(ggplot2)
library(dplyr)
library(tidyr)

names(Bruna) <- names(Lucas) <- names(Giulia)

Lucas$integrante <- "Lucas"
Bruna$integrante <- "Bruna"
Giulia$integrante <- "Giulia"

# junta tudo
todos <- rbind(Lucas, Bruna,Giulia)

#  quantidade de linhas e tipos das colunas
nrow(todos)
str(todos)

#media de CPU, RAM e discode caada um
medias <- aggregate(
  cbind(cpu_uso_porcentagem,ram_uso_porcentagem,disco_uso_porcentagem) ~ integrante,
 data = todos,
  FUN = mean,
  na.rm = TRUE
)
# consumo geral medio
medias$consumo_geral <- rowMeans(
  medias[, c("cpu_uso_porcentagem","ram_uso_porcentagem", "disco_uso_porcentagem")])
medias[order(medias$consumo_geral, decreasing = TRUE), ]

# maior e menor consumo geral
medias$integrante[which.max(medias$consumo_geral)]
medias$integrante[which.min(medias$consumo_geral)]


# uso da CPU
tapply(todos$cpu_uso_porcentagem,todos$integrante, summary)
#uso da RAM
tapply(todos$ram_uso_porcentagem,todos$integrante, summary)
# uso do disco
tapply(todos$disco_uso_porcentagem,todos$integrante, summary)
table(todos$cpu_status)
table(todos$ram_status)
table(todos$disco_status)
table(todos$integrante, todos$cpu_status)
table(todos$integrante, todos$ram_status)
table(todos$integrante, todos$disco_status)

#% CPU
prop.table(table(todos$cpu_status)) * 100

#%RAM
prop.table(table(todos$ram_status)) * 100

# % disco
prop.table(table(todos$disco_status)) * 100

#% por integrante
prop.table(table(todos$integrante, todos$cpu_status), 1) * 100
prop.table(table(todos$integrante, todos$ram_status), 1) * 100
prop.table(table(todos$integrante, todos$disco_status), 1) * 100

# Padroniza os nomes dos status
alertas <- data.frame(integrante = todos$integrante,cpu = tolower(trimws(todos$cpu_status)),ram = tolower(trimws(todos$ram_status)),disco = tolower(trimws(todos$disco_status)))

alertas$total_alertas <-
  (alertas$cpu %in% c("alerta", "crítico")) +(alertas$ram %in% c("alerta", "crítico")) +(alertas$disco %in% c("alerta", "crítico"))

aggregate(total_alertas ~ integrante,
          data = alertas,
          FUN = sum)


sum(alertas$cpu %in% c("alerta", "crítico"))
sum(alertas$ram %in% c("alerta", "crítico"))
sum(alertas$disco %in% c("alerta", "crítico"))

sum(alertas$total_alertas)


# grafico de dispersão ( sem relação)
plot(todos$ram_uso_porcentagem,
     todos$cpu_uso_porcentagem,
     main = "Uso de RAM x CPU",
     xlab = "Uso RAM (%)",
     ylab = "Uso CPU (%)",
     pch = 19,
     col = "blue")

# histogramas
# CPU
hist(todos$cpu_uso_porcentagem,
     main = "Distribuicao do uso de CPU",
     xlab = "Uso de CPU (%)",
     ylab = "Quantidade de leituras",
     col = "steelblue",
     breaks = 20)

# RAM
hist(todos$ram_uso_porcentagem,
     main = "Distribuicao do uso de RAM",
     xlab = "Uso de RAM (%)",
     ylab = "Quantidade de leituras",
     col = "darkblue",
     breaks = 20)

#disco
hist(todos$disco_uso_porcentagem,
     main = "Distribuicao do uso de disco",
     xlab = "Uso de disco (%)",
     ylab = "Quantidade de leituras",
     col = "skyblue",
     breaks = 20)


# grafico 1  para a dashboard
ggplot(todos, aes(x = cpu_uso_porcentagem, fill = integrante)) +
  geom_histogram(color = "black", bins = 30, alpha = 0.8) +
  facet_grid(integrante ~ .) +
  theme_minimal() +
  labs(title = "Frequencia de Uso de CPU por Servidor",
       subtitle = "Histogramas separados para comparacao",
       x = "Uso de CPU (%)",
       y = "Quantidade de Coletas") +
  theme(legend.position = "none",
        strip.text = element_text(size = 12, face = "bold"))

ggplot(todos, aes(x = ram_uso_porcentagem,
                  fill = integrante,
                  color = integrante)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  labs(title = "Distribuicao de Uso de Memoria",
       subtitle = "Curvas de densidade por servidor",
       x = "Uso de Memoria (%)",
       y = "Densidade",
       fill = "Servidor",
       color = "Servidor") +
  theme(legend.position = "bottom")


# definição de metricas e alertas

limites <- data.frame(
  metrica = c("CPU", "RAM", "Disco"),
  alerta = c(70, 80, 80),
  critico = c(90, 90, 90))


#data e hora
todos$data_coleta <- as.POSIXct(
  todos$data_coleta,
  format = "%Y-%m-%d %H:%M:%S")
#preparacao para graficos com metricas
dados_longos <- todos %>%
  pivot_longer(
    cols = c( cpu_uso_porcentagem,ram_uso_porcentagem,disco_uso_porcentagem),
    names_to = "metrica",
    values_to = "uso" ) %>%
  mutate( metrica = recode( metrica,cpu_uso_porcentagem = "CPU",ram_uso_porcentagem = "RAM", disco_uso_porcentagem = "Disco"
    )
  )
# grafico 2 consumo 

ggplot(
  dados_longos,
  aes(x = data_coleta,
      y = uso,
    color = integrante,
    group = integrante)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_hline(
  data = limites,
    aes(yintercept = alerta),
    inherit.aes = FALSE,
    color = "orange",
    linetype = "dashed"
  ) +
  geom_hline(
    data = limites,
    aes(yintercept = critico),
    inherit.aes = FALSE,
    color = "red",
    linetype = "dashed"
  ) +
  facet_wrap(~metrica, ncol = 1) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100)
  ) +
  theme_minimal() +
  labs(
    title = "monitoramenoto continuo ",
    subtitle = "Laranja: alerta | Vermelho: critico",
    x = "Data e horario da coleta",
    y = "Uso (%)",
    color = "Integrante"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"))

# grafico 3 de comparação entre o valor maximo e medio por componete e por csv
comparacao <- todos %>%
  group_by(integrante) %>%
  summarise(CPU_media = mean(cpu_uso_porcentagem , na.rm = TRUE),
  CPU_maximo = max (cpu_uso_porcentagem , na.rm = TRUE),
 RAM_media = mean (ram_uso_porcentagem, na.rm = TRUE),
RAM_maximo = max (ram_uso_porcentagem, na.rm = TRUE),
Disco_media = mean(disco_uso_porcentagem , na.rm = TRUE),
    Disco_maximo = max(disco_uso_porcentagem, na.rm = TRUE),
    .groups = "drop")

comparacao_long <- comparacao %>%
  pivot_longer(
    cols = -integrante,
    names_to = "indicador",
    values_to = "valor"
  ) %>%
  mutate(
    metrica = case_when(
      grepl("^CPU_", indicador) ~ "CPU",
      grepl("^RAM_", indicador) ~ "RAM",
      grepl("^Disco_", indicador) ~ "Disco"
    ),
    tipo = ifelse(
      grepl("media", indicador),
      "Media",
      "Maximo"))

ggplot(
  comparacao_long,
  aes(
    x = integrante,
    y = valor,
    fill = tipo
  )
) +
  geom_col(position = "dodge") +
  geom_hline(
    data = limites,
    aes(yintercept = alerta),
    inherit.aes = FALSE,
    color = "orange",
    linetype = "dashed"
  ) +
  geom_hline(
    data = limites,
    aes(yintercept = critico),
    inherit.aes = FALSE,
    color = "red",
    linetype = "dashed"
  ) +
  facet_wrap(~metrica, ncol = 1) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100)
  ) +
  theme_minimal() +
  labs(
    title = "Consumo Medio e Maximo por Sistema",
    x = "Integrante",
    y = "Uso (%)",
    fill = "Indicador"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"))
  



# componente critico ou normal 


faixas_consumo <- dados_longos %>%
  mutate(
    faixa = case_when(
      metrica == "CPU" & uso < 70 ~ "Normal",
      metrica == "CPU" & uso < 80 ~ "Alerta",
      metrica == "CPU" & uso >= 90 ~ "Critico",
      
      metrica == "RAM" & uso < 60 ~ "Normal",
      metrica == "RAM" & uso < 80 ~ "Alerta",
      metrica == "RAM" & uso >= 90 ~ "Critico",
      
      metrica == "Disco" & uso < 60 ~ "Normal",
      metrica == "Disco" & uso < 75 ~ "Alerta",
      metrica == "Disco" & uso >= 90 ~ "Critico"
    )
  ) %>%
  count(integrante, metrica, faixa) %>%
  group_by(integrante, metrica) %>%
  mutate(
    percentual = n / sum(n) * 100
  ) %>%
  ungroup()

print(faixas_consumo)

ggplot(
  faixas_consumo,
  aes(
    x = integrante,
    y = percentual,
    fill = faixa
  )
) +
  geom_col(position = "stack") +
  facet_wrap(~metrica, ncol = 1) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100)
  ) +
  
  geom_col(position = "stack") +
  facet_wrap(~metrica, ncol = 1) +
  scale_fill_manual(
    values = c(
      "Normal" = "steelblue",
      "Alerta" = "orange",
      "Critico" = "red"
    )
  ) +
  theme_minimal() +
  labs(
    title = "Capturas por Nivel de Consumo",
    subtitle = "Percentual de leituras normais, em alerta e criticas",
    x = "Integrante",
    y = "Capturas (%)",
    fill = "Faixa de consumo"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

########
#Giulia <- read.csv(
#file.choose(),
#header = TRUE,
#sep = ",",
#stringsAsFactors = FALSE
#)
