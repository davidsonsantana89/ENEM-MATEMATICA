## Analise de microdados do Enem 2017
## Autor: Davidson Santana
## Criado em: 17/02/2021 Jaboatao dos Guararapes - PE - Brasil 

library(tidyverse)
library(dplyr)

getwd()

# Carregar o data set ITENS_PROVA_2017.csv no objeto itens
itens_17 <- read.csv("2017/MICRODADOS17/DADOS/ITENS_PROVA_2017.csv", sep = ";")

# Selecionar apenas a prova de matematica de cor azul, que foi
# aplicada aos candidatos que nao requerem atendimento especial
# selecionar todas as colunas, exceto SG_AREA, TX_COR, CO_PROVA, 
# TP_LINGUA e IN_ITEM_ADAPTADO. Guardar essa selecao no objeto mt_17

mt_17 <- itens_17 %>% 
  filter(SG_AREA == "MT" & IN_ITEM_ADAPTADO == 0 & CO_PROVA == 403) %>% 
  select(-c(SG_AREA, TX_COR, CO_PROVA, TP_LINGUA, IN_ITEM_ADAPTADO)) %>% 
  as_tibble()

# Transformar as observações das colunas TX_GABARITO e 
# CO_HABILIDADE no tipo factor.
mt_17$CO_HABILIDADE <- as.factor(mt_17$CO_HABILIDADE)
mt_17$TX_GABARITO <- as.factor(mt_17$TX_GABARITO)

# Sera que ha uma distribuicao igualitaria dos gabaritos das questoes ?
# Criar um grafico de barras para responder este questionamento
# a partir da prova azul

png("enem_2017_alternativas_por_itens.png", width = 1000, height = 600)
ggplot(mt_17, aes(x = TX_GABARITO, fill = TX_GABARITO)) +
  geom_bar(stat = "count", position = "dodge", size = 0.5) +
  scale_y_continuous(breaks = seq(0, 12, 1)) +
  labs(
    x = "Gabarito",
    y = "Num. de itens",
    title = "Enem 2017",
    subtitle = "Comparativo da quantidade de alternativas por itens",
    caption = "Fonte: Microdados Enem 2017 | Visualizacao: Davidson Santana (@davidson_vila)",
    tag = "Matematica"
  ) +
  scale_fill_discrete(name = "Alternativas")
dev.off()


# Agrupar as questoes da prova por habilidades, contar o número de itens
# por habilidades, ordenar as linhas na ordem descendente e por em 
# uma tibble
habil_17 <- mt_17 %>% 
  group_by(CO_HABILIDADE) %>% 
  summarize(QUANTIDADE = n()) %>% 
  arrange(desc(QUANTIDADE)) %>% 
  as_tibble()

# Quais as habilidades mais exploradas no Enem 2017 ?
# Criar um grafico de barras para responder este questionamento
# a partir da prova azul

png("enem_2017_itens_por_habilidades.png", width = 1000, height = 600)
ggplot(habil_17, aes(x = CO_HABILIDADE, y = QUANTIDADE, fill = QUANTIDADE)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0,3,1)) +
  labs(
    x = "Habilidades",
    y = "Num. de itens",
    title = "Enem 2017",
    subtitle = "Comparativo da quantidade de itens por habilidades",
    caption = "Fonte: Microdados Enem 2017 | Visualizacao: Davidson Santana (@davidson_vila)",
    tag = "Matemática"
  ) +
  scale_fill_continuous(name = "Num. de itens")
dev.off()

