## Analise de microdados do Enem 2016
## Autor: Davidson Santana
## Criado em: 18/02/2021 Jaboatao dos Guararapes - PE - Brasil 

#getwd()

library(tidyverse)

# Carregar o data set itens_prova_2016.csv no objeto itens
itens_16 <- read.csv("2016/DADOS/itens_prova_2016.csv", sep = ";")


# Selecionar apenas a prova de matematica de cor azul, que foi
# aplicada aos candidatos que nao requerem atendimento especial
# selecionar todas as colunas, exceto SG_AREA, TX_COR, CO_PROVA, 
# TP_LINGUA e IN_ITEM_ADAPTADO. Guardar essa selecao no objeto mt_16

mt_16 <- itens_16 %>% 
  filter(IN_ITEM_ADAPTADO == 0 & 
           TX_COR == "AZUL" & 
           SG_AREA == "MT" & 
           CO_PROVA == 303) %>% 
  select(-c(SG_AREA, TX_COR, CO_PROVA, TP_LINGUA, IN_ITEM_ADAPTADO))

# Transformar as observações das colunas TX_GABARITO e 
# CO_HABILIDADE no tipo factor.
mt_16$TX_GABARITO <- as.factor(mt_16$TX_GABARITO)
mt_16$CO_HABILIDADE <- as.factor(mt_16$CO_HABILIDADE)


# Sera que ha uma distribuicao igualitaria dos gabaritos das questoes ?
# Criar um grafico de barras para responder este questionamento
# a partir da prova azul

png("enem_2016_alternativas_por_itens.png", width = 1000, height = 600)
ggplot(mt_16, aes(x = TX_GABARITO, fill = TX_GABARITO)) +
  geom_bar() +
  scale_y_continuous(breaks = seq(0, 11, 1)) +
  labs(
    x = "Gabarito",
    y = "Num. de itens",
    title = "Enem 2016",
    subtitle = "Comparativo da quantidade de alternativas por itens",
    caption = "Fonte: Microdados Enem 2016 | Visualizacao: Davidson Santana (@davidson_vila)",
    tag = "Matemática"
  ) +
  scale_fill_discrete(name = "Alternativas")
dev.off()

# Agrupar as questoes da prova por habilidades, contar o número de itens
# por habilidades, ordenar as linhas na ordem descendente e por em 
# uma tibble

habil_16 <- mt_16 %>% 
  group_by(CO_HABILIDADE) %>% 
  summarize(QUANTIDADE = n()) %>%
  arrange(desc(QUANTIDADE)) %>% 
  as_tibble()

habil_16

# Construir um grafico de barras para relacionar o numero de itens por 
# habilidades.

png("enem_2016_itens_por_habilidades.png", width = 1000, height = 600)
ggplot(habil_16, aes(x = CO_HABILIDADE, y = QUANTIDADE, fill = QUANTIDADE)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0,3,1)) +
  labs(
    x = "Habilidades",
    y = "Num. de itens",
    title = "Enem 2016",
    subtitle = "Comparativo da quantidade de itens por habilidades",
    caption = "Fonte: Microdados Enem 2016 | Visualizacao: Davidson Santana (@davidson_vila)",
    tag = "Matemática"
  ) +
  scale_fill_continuous(name = "Num. de itens")
dev.off()





