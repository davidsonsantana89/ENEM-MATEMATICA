## Analise de microdados do Enem 2018
## Autor: Davidson Santana
## Criado em: 17/02/2021 Jaboatao dos Guararapes - PE - Brasil 

library(tidyverse)

# Carregar o data set ITENS_PROVA_2018.csv no objeto itens_18
itens_18 <- read.csv("2018/ITENS_PROVA_2018.csv", sep = ";")

# Selecionar apenas a prova de matematica de cor azul, que foi
# aplicada aos candidatos que nao requerem atendimento especial
# selecionar todas as colunas, exceto SG_AREA, TX_COR, CO_PROVA, 
# TP_LINGUA e IN_ITEM_ADAPTADO. Guardar essa selecao no objeto mt_18

mt_18 <- itens_18 %>% 
  filter(SG_AREA == "MT" & IN_ITEM_ADAPTADO == 0 & CO_PROVA == 459) %>% 
  select(-c(SG_AREA, TX_COR, CO_PROVA, TP_LINGUA, IN_ITEM_ADAPTADO)) %>% 
  as_tibble()

# Transformar as observações das colunas TX_GABARITO e 
# CO_HABILIDADE no tipo factor.
mt_18$CO_HABILIDADE <- as.factor(mt_18$CO_HABILIDADE)
mt_18$TX_GABARITO <- as.factor(mt_18$TX_GABARITO)


# Sera que ha uma distribuicao igualitaria dos gabaritos das questoes ?
# Criar um grafico de barras para responder este questionamento
# a partir da prova azul
png("enem_2018_alternativas_por_itens.png", width = 1000, height = 600)
ggplot(mt_18, aes(x = TX_GABARITO, fill = TX_GABARITO)) +
  geom_bar(stat = "count", position = "dodge", size = 0.5) +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  labs(
    x = "Gabarito",
    y = "Num. de itens",
    title = "Enem 2018",
    subtitle = "Comparativo da quantidade de alternativas por itens",
    caption = "Fonte: Microdados Enem 2018 | Visualizacao: Davidson Santana (@davidson_vila)",
    tag = "Matemática"
  ) +
  scale_fill_discrete(name = "Alternativas")
dev.off()

# Agrupar as questoes da prova por habilidades, contar o número de itens
# por habilidades, ordenar as linhas na ordem descendente e por em 
# uma tibble
habil_18 <- mt_18 %>% 
  group_by(CO_HABILIDADE) %>% 
  summarize(QUANTIDADE = n()) %>% 
  arrange(desc(QUANTIDADE)) %>% 
  as_tibble()

# Quais as habilidades mais exploradas no Enem 2018 ?
# Criar um grafico de barras para responder este questionamento
# a partir da prova azul
png("enem_2018_itens_por_habilidades.png", width = 1000, height = 600)
ggplot(habil_18, aes(x = CO_HABILIDADE, y = QUANTIDADE, fill = QUANTIDADE)) +
  geom_col() +
  scale_fill_continuous(name = "Num. de itens") +
  scale_y_continuous(breaks = seq(0,3,1)) +
  labs(
    x = "Habilidades",
    y = "Num. de itens",
    title = "Enem 2018",
    subtitle = "Comparativo da quantidade de itens por habilidades",
    caption = "Fonte: Microdados Enem 2018 | Visualizacao: Davidson Santana (@davidson_vila)",
    tag = "Matemática"
  )
dev.off()

