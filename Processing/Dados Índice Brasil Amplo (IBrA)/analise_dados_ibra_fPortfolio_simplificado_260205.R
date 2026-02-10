
library(tidyverse)
library(tidyr)
library(slider) # Para operações do tipo "sliding window"
library(comprehenr) # Para "list comprehension" à la Python
library(ggplot2)
library(corrplot)
library(fPortfolio)
library(timeSeries) # stats::filter sobrescreve dplyr::filter, CUIDADO!!!
library(readxl)

###
#
INPUT_FILEPATH <- "C:/Users/nicho/Documents/MastersDegree/DataInputs/Dados Índice Brasil Amplo (IBrA)/Cotacoes18a25.csv"
COMPOSICAO_FILEPATH <- "C:/Users/nicho/Documents/MastersDegree/DataInputs/Dados Índice Brasil Amplo (IBrA)/CotaçõesIBRA.xlsx"
NUMERO_PONTOS_FRONTEIRA_EFICIENTE <- 50
DATA_INICIAL_REFERENCIA <- ymd("2018-02-01")
QTD_MESES_JANELA_MOVEL <- 24
DATA_FINAL_REFERENCIA <- DATA_INICIAL_REFERENCIA + months(QTD_MESES_JANELA_MOVEL) - 1

# Primeira otimização tem Data >= PRIMEIRO_MES_OTIMIZACAO
# Última otimização tem Data < ULTIMO_MES_OTIMIZACAO
PRIMEIRO_MES_OTIMIZACAO <- ym("2020-01") 
QTD_MESES_OTIMIZACAO <- 60
ULTIMO_MES_OTIMIZACAO <- PRIMEIRO_MES_OTIMIZACAO + months(QTD_MESES_OTIMIZACAO) - 1
#
###



# Lendo dados
ibra <- read_delim(INPUT_FILEPATH,
                   delim=";")

# Preprocessamento: 
## Arrumando variável de data (lubridate::dmy = "day, month, year")
## Criando índice diário de CDI (requer "des-anualização")
ibra2 <- ibra %>%
  mutate(Data = Data %>% str_replace_all("/", "-") %>% dmy()) %>% 
  arrange(Data) %>% 
  mutate(indice_CDI = cumprod((1 + `CDI 252 dias`/100)^(1/252)) / 
           first((1 + `CDI 252 dias`/100)^(1/252)),  # `first` garante que primeiro índice será 1
         mes = month(Data),
         ano = year(Data)) %>% 
  relocate(mes, .after = Data) %>%
  relocate(ano, .after = mes)

composicao_ibra <- read_xlsx(COMPOSICAO_FILEPATH,
                             sheet="Composição IBRA")

# Informações de mercado
colunas_mercado <- c("indice_CDI",
                     "IBOV",
                     "IBRA")
mercado <- ibra2 %>% 
  select(c(Data,
           mes,
           ano,
           all_of(colunas_mercado))
         )

# Tickets de ações
todas_acoes <- dplyr::setdiff(colnames(ibra2), 
                              c(colunas_mercado, "Data", "mes", "ano", "CDI 252 dias")
                              )


# Retornos diários
## Retornos diários: R_t = P_t/P_{t-1} - 1
## Log-retornos diários: r_t = log(P_t/P_{t-1}) = log(1 + R_t)
retornos_diarios <- ibra2 %>% 
  arrange(Data) %>% 
  select(c(Data,
           mes,
           ano,
           all_of(todas_acoes)
           )
         ) %>% 
  mutate(across(-c(Data, mes, ano), 
                ~ .x/dplyr::lag(.x) - 1) 
  ) %>% 
  slice(2:n())

log_retornos_diarios <- ibra2 %>% 
  arrange(Data) %>% 
  select(c(Data,
           mes,
           ano,
           all_of(todas_acoes)
           )
         ) %>% 
  mutate(across(-c(Data, mes, ano), 
                ~ log(.x/dplyr::lag(.x))
                )
         ) %>%
  slice(2:n())

mercado_retornos_diarios <- mercado %>% 
  arrange(Data) %>% 
  mutate(across(all_of(colunas_mercado),
                ~ .x/dplyr::lag(.x) - 1)
         ) %>% 
  slice(2:n())

mercado_log_retornos_diarios <- mercado %>%
  arrange(Data) %>%
  mutate(across(all_of(colunas_mercado),
                ~ log(.x/dplyr::lag(.x)))
         ) %>%
  slice(2:n())
## Obs: retorno_CDI não tem primeiro valor como NA porque ele próprio deriva de um retorno pré-calculado (`CDI 252 dias`)


# Compra-venda de ações
retornos_compra_venda <- ibra2 %>%
  select(Data,
         ano,
         mes,
         all_of(todas_acoes)
         ) %>% 
  summarise(across(-Data,
                   ~ log(last(.x, order_by=Data)/first(.x, order_by=Data))
  )
  ) %>% 
  ungroup



retornos_compra_venda <- ibra2 %>% 
  arrange(Data) %>%
  mutate(grupo = 1 + (row_number() - 1) %/% 21) %>%
  group_by(grupo) %>%
  summarise(
    Data_ini = first(Data),
    Data_fim = last(Data),
    across(
      all_of(todas_acoes),
      ~ last(.x) / first(.x) - 1),
    .groups = "drop"
  )

log_retornos_compra_venda <- ibra2 %>% 
  arrange(Data) %>%
  mutate(grupo = 1 + (row_number() - 1) %/% 21) %>%
  group_by(grupo) %>%
  summarise(
    Data_ini = first(Data),
    Data_fim = last(Data),
    across(
      all_of(todas_acoes),
      ~ log(last(.x)/first(.x))),
    .groups = "drop"
  )


  

