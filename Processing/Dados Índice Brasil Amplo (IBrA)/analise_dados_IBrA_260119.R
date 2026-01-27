library(tidyverse)

# Lendo dados
ibra <- read_delim("C:/Users/nicho/Documents/MastersDegree/DataInputs/Dados Índice Brasil Amplo (IBrA)/Cotacoes18a25.csv",
                 delim=";")

# Preprocessamento: Arrumando variável de data (lubridate::dmy = "day, month, year")
ibra2 <- ibra %>% 
  mutate(Data = Data %>% str_replace_all("/", "-") %>% dmy(),
         mes = month(Data),
         ano = year(Data))

colunas_mercado <- c("CDI 252 dias",
                     "IBOV",
                     "IBRA")

# Informações de mercado
mercado <- ibra2 %>% 
  select(c(Data,
           mes, 
           ano, 
           all_of(colunas_mercado))
  )


# Dataframe de retornos diários ($\frac{P_t}{P_{t-1}} - 1$)
retornos_diarios <- ibra2 %>% 
  arrange(Data) %>% 
  mutate(across(-Data, ~ .x/lag(.x) - 1),
         mes = month(Data),
         ano = year(Data)) %>% 
  relocate(mes, .after = Data) %>% 
  relocate(ano, .after = mes)
  

# Ações do portfolio
acoes_escolhidas <- c("ECOR3",
                      "ELET3",
                      "ELET6",
                      "EMBJ3",
                      "ENGI11",
                      "ENEV3",
                      "EGIE3",
                      "EQTL3",
                      "EVEN3",
                      "EZTC3",
                      "GFSA3",
                      "GGBR4",
                      "GOAU4",
                      "GRND3",
                      "GUAR3",
                      "CAML3",
                      "CMIG4",
                      "COGN3",
                      "CSMG3",
                      "CPLE3")

# portfolio <- retornos_diarios %>% 
#   arrange(Data) %>% 
#   select(Data, 
#          mes,
#          ano, 
#          all_of(acoes_escolhidas)
#          )


# Retornos
retornos_acoes <- retornos_diarios %>% 
  select(c(Data,
           mes, 
           ano,
           all_of(acoes_escolhidas))
         )

# Agregados mensais
## 1) Mercado
# mercado_mensal <- mercado %>%
#   group_by(ano, mes) %>%
#   summarise(across(all_of(colunas_mercado), ~prod(1 + .x) - 1)) %>%
#   ungroup()

## 2) Portfolio: R_{ano-mês} = prod(1 + R_dia) - 1 [sobre dias do mês de dado ano]
retornos_mensais <- retornos_acoes %>% 
  group_by(ano, mes) %>% 
  summarise(across(all_of(acoes_escolhidas), ~prod(1 + .x, na.rm=TRUE) - 1)) %>% 
  ungroup()


# Agregados anualizados
## 1) Mercado


## 2) Portfolio: R_{ano} = prod(1 + R_mes) - 1 [sobre meses de dado ano]
retornos_anualizados <- retornos_mensais %>% 
  group_by(ano) %>% 
  summarise(across(all_of(acoes_escolhidas), ~prod(1 + .x, na.rm=TRUE)**(1/12) - 1)) %>% 
  ungroup()


# desvios_mensais <- retornos_acoes %>% 
#   select(-Data) %>% 
#   # mutate(mes = as.character(mes),
#   # ano = as.character(ano)) %>% 
#   group_by(ano, mes) %>% 
#   summarise(across(everything(), ~sd(.x, na.rm = TRUE))) %>% 
#   ungroup()

matriz_covariancia_portfolio <- retornos_mensais %>% 
  select(-mes, -ano) %>% cov


# # Agregados anualizados
# retornos_anualizados <- retornos_mensais %>% 
#   # mutate(mes = as.character(mes),
#   # ano = as.character(ano)) %>% 
#   group_by(ano) %>% 
#   summarise(across(everything(), ~12*mean(.x, na.rm = TRUE))) %>% 
#   select(-mes) %>% 
#   ungroup()
# 
# desvios_anualizados <- desvios_mensais %>% 
#   # mutate(mes = as.character(mes),
#   # ano = as.character(ano)) %>% 
#   group_by(ano) %>% 
#   summarise(across(everything(), ~sqrt(12)*mean(.x, na.rm = TRUE))) %>% 
#   select(-mes) %>% 
#   ungroup()


