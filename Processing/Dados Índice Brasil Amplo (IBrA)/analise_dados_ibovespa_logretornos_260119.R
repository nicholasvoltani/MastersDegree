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


# Dataframes de log-retornos: 1 + R_t = log(P_t/P_{t-1})
## Mantém somente a partir da segunda linha (primeira é NA devido ao cálculo com lag(.x))
log_retornos_diarios <- ibra2 %>% 
  arrange(Data) %>% 
  select(c(Data,
           all_of(acoes_escolhidas)
           )
         ) %>% 
  mutate(across(-Data, ~ log(.x/lag(.x))),
         mes = month(Data),
         ano = year(Data)) %>% 
  relocate(mes, .after = Data) %>% 
  relocate(ano, .after = mes) %>% 
  slice(2:n())

## Log-retornos_mensais = sum(Log-retornos_diários)  <=>  1 + Retornos_mensais = prod(1 + retornos_diários)
log_retornos_mensais <- log_retornos_diarios %>% 
  group_by(ano, mes) %>% 
  summarise(across(all_of(acoes_escolhidas), ~sum(.x, na.rm=TRUE))) %>% 
  ungroup()

## Log-retornos_anualizados = 12*log_retornos_diarios_médios <=> (média_geométrica_retornos_diários)**12
log_retornos_anualizados <- log_retornos_diarios %>% 
  summarise(across(all_of(acoes_escolhidas), ~(12/n())*sum(.x, na.rm=TRUE)))

## Desvio padrão anualizado de log-retornos = sqrt(12)*sd(log_retornos_diarios) <=> 
desvio_padrao_anualizado_log_retornos <- log_retornos_diarios %>% 
  summarise(across(all_of(acoes_escolhidas), ~sqrt(12)*sd(.x, na.rm=TRUE)))

