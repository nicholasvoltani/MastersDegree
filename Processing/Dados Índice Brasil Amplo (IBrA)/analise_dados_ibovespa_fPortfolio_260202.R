library(tidyverse)
library(slider) # Para operações do tipo "sliding window"
library(comprehenr) # Para "list comprehension" à la Python


###
# Parâmetros variáveis neste script (mudar conforme necessário/desejado)
INPUT_FILEPATH <- "C:/Users/nicho/Documents/MastersDegree/DataInputs/Dados Índice Brasil Amplo (IBrA)/Cotacoes18a25.csv"
NUMERO_PONTOS_FRONTEIRA_EFICIENTE <- 50
DATA_INICIAL_REFERENCIA <- ymd("2018-02-01")
QTD_MESES_JANELA_MOVEL <- 24
QTD_MESES_OTIMIZACAO <- 60

# Primeira otimização tem Data >= PRIMEIRO_MES_OTIMIZACAO
# Última otimização tem Data < ULTIMO_MES_OTIMIZACAO
PRIMEIRO_MES_OTIMIZACAO <- ym("2020-01") 
ULTIMO_MES_OTIMIZACAO <- PRIMEIRO_MES_OTIMIZACAO + months(QTD_MESES_OTIMIZACAO)
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
  mutate(indice_CDI = cumprod((1 + `CDI 252 dias` / 100)^(1/252)) / 
           first((1 + `CDI 252 dias` / 100)^(1/252)),  # `first` garante que primeiro índice será 1
         mes = month(Data),
         ano = year(Data)) %>% 
  relocate(mes, .after = Data) %>%
  relocate(ano, .after = mes)  
        

  
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

acoes_e_risk_free <- c(acoes_escolhidas, "indice_CDI")

# Retornos diários
## Retornos diários: R_t = P_t/P_{t-1} - 1
## Log-retornos diários: r_t = log(P_t/P_{t-1}) = log(1 + R_t)
retornos_diarios <- ibra2 %>% 
  arrange(Data) %>% 
  select(c(Data,
           mes,
           ano,
           all_of(acoes_e_risk_free)
           )
         ) %>% 
  mutate(across(-c(Data, mes, ano), 
                ~ .x/lag(.x) - 1) 
         ) %>% 
  slice(2:n())

log_retornos_diarios <- ibra2 %>% 
  arrange(Data) %>% 
  select(c(Data,
           mes,
           ano,
           all_of(acoes_e_risk_free)
  )
  ) %>% 
  mutate(across(-c(Data, mes, ano), 
                ~ log(.x/lag(.x))
                )
         ) %>%
  slice(2:n())

retornos_diarios_percentuais <- retornos_diarios %>%
  mutate(across(all_of(acoes_e_risk_free),
                ~ .x * 100))
log_retornos_diarios_percentuais <- log_retornos_diarios %>%
  mutate(across(all_of(acoes_e_risk_free),
         ~ .x * 100))




# Retornos mensais
## Retornos mensais requerem multiplicação dos retornos diários
## Log-retornos mensais requerem soma dos log-retornos diários
## Log-retornos_mensais = sum(Log-retornos_diários)  <=>  1 + Retornos_mensais = prod(1 + retornos_diários)
retornos_mensais <- retornos_diarios %>% 
  group_by(ano, mes) %>% 
  summarise(across(all_of(acoes_e_risk_free), 
                   ~ prod(1+.x,na.rm=TRUE) - 1
                   )) %>% 
  ungroup()

log_retornos_mensais <- log_retornos_diarios %>% 
  group_by(ano, mes) %>% 
  summarise(across(all_of(acoes_e_risk_free), 
                   ~ sum(.x, na.rm=TRUE)
                   )) %>% 
  ungroup()

retornos_mensais_percentuais <- retornos_mensais %>% 
  mutate(across(all_of(acoes_e_risk_free), 
                ~ .x * 100))
log_retornos_mensais_percentuais <- log_retornos_mensais %>% 
  mutate(across(all_of(acoes_e_risk_free),
                ~ .x * 100))



# Informações de mercado
mercado_retornos_diarios <- mercado %>% 
  arrange(Data) %>% 
  mutate(across(all_of(colunas_mercado),
                ~ .x/lag(.x) - 1)
         ) %>% 
  slice(2:n())
## Obs: retorno_CDI não tem primeiro valor como NA porque ele próprio deriva de um retorno pré-calculado (`CDI 252 dias`)

mercado_log_retornos_diarios <- mercado %>%
  arrange(Data) %>%
  mutate(across(all_of(colunas_mercado),
                ~ log(.x/lag(.x)))
         ) %>%
  slice(2:n())

mercado_retornos_diarios_percentuais <- mercado_retornos_diarios %>%
  mutate(across(all_of(colunas_mercado),
                ~ .x * 100))
mercado_log_retornos_percentuais <- mercado_log_retornos_diarios %>%
  mutate(across(all_of(colunas_mercado),
                ~ .x * 100))


mercado_retornos_mensais <- mercado_retornos_diarios %>% 
  group_by(ano, mes) %>% 
  summarise(across(all_of(colunas_mercado), 
                   ~ prod(1+.x, na.rm=TRUE) - 1
                   )) %>% 
  ungroup()

mercado_log_retornos_mensais <- mercado_log_retornos_diarios %>%
  group_by(ano, mes) %>%
  summarise(across(all_of(colunas_mercado),
                   ~ sum(.x, na.rm=TRUE)
                   )) %>%
  ungroup()

mercado_retornos_mensais_percentuais <- mercado_retornos_mensais %>% 
  mutate(across(all_of(colunas_mercado), 
                ~ .x * 100))
mercado_log_retornos_mensais_percentuais <- mercado_log_retornos_mensais %>%
  mutate(across(all_of(colunas_mercado),
                ~ .x * 100))


# Criação de janelas de tempo de $QTD_MESES_JANELA_MOVEL$
## Notar que primeiros $QTD_MESES_JANELA_MOVEL$ serão NULL devido ao .complete=TRUE
janelas_tempo <- slide_period(log_retornos_diarios$Data,
                              log_retornos_diarios$Data,
                              "month", 
                              ~ .x, 
                              .origin=DATA_INICIAL_REFERENCIA,
                              .before=QTD_MESES_JANELA_MOVEL-1,
                              .complete=TRUE
                              )

retornos_percentuais_compra_venda <- ibra2 %>%
  filter(Data >= PRIMEIRO_MES_OTIMIZACAO & 
           Data < ULTIMO_MES_OTIMIZACAO) %>% 
  select(Data,
         ano,
         mes,
         all_of(acoes_e_risk_free),
         all_of(colunas_mercado)) %>% 
  group_by(ano, mes) %>% 
  summarise(across(-Data,
                   ~ 100 * (last(.x, order_by=Data)/first(.x, order_by=Data) - 1)
                   )
            ) %>% 
  ungroup %>% 
  mutate(ano_mes = paste0(ano, "_", mes)) %>% 
  relocate(ano_mes, .after=mes) %>% 
  select(-c(ano, mes, ano_mes))

retornos_percentuais_compra_venda_portfolio_risk_and_free <- retornos_percentuais_compra_venda %>% 
  select(all_of(acoes_e_risk_free))

retornos_percentuais_compra_venda_portfolio_risky <- retornos_percentuais_compra_venda %>% 
  select(all_of(acoes_escolhidas))

retornos_percentuais_compra_venda_mercado <- retornos_percentuais_compra_venda %>% 
  select(all_of(colunas_mercado))



# Separação de dados dentro das janelas de tempo adequadas
## Otimizações de portfolio serão com log-retornos percentuais
## Gerará 1 portfolio de estimativa inicial + $QTD_MESES_OTIMIZACAO$ de re-cálculo
risky_portfolios <- comprehenr::to_list(for (t in (QTD_MESES_JANELA_MOVEL):(QTD_MESES_JANELA_MOVEL+QTD_MESES_OTIMIZACAO) )
  log_retornos_diarios_percentuais %>% 
    select(-indice_CDI, 
           -mes, 
           -ano) %>% 
    filter(Data %in% janelas_tempo[[t]])
  )

risk_plus_free_portfolios <- comprehenr::to_list(for (t in (QTD_MESES_JANELA_MOVEL):(QTD_MESES_JANELA_MOVEL+QTD_MESES_OTIMIZACAO) )
  log_retornos_diarios_percentuais %>% 
    select(-mes, 
           -ano) %>% 
    filter(Data %in% janelas_tempo[[t]])
  )

risk_free_portfolio <- comprehenr::to_list(for (t in (QTD_MESES_JANELA_MOVEL):(QTD_MESES_JANELA_MOVEL+QTD_MESES_OTIMIZACAO) )
  log_retornos_diarios_percentuais %>%
    select(-all_of(acoes_escolhidas), 
           -mes,
           -ano) %>% 
    filter(Data %in% janelas_tempo[[t]])
  )



## Lendo estes pacotes no meio do script (um pecado capital),
## porque, POR ALGUM MOTIVO, ele destrói o cálculo dos retornos acima,
## se esses pacotes são lidos antes dos cálculos (???????)
### Ou seja, se for rodar coisas antes dessas linhas de novo, feche e abra o RStudio (so it goes...)
library(fPortfolio)
library(timeSeries)

# Conversão para séries temporais
retornos_percentuais_ts = as.timeSeries(retornos_diarios_percentuais)
log_retornos_percentuais_ts = as.timeSeries(log_retornos_diarios_percentuais)


# Inicialização de cálculo de fronteira eficiente
specs_portfolio_eficiente <- portfolioSpec()
setNFrontierPoints(specs_portfolio_eficiente) <- 50

# Cálculo da fronteira eficiente (retornos percentuais comuns, não log)
fronteira_eficiente <- portfolioFrontier(retornos_percentuais_ts, specs_portfolio_eficiente)

# Plot da fronteira eficiente
frontierPlot(fronteira_eficiente)

tailoredFrontierPlot(object= fronteira_eficiente,
  return = c("mean"), 
  risk = c( "Sigma"), 
  mText = "MV Portfolio – LongOnly Constraints", 
  sharpeRatio = FALSE)

tailoredFrontierPlot(object= fronteira_eficiente, 
  return = c("mean"), 
  risk = c( "Sigma"), 
  mText = "MV Portfolio – LongOnly Constraints", 
  sharpeRatio = TRUE)

# Plot interativo (console abaixo, na página)
plot(fronteira_eficiente)


# Cálculos do portfolio de variância mínima -- USA LOG-RETORNOS!!!
## "LongOnly" requer pesos >= 0 (sem vendas a descoberto, i.e. pesos negativos)
## Cada otimização é feita visando o mês logo após:
### primeira estimação é feita com os meses inteiros 01/2018 - 12/2019, para alocar para 01/2020
### segunda estimação usa meses 02/2018 - 01/2020, para alocar para 02/2020, etc.
average_returns_risky_ptf <- c()
average_returns_risk_plus_free_ptf <- c()
specs_portfolio_var_minima <- portfolioSpec()

for (t in 1:(QTD_MESES_OTIMIZACAO)) {
  # Otimização do portfolio somente com ações
  min_var_risky_portfolio <- minvariancePortfolio(data = as.timeSeries(risky_portfolios[[t]]), 
                                                  spec = specs_portfolio_var_minima,
                                                  constraints = "LongOnly")
  weights_risky_ptf <- getWeights(min_var_risky_portfolio)
  average_return_risky_ptf <- weights_risky_ptf %*% t(retornos_percentuais_compra_venda_portfolio_risky %>%
                                                        slice(t))
  average_returns_risky_ptf <- c(average_returns_risky_ptf, average_return_risky_ptf)
  
  
  # Otimização do portfolio com ações & risk-free rate (log-retorno diário do CDI)
  min_var_risk_plus_free_portfolio <- minvariancePortfolio(data = as.timeSeries(risk_plus_free_portfolios[[t]]), 
                                                           spec = specs_portfolio_var_minima,
                                                           constraints = "LongOnly")
  weights_risk_plus_free_ptf <- getWeights(min_var_risk_plus_free_portfolio)
  average_return_risk_plus_free_ptf <- weights_risk_plus_free_ptf %*% t(retornos_percentuais_compra_venda_portfolio_risk_and_free %>%
                                                                 slice(t))
  average_returns_risk_plus_free_ptf <- c(average_returns_risk_plus_free_ptf, average_return_risk_plus_free_ptf)
   
}

# Comparações com Ibovespa e IBrA
timeseries_ibovespa <- retornos_percentuais_compra_venda_mercado %>% select(IBOV) %>% as.timeSeries
timeseries_ibra <- retornos_percentuais_compra_venda_mercado %>% select(IBRA) %>% as.timeSeries

plot(cbind(average_returns_risky_ptf,
           average_returns_risk_plus_free_ptf,
           timeseries_ibovespa,
           timeseries_ibra),
     plot.type=c("single"),
     col=c('red','green', 'purple', 'gold'))

