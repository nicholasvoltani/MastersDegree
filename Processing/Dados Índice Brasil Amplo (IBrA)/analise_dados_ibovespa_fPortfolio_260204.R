library(tidyverse)
library(tidyr)
library(slider) # Para operações do tipo "sliding window"
library(comprehenr) # Para "list comprehension" à la Python
library(ggplot2)
library(corrplot)
library(fPortfolio)
library(timeSeries) # stats::filter sobrescreve dplyr::filter, CUIDADO!!!

###
# Parâmetros variáveis neste script (mudar conforme necessário/desejado)
INPUT_FILEPATH <- "C:/Users/nicho/Documents/MastersDegree/DataInputs/Dados Índice Brasil Amplo (IBrA)/Cotacoes18a25.csv"
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
                ~ .x/dplyr::lag(.x) - 1) 
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

# retornos_diarios_percentuais <- retornos_diarios %>%
#   mutate(across(all_of(acoes_e_risk_free),
#                 ~ .x * 100))
# log_retornos_diarios_percentuais <- log_retornos_diarios %>%
#   mutate(across(all_of(acoes_e_risk_free),
#          ~ .x * 100))
# mercado_retornos_diarios_percentuais <- mercado_retornos_diarios %>%
#   mutate(across(all_of(colunas_mercado),
#                 ~ .x * 100))
# mercado_log_retornos <- mercado_log_retornos_diarios %>%
#   mutate(across(all_of(colunas_mercado),
#                 ~ .x * 100))

# Visualização de matriz de correlação entre log-RETORNOS das ações
corrplot(log_retornos_diarios %>% 
           select(all_of(acoes_escolhidas)) %>% 
           cor(use = "complete.obs"), 
         method = 'square', 
         order = 'FPC', 
         type = 'lower', 
         diag = FALSE)


# Retornos mensais
## Retornos mensais requerem multiplicação dos retornos diários
## Log-retornos mensais requerem soma dos log-retornos diários
## Log-retornos_mensais = sum(Log-retornos_diários)  <=>  Retornos_mensais = prod(1 + retornos_diários) - 1
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


# retornos_mensais_percentuais <- retornos_mensais %>% 
#   mutate(across(all_of(acoes_e_risk_free), 
#                 ~ .x * 100))
# log_retornos_mensais_percentuais <- log_retornos_mensais %>% 
#   mutate(across(all_of(acoes_e_risk_free),
#                 ~ .x * 100))
# mercado_retornos_mensais_percentuais <- mercado_retornos_mensais %>% 
#   mutate(across(all_of(colunas_mercado), 
#                 ~ .x * 100))
# mercado_log_retornos_mensais_percentuais <- mercado_log_retornos_mensais %>%
#   mutate(across(all_of(colunas_mercado),
#                 ~ .x * 100))


# Criação de janelas de tempo de $QTD_MESES_JANELA_MOVEL$
## Notar que primeiros $QTD_MESES_JANELA_MOVEL$ serão NULL devido ao .complete=TRUE
janelas_tempo <- slide_period(.x = log_retornos_diarios$Data,
                              .i = log_retornos_diarios$Data,
                              .period = "month", 
                              .f = ~ .x, 
                              .origin=DATA_INICIAL_REFERENCIA,
                              .before=QTD_MESES_JANELA_MOVEL-1, # -1 pois conta o mês anterior INTEIRO (?)
                              .complete=TRUE
                              )

# Log-retornos [!] MENSAIS para otimizações de portfolio / aferições de retornos obtidos
## Parte-se do princípio que haverá otimização -> compra de ações -> venda após 1 mês inteiro -> recálculo de otimização etc.
## Portanto, o retorno descreve o rendimento que vem do diferencial obtido entre a compra e a venda das ações, a cada mês
anos_meses_otimizacao <- ibra2 %>%
  dplyr::filter(Data >= PRIMEIRO_MES_OTIMIZACAO & 
           Data < ULTIMO_MES_OTIMIZACAO) %>% 
  mutate(ano_mes = paste0(ano, "_", mes)) %>% 
  select(ano_mes) %>% 
  unique

log_retornos_compra_venda <- ibra2 %>%
  dplyr::filter(Data >= PRIMEIRO_MES_OTIMIZACAO & 
           Data < ULTIMO_MES_OTIMIZACAO) %>% 
  select(Data,
         ano,
         mes,
         all_of(acoes_e_risk_free),
         all_of(colunas_mercado)) %>% 
  group_by(ano, mes) %>% 
  summarise(across(-Data,
                   ~ log(last(.x, order_by=Data)/first(.x, order_by=Data))
                   )
            ) %>% 
  ungroup #%>% 
  # mutate(ano_mes = paste0(ano, "_", mes)) %>% 
  # relocate(ano_mes, .after=mes) %>% 
  # select(-c(ano, mes, ano_mes))

log_retornos_compra_venda_portfolio_risk_and_free <- log_retornos_compra_venda %>% 
  select(all_of(acoes_e_risk_free))

log_retornos_compra_venda_portfolio_risky <- log_retornos_compra_venda %>% 
  select(all_of(acoes_escolhidas))

log_retornos_compra_venda_mercado <- log_retornos_compra_venda %>% 
  select(all_of(colunas_mercado))



# Separação de dados dentro das janelas de tempo adequadas
## Otimizações de portfolio serão com log-retornos
## Gerará 1 portfolio de estimativa inicial + $QTD_MESES_OTIMIZACAO$ de re-cálculo
### Pega somente [[janelas_tempo]] para t \in [QTD_MESES_JANELA_MOVEL, QTD_MESES_JANELA_MOVEL+QTD_MESES_OTIMIZACAO],
### porque primeiros (QTD_MESES_JANELA_MOVEL-1) elementos são NULL
risky_portfolios <- comprehenr::to_list(for (t in (QTD_MESES_JANELA_MOVEL):(QTD_MESES_JANELA_MOVEL+QTD_MESES_OTIMIZACAO) )
  log_retornos_diarios %>% 
    select(-indice_CDI, 
           -mes, 
           -ano) %>% 
    dplyr::filter(Data %in% janelas_tempo[[t]])
  )

risk_plus_free_portfolios <- comprehenr::to_list(for (t in (QTD_MESES_JANELA_MOVEL):(QTD_MESES_JANELA_MOVEL+QTD_MESES_OTIMIZACAO) )
  log_retornos_diarios %>% 
    select(-mes, 
           -ano) %>% 
    dplyr::filter(Data %in% janelas_tempo[[t]])
  )

risk_free_portfolios <- comprehenr::to_list(for (t in (QTD_MESES_JANELA_MOVEL):(QTD_MESES_JANELA_MOVEL+QTD_MESES_OTIMIZACAO) )
  log_retornos_diarios %>%
    select(-all_of(acoes_escolhidas), 
           -mes,
           -ano) %>% 
    dplyr::filter(Data %in% janelas_tempo[[t]])
  )



# Fronteira eficiente dos dados de estimação
log_retornos_ts = as.timeSeries(log_retornos_diarios %>%
                                              select(-c(ano, 
                                                        mes,
                                                        indice_CDI)) %>% 
                                              dplyr::filter(Data >= PRIMEIRO_MES_OTIMIZACAO & 
                                                              Data < ULTIMO_MES_OTIMIZACAO))

specs_portfolio_eficiente <- portfolioSpec()
setNFrontierPoints(specs_portfolio_eficiente) <- 100

fronteira_eficiente <- portfolioFrontier(log_retornos_ts, specs_portfolio_eficiente)

# Plot da fronteira eficiente
# frontierPlot(fronteira_eficiente)
# 
# tailoredFrontierPlot(object= fronteira_eficiente,
#   return = c("mean"), 
#   risk = c( "Sigma"), 
#   mText = "MV Portfolio – LongOnly Constraints", 
#   sharpeRatio = FALSE)
# 
tailoredFrontierPlot(object= fronteira_eficiente,
  return = c("mean"),
  risk = c( "Sigma"),
  mText = "MV Portfolio – LongOnly Constraints",
  sharpeRatio = TRUE)

# Plot interativo (console abaixo, na página)
# plot(fronteira_eficiente)


# Fronteira eficiente -- risk+free portfolio dos dados de estimação
log_retornos_ts_risk_plus_free = as.timeSeries(log_retornos_diarios %>%
                                                             select(-c(ano, 
                                                                       mes)) %>% 
                                                             dplyr::filter(Data >= PRIMEIRO_MES_OTIMIZACAO & 
                                                                             Data < ULTIMO_MES_OTIMIZACAO))

specs_portfolio_eficiente_risk_plus_free <- portfolioSpec()
setNFrontierPoints(specs_portfolio_eficiente_risk_plus_free) <- 100

fronteira_eficiente_risk_plus_free <- portfolioFrontier(log_retornos_ts_risk_plus_free, 
                                                        specs_portfolio_eficiente_risk_plus_free)

tailoredFrontierPlot(object= fronteira_eficiente_risk_plus_free,
  return = c("mean"),
  risk = c( "Sigma"),
  mText = "MV Portfolio – LongOnly Constraints",
  sharpeRatio = TRUE)

# Plot interativo (console abaixo, na página)
# plot(fronteira_eficiente_risk_plus_free)



# Cálculos do portfolio de variância mínima -- USA LOG-RETORNOS!!!
## "LongOnly" requer pesos >= 0 (sem vendas a descoberto, i.e. pesos negativos)
## Cada otimização é feita visando o mês logo após:
### primeira estimação é feita com os meses inteiros 01/2018 - 12/2019, para alocar para 01/2020
### segunda estimação usa meses 02/2018 - 01/2020, para alocar para 02/2020, etc.
log_returns_risky_ptf <- c()
risky_ptf_variances <- c()
log_returns_risk_plus_free_ptf <- c()
risk_plus_free_ptf_variances <- c()

## C/ vendas a descoberto
log_returns_risky_ptf_short <- c()
risky_ptf_variances_short <- c()
log_returns_risk_plus_free_ptf_short <- c()
risk_plus_free_ptf_variances_short <- c()

specs_portfolio_var_minima <- portfolioSpec()
shortSpec <- portfolioSpec()
setSolver(shortSpec) <- "solveRshortExact"

for (t in 1:(QTD_MESES_OTIMIZACAO)) {
  # Ordem das otimizações: 
  ## 1) Somente ações (Long);
  ## 2) Ações + risk-free (Long); 
  ## 3) Somente ações (Short); 
  ## 4) Ações + risk-free (Short)
  
  # Otimizações de portfolio: 
  min_var_risky_portfolio <- minvariancePortfolio(data = as.timeSeries(risky_portfolios[[t]]),
                                                  spec = specs_portfolio_var_minima,
                                                  constraints = "LongOnly")
  min_var_risk_plus_free_portfolio <- minvariancePortfolio(data = as.timeSeries(risk_plus_free_portfolios[[t]]),
                                                           spec = specs_portfolio_var_minima,
                                                           constraints = "LongOnly")
  min_var_risky_portfolio_short <- minvariancePortfolio(data = as.timeSeries(risky_portfolios[[t]]),
                                                        spec = shortSpec,
                                                        constraints = "Short")
  min_var_risk_plus_free_portfolio_short <- minvariancePortfolio(data = as.timeSeries(risk_plus_free_portfolios[[t]]),
                                                        spec = shortSpec,
                                                        constraints = "Short")
  # Cálculos de pesos óptimos  
  weights_risky_ptf <- getWeights(min_var_risky_portfolio)
  weights_risk_plus_free_ptf <- getWeights(min_var_risk_plus_free_portfolio)
  weights_risky_ptf_short <- getWeights(min_var_risky_portfolio_short)
  weights_risk_plus_free_ptf_short <- getWeights(min_var_risk_plus_free_portfolio_short)
  
  
  # Cálculo de matrizes de covariância (somente diz respeito aos assets, não a Long/Short)
  risky_covariance_matrix <- risky_portfolios[[t]] %>% select(-Data) %>% cov
  risk_plus_free_covariance_matrix <- risk_plus_free_portfolios[[t]] %>% select(-Data) %>% cov

  # Cálculo de retornos ponderados pelos pesos, obtidos no mês `t`
  log_return_risky_ptf <- weights_risky_ptf %*% t(log_retornos_compra_venda_portfolio_risky %>% slice(t))
  log_returns_risky_ptf <- c(log_returns_risky_ptf, log_return_risky_ptf)

  log_return_risk_plus_free_ptf <- weights_risk_plus_free_ptf %*% t(log_retornos_compra_venda_portfolio_risk_and_free %>% slice(t))
  log_returns_risk_plus_free_ptf <- c(log_returns_risk_plus_free_ptf,log_return_risk_plus_free_ptf)

  log_return_risky_ptf_short <- weights_risky_ptf_short %*% t(log_retornos_compra_venda_portfolio_risky %>% slice(t))
  log_returns_risky_ptf_short <- c(log_returns_risky_ptf_short, log_return_risky_ptf_short)
  
  log_return_risk_plus_free_ptf_short <- weights_risk_plus_free_ptf_short %*% t(log_retornos_compra_venda_portfolio_risk_and_free %>% slice(t))
  log_returns_risk_plus_free_ptf_short <- c(log_returns_risk_plus_free_ptf_short, log_return_risk_plus_free_ptf_short)

  
  # Cálculo de variâncias dos portfolios
  risky_ptf_variance <- t(weights_risky_ptf) %*% risky_covariance_matrix %*% weights_risky_ptf
  risky_ptf_variances <- c(risky_ptf_variances, risky_ptf_variance)

  risk_plus_free_ptf_variance <- t(weights_risk_plus_free_ptf) %*% risk_plus_free_covariance_matrix %*% weights_risk_plus_free_ptf
  risk_plus_free_ptf_variances <- c(risk_plus_free_ptf_variances, risk_plus_free_ptf_variance)

  risky_ptf_variance_short <- t(weights_risky_ptf_short) %*% risky_covariance_matrix %*% weights_risky_ptf_short
  risky_ptf_variances_short <- c(risky_ptf_variances_short, risky_ptf_variance_short)

  risk_plus_free_ptf_variance_short <- t(weights_risk_plus_free_ptf_short) %*% risk_plus_free_covariance_matrix %*% weights_risk_plus_free_ptf_short
  risk_plus_free_ptf_variances_short <- c(risk_plus_free_ptf_variances_short, risk_plus_free_ptf_variance_short)
  
}

# Cálculos de desvios padrão
risky_ptf_stdevs <- risky_ptf_variances %>% sqrt
risk_plus_free_ptf_stdevs <- risk_plus_free_ptf_variances %>% sqrt
risky_ptf_stdevs_short <- risky_ptf_variances_short %>% sqrt
risk_plus_free_ptf_stdevs_short <- risk_plus_free_ptf_variances_short %>% sqrt

# Comparações com Ibovespa e IBrA
timeseries_ibovespa <- log_retornos_compra_venda_mercado %>% select(IBOV) %>% as.timeSeries
timeseries_ibra <- log_retornos_compra_venda_mercado %>% select(IBRA) %>% as.timeSeries

corr_risky_ibov <- cor(log_returns_risky_ptf,
                       timeseries_ibovespa)
corr_risk_plus_free_ibov <- cor(log_returns_risk_plus_free_ptf,
                                timeseries_ibovespa)

corr_risky_short_ibov <- cor(log_returns_risky_ptf_short,
                             timeseries_ibovespa)
corr_risk_plus_free_short_ibov <- cor(log_returns_risk_plus_free_ptf_short,
                                      timeseries_ibovespa)

# Plot
to_plot <- data.frame(
  x = anos_meses_otimizacao, # Dataframe cuja única coluna se chama `ano_mes`
  risky = log_returns_risky_ptf %>% cumsum,
  risky_plus_free = log_returns_risk_plus_free_ptf %>% cumsum,
  risky_short = log_returns_risky_ptf_short %>% cumsum,
  risky_plus_free_short = log_returns_risk_plus_free_ptf_short %>% cumsum,
  ibovespa = timeseries_ibovespa %>% cumsum # Tem coluna `IBOV`
  ) %>% 
  mutate(ano_mes = ano_mes %>% zoo::as.yearmon(format = "%Y_%m"))

to_plot_total <- tidyr::pivot_longer(
  to_plot,
  cols = c(risky, risky_plus_free, 
           risky_short, risky_plus_free_short,
           IBOV),
  names_to = "series",
  values_to = "value"
)

to_plot_long <- tidyr::pivot_longer(
  to_plot %>% select(-c(risky_short, risky_plus_free_short)),
  cols = c(risky, risky_plus_free, 
           # risky_short, risky_plus_free_short, 
           IBOV),
  names_to = "series",
  values_to = "value"
)

to_plot_short <- tidyr::pivot_longer(
  to_plot %>% select(-c(risky, risky_plus_free)),
  cols = c(
    # risky, risky_plus_free, 
           risky_short, risky_plus_free_short,
           IBOV),
  names_to = "series",
  values_to = "value"
)


# Gráfico comparando todos os portfolios
ggplot(to_plot_total, aes(x = ano_mes, 
                         y = value, 
                         color = series)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Log-retornos mensais acumulados",
    subtitle = "Legenda mostra correlações com Ibovespa & log-retorno acumulado de compra-venda (começo/fim do mês)",
    x = "Tempo",
    y = "% Log-retornos mensais médios",
    color = "Séries"
  ) +
  scale_color_manual(
    values = c(
      IBOV = "gold",
      risky = "darkorange",
      risky_short = "red",
      risky_plus_free = "blue",
      risky_plus_free_short = "green"
      ),
    labels = c(paste0("Ibovespa (",
                      round(sum(log_retornos_compra_venda_mercado$IBOV), 2), ")"),
               paste0("Risky (", round(corr_risky_ibov, 2), ", ", 
                      round(sum(log_returns_risky_ptf), 2), ")"), 
               paste0("Risky+Short (", round(corr_risky_short_ibov, 2), ", ", 
                      round(sum(log_returns_risky_ptf_short), 2), ")"),
               paste0("Risky+Free (", round(corr_risk_plus_free_ibov, 2), ", ", 
                      round(sum(log_returns_risk_plus_free_ptf), 2), ")"),
               paste0("Risky+Free+Short (", round(corr_risk_plus_free_short_ibov, 2), ", ", 
                      round(sum(log_returns_risk_plus_free_ptf_short), 2), ")")
               )
    ) +
  theme_minimal()


###############
# ggplot(to_plot_total %>% dplyr::filter(series %in% c("risky_plus_free", "risky_plus_free_short")), 
#        aes(x = ano_mes, 
#            y = value, 
#            color = series)) +
#   geom_line(linewidth = 1) +
#   labs(
#     title = "Log-retornos mensais acumulados",
#     subtitle = "Legenda mostra correlações com Ibovespa & log-retorno acumulado de compra-venda (começo/fim do mês)",
#     x = "Tempo",
#     y = "% Log-retornos mensais médios",
#     color = "Séries"
#   ) +
#   scale_color_manual(
#     values = c(
#       # IBOV = "gold",
#       # risky = "darkorange",
#       # risky_short = "red",
#       risky_plus_free = "blue",
#       risky_plus_free_short = "green"
#     ),
#     labels = c(
#       # paste0("Ibovespa (",
#       #                 round(sum(log_retornos_compra_venda_mercado$IBOV), 2), ")"),
#       #          paste0("Risky (", round(corr_risky_ibov, 2), ", ",
#       #                 round(sum(log_returns_risky_ptf), 2), ")"),
#       #          paste0("Risky+Short (", round(corr_risky_short_ibov, 2), ", ",
#       #                 round(sum(log_returns_risky_ptf_short), 2), ")"),
#                paste0("Risky+Free (", round(corr_risk_plus_free_ibov, 2), ", ",
#                       round(sum(log_returns_risk_plus_free_ptf), 2), ")"),
#                paste0("Risky+Free+Short (", round(corr_risk_plus_free_short_ibov, 2), ", ", 
#                       round(sum(log_returns_risk_plus_free_ptf_short), 2), ")")
#     )
#   ) +
#   theme_minimal()
################


# Gráfico comparando portfolios sem vendas a descoberto
# ggplot(to_plot_long, aes(x = ano_mes, 
#                          y = value, 
#                          color = series)) +
#   geom_line(linewidth = 1) +
#   labs(
#     title = "Log-retornos médios mensais",
#     subtitle = "Legenda mostra correlações com Ibovespa",
#     x = "Tempo",
#     y = "% Log-retornos mensais médios",
#     color = "Séries"
#   ) +
#   scale_color_manual(
#     values = c(
#       IBOV = "gold",
#       risky = "red",
#       risky_short = "orange",
#       risky_plus_free = "darkgreen",
#       risky_plus_free_short = "blue"
#       ),
#     labels = c("Ibovespa",
#                paste0("Risky: ", round(corr_risky_ibov, 2)), 
#                paste0("Risky+Free: ", round(corr_risk_plus_free_ibov, 2)),
#                paste0("Risky+Short: ", round(corr_risky_short_ibov, 2)),
#                paste0("Risky+Free+Short: ", round(corr_risk_plus_free_short_ibov, 2))
#                )
#     ) +
#   theme_minimal()


# Gráfico comparando portfolios com vendas a descoberto
# ggplot(to_plot_short, aes(x = ano_mes, 
#                          y = value, 
#                          color = series)) +
#   geom_line(linewidth = 1) +
#   labs(
#     title = "Log-retornos mensais cumulativos",
#     subtitle = "Legenda mostra correlações com Ibovespa",
#     x = "Tempo",
#     y = "Log-retorno",
#     color = "Séries"
#   ) +
#   scale_color_manual(
#     values = c(
#       IBOV = "gold",
#       # risky = "red",
#       risky_short = "darkorange",
#       # risky_plus_free = "darkgreen"
#       risky_plus_free_short = "blue"
#     ),
#     labels = c("Ibovespa",
#                # paste0("Risky: ", round(corr_risky_ibov, 2)), 
#                # paste0("Risky+Free: ", round(corr_risk_plus_free_ibov, 2))
#                paste0("Risky+Short: ", round(corr_risky_short_ibov, 2)),
#                paste0("Risky+Free+Short: ", round(corr_risk_plus_free_short_ibov, 2))
#     )
#   ) +
#   theme_minimal()



