library(tidyverse)
library(tidyr)
library(slider) # For "sliding window" operations
library(comprehenr) # "List comprehension" à la Python
library(ggplot2)
library(corrplot)
library(fPortfolio)
library(timeSeries) # stats::filter overwrites dplyr::filter, BEWARE!!!
library(readxl)
library(lubridate)

###
# Fixed script parameters
INPUT_FILEPATH <- "C:/Users/nicho/Documents/MastersDegree/DataInputs/Dados Índice Brasil Amplo (IBrA)/Cotacoes18a25.csv"
COMPOSICAO_FILEPATH <- "C:/Users/nicho/Documents/MastersDegree/DataInputs/Dados Índice Brasil Amplo (IBrA)/CotaçõesIBRA.xlsx"
EFFICIENT_FRONTIER_DATAPOINTS <- 50
INITIAL_ESTIMATION_DATE <- ymd("2018-01-01")
SLIDING_WINDOW_MONTHS <- 24
LAST_ESTIMATION_DATE <- INITIAL_ESTIMATION_DATE + months(SLIDING_WINDOW_MONTHS) - 1

# First optimization occurs for Data >= PRIMEIRO_MES_OTIMIZACAO
# Last optimization occurs within < ULTIMO_MES_OTIMIZACAO
FIRST_OPTIMIZATION_YEARMON <- zoo::as.yearmon("2020-01", format="%Y-%m") 
TOTAL_OPTIMIZATION_YEARS <- 5 # `zoo`'s logic is that adding to `yearmon` objects add YEARS
TOTAL_OPTIMIZATION_MONTHS <- TOTAL_OPTIMIZATION_YEARS * 12
LAST_OPTIMIZATION_YEARMON <- FIRST_OPTIMIZATION_YEARMON + TOTAL_OPTIMIZATION_YEARS

#
###


# Lendo dados
ibra <- read_delim(INPUT_FILEPATH,
                   delim=";")

# Preprocessing: 
## Fixing date (`Data`) variable (lubridate::dmy = "day, month, year")
## Creating daily CDI index (risk-free rate, Brazilian "Taxa Selic") (requires "de-annualizing")
ibra2 <- ibra %>%
  mutate(Data = Data %>% str_replace_all("/", "-") %>% dmy()) %>% 
  arrange(Data) %>% 
  mutate(indice_CDI = cumprod((1 + `CDI 252 dias`/100)^(1/252)) / 
           first((1 + `CDI 252 dias`/100)^(1/252)),  # `first`guarantees first calculated index will be 1
         mes = month(Data),
         ano = year(Data)) %>% 
  relocate(mes, .after = Data) %>%
  relocate(ano, .after = mes)  
        
composicao_ibra <- read_xlsx(COMPOSICAO_FILEPATH,
                             sheet="Composição IBRA")


  
# Market info
mkt_columns <- c("indice_CDI",
                 "IBOV",
                 "IBRA")

# market_data <- ibra2 %>% 
#   select(c(Data,
#            mes,
#            ano,
#            all_of(TOTAL_OPTIMIZATION_MONTHS))
#          )

# Stock tickers
all_stocks <- dplyr::setdiff(colnames(ibra2), 
                             c(mkt_columns, "Data", "mes", "ano", "CDI 252 dias"))


#
net_returns <- ibra2 %>% 
  arrange(Data) %>% 
  group_by(ano, mes) %>% 
  summarise(across(-Data,
                   ~ last(.x, order_by=Data)/first(.x, order_by=Data) - 1
  )
  ) %>% 
  ungroup %>% 
  mutate(ano_mes = paste0(ano, "_", mes) %>% zoo::as.yearmon(format="%Y_%m")) %>% 
  relocate(ano_mes, .after=mes)

log_returns <- ibra2 %>% 
  arrange(Data) %>%
  group_by(ano, mes) %>% 
  summarise(across(-Data,
                   ~ log(last(.x, order_by=Data)/first(.x, order_by=Data))
  )
  ) %>% 
  ungroup %>% 
  mutate(ano_mes = paste0(ano, "_", mes) %>% zoo::as.yearmon(format="%Y_%m")) %>% 
  relocate(ano_mes, .after=mes)


# Calculating stock returns within estimation reference window:
## stocks being bought at first weekday of the month,
## and being sold at the last weekday, and so on

# net_returns_estimation <- net_returns %>%
#   dplyr::filter(ano_mes >= INITIAL_ESTIMATION_DATE %>% zoo::as.yearmon() &
#                   ano_mes < LAST_ESTIMATION_DATE %>% zoo::as.yearmon())

log_returns_estimation <- log_returns %>%
  dplyr::filter(ano_mes >= INITIAL_ESTIMATION_DATE %>% zoo::as.yearmon() &
                  ano_mes < LAST_ESTIMATION_DATE %>% zoo::as.yearmon())

# net_return_optimization <- net_returns %>%
#   dplyr::filter(ano_mes >=  FIRST_OPTIMIZATION_YEARMON &
#                   ano_mes < LAST_OPTIMIZATION_YEARMON)
# 



# Data pre-visualization
## Best log-returns stocks
pivot_wider_stocks_log_returns <- log_returns_estimation %>% 
  summarise(across(all_of(all_stocks),
                   sum)
            ) %>% 
  pivot_longer(all_of(all_stocks),
               names_to = "tickets",
               values_to = "log_return_sum"
               ) %>%
  arrange(log_return_sum)

best_log_return_stocks_estimation <- pivot_wider_stocks_log_returns %>%
  dplyr::filter(log_return_sum > 0.8) %>%
  select(tickets) %>%
  unique

# Correlation matrix visualization of total log-returns within estimation window
corrplot(log_returns_estimation %>% 
           select(all_of(best_log_return_stocks_estimation$tickets)) %>% 
           cor(use = "complete.obs"), 
         method = 'square', 
         order = 'alphabet', 
         type = 'lower', 
         diag = FALSE)

# ## Least (log-return) variance
least_variance_stocks_estimation <- log_returns_estimation %>%
  summarise(across(all_of(all_stocks),
                   var))

pivot_wider_mkt_variance <- least_variance_stocks_estimation %>%
  pivot_longer(all_of(all_stocks),
               names_to = "tickets",
               values_to = "variance"
  ) %>%
  arrange(variance)

best_variance_stocks_estimation <- pivot_wider_mkt_variance %>%
  slice(1:20) %>%
  select(tickets) %>%
  unique

# # Correlation matrix visualization of total log-returns within estimation window
corrplot(log_returns_estimation %>%
           select(all_of(best_variance_stocks_estimation$tickets)) %>%
           cor(use = "complete.obs"),
         method = 'square',
         order = 'alphabet',
         type = 'lower',
         diag = FALSE)





# Portfolio optimization calculations
chosen_stocks <- best_variance_stocks_estimation$tickets

log_return_optimization <- log_returns %>%
  dplyr::filter(ano_mes >=  FIRST_OPTIMIZATION_YEARMON &
                  ano_mes < LAST_OPTIMIZATION_YEARMON) %>% 
  select(all_of(chosen_stocks))


# Criação de janelas de tempo de $QTD_MESES_JANELA_MOVEL$
## Notar que primeiros $QTD_MESES_JANELA_MOVEL$ serão NULL devido ao .complete=TRUE
time_windows <-  slide_period(.x = log_returns,                # Pass the WHOLE dataframe here
                              .i = date(log_returns$ano_mes), # Use the dates for the calendar logic
                              .period = "month", 
                              .f = ~ .x %>% select(ano_mes),                       # This now returns a slice of the dataframe
                              .origin = INITIAL_ESTIMATION_DATE,
                              .before = SLIDING_WINDOW_MONTHS - 1, 
                              .complete = TRUE
                              )



# Data separation within adequate time windows
## Portfolio optimizations will be done with log-returns
## Generates an a priori optimized ptf (for `SLIDING_WINDOW_MONTHS` months) + `TOTAL_OPTIMIZATION_MONTHS` optimized a posteriori
### Keeps only [[time_windows]] for t \in [SLIDING_WINDOW_MONTHS, SLIDING_WINDOW_MONTHS+TOTAL_OPTIMIZATION_MONTHS],
### because the first (SLIDING_WINDOW_MONTHS-1) elements are NULL
risky_portfolios <- comprehenr::to_list(for (t in (SLIDING_WINDOW_MONTHS):(SLIDING_WINDOW_MONTHS+TOTAL_OPTIMIZATION_MONTHS) )
  log_returns %>% 
  dplyr::filter(ano_mes %in% time_windows[[t]]$ano_mes) %>% 
  select(all_of(chosen_stocks))
  )



# Fronteira eficiente dos dados de estimação
log_retornos_ts = as.timeSeries(log_returns_estimation %>%
                                              select(all_of(best_log_return_stocks_estimation$tickets))
                                                     )


specs_portfolio_eficiente <- portfolioSpec()
setNFrontierPoints(specs_portfolio_eficiente) <- 100

fronteira_eficiente <- portfolioFrontier(log_retornos_ts, specs_portfolio_eficiente)

# Efficient frontier plot
tailoredFrontierPlot(object= fronteira_eficiente,
  return = c("mean"),
  risk = c( "Sigma"),
  mText = "MV Portfolio – LongOnly Constraints",
  sharpeRatio = TRUE)




# Cálculos do portfolio de variância mínima -- USA LOG-RETORNOS!!!
## "LongOnly" requer pesos >= 0 (sem vendas a descoberto, i.e. pesos negativos)
## Cada otimização é feita visando o mês logo após:
### primeira estimação é feita com os meses inteiros 01/2018 - 12/2019, para alocar para 01/2020
### segunda estimação usa meses 02/2018 - 01/2020, para alocar para 02/2020, etc.
log_returns_risky_ptf <- c()
risky_ptf_variances <- c()


for (t in 1:(TOTAL_OPTIMIZATION_MONTHS)) {
  
  specs_portfolio_var_minima <- portfolioSpec()
  shortSpec <- portfolioSpec()
  setSolver(shortSpec) <- "solveRshortExact"
  
  # Otimizações de portfolio: 
  min_var_risky_portfolio <- minvariancePortfolio(data = as.timeSeries(risky_portfolios[[t]]),
                                                  spec = specs_portfolio_var_minima,
                                                  constraints = "LongOnly")

  # Cálculos de pesos óptimos  
  weights_risky_ptf <- getWeights(min_var_risky_portfolio)
  
  # # Debug
  # print(weights_risky_ptf_short,
  #       weights_risk_plus_free_ptf_short)
  # 
  
  # Cálculo de matrizes de covariância (somente diz respeito aos assets, não a Long/Short)
  risky_covariance_matrix <- risky_portfolios[[t]] %>% cov

  # Cálculo de retornos ponderados pelos pesos, obtidos no mês `t`
  log_return_risky_ptf <- weights_risky_ptf %*% t(log_return_optimization %>% slice(t))
  log_returns_risky_ptf <- c(log_returns_risky_ptf, log_return_risky_ptf)
  
  # Cálculo de variâncias dos portfolios
  risky_ptf_variance <- t(weights_risky_ptf) %*% risky_covariance_matrix %*% weights_risky_ptf
  risky_ptf_variances <- c(risky_ptf_variances, risky_ptf_variance)

  }


# Cálculos de desvios padrão
risky_ptf_stdevs <- risky_ptf_variances %>% sqrt

# Comparações com Ibovespa e IBrA
timeseries_ibovespa <- log_returns %>%
  dplyr::filter(ano_mes >=  FIRST_OPTIMIZATION_YEARMON &
                  ano_mes < LAST_OPTIMIZATION_YEARMON) %>% 
  select(IBOV) %>% 
  as.timeSeries


timeseries_ibra <- log_returns %>% 
  dplyr::filter(ano_mes >=  FIRST_OPTIMIZATION_YEARMON &
                  ano_mes < LAST_OPTIMIZATION_YEARMON) %>% 
  select(IBRA) %>%
  as.timeSeries

corr_risky_ibov <- cor(log_returns_risky_ptf,
                       timeseries_ibovespa)

corr_risky_ibra <- cor(log_returns_risky_ptf,
                       timeseries_ibra)



# Plot: retornos LÍQUIDOS (multiplicativos) obtidos
to_plot <- data.frame(
  x = log_returns %>% dplyr::filter(ano_mes >=  FIRST_OPTIMIZATION_YEARMON &
                    ano_mes < LAST_OPTIMIZATION_YEARMON) %>% 
    select(ano_mes), # Dataframe cuja única coluna se chama `ano_mes`
  risky = log_returns_risky_ptf %>% cumsum, #%>% exp %>% `-`(1),
  ibovespa = timeseries_ibovespa %>% cumsum, # %>% exp %>% `-`(1), # Tem coluna `IBOV`,
  ibra = timeseries_ibra %>% cumsum # %>% exp %>% `-`(1) # Tem coluna `IBRA`
  ) %>% 
  mutate(ano_mes = ano_mes %>% zoo::as.yearmon(format = "%Y_%m"))

to_plot_total <- tidyr::pivot_longer(
  to_plot,
  cols = c(risky, 
           IBOV
           # IBRA
           ),
  names_to = "series",
  values_to = "value"
)

# Gráfico comparando todos os portfolios
ggplot(to_plot_total, aes(x = ano_mes, 
                         y = value, 
                         color = series)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Accumulated monthly net returns (60 months)",
    subtitle ="In parenthesis: correlation with `Ibovespa` market index & accumulated net return",
    x = "Time",
    y = "Net returns",
    color = "Time series"
  ) +
  scale_color_manual(
    values = c(
      IBOV = "purple",
      # IBRA = "pink",
      risky = "darkorange"
      ),
    labels = c(paste0("Ibovespa (-, ",
                      round(sum(log_returns$IBOV), 2), #%>% exp %>% `-`(1), 2), 
                      ")"),
               # paste0("IBRA (", round(corr_risky_ibov, 2), ", ", 
               #        round(sum(log_returns_risky_ptf) %>% exp %>% `-`(1), 2), ")"),
               paste0("Portfolio (", round(corr_risky_ibov, 2), ", ", 
                      round(sum(log_returns_risky_ptf), 2), # %>% exp %>% `-`(1), 2), 
                      ")")
               )
) +
  theme_minimal()

# dev.off()


