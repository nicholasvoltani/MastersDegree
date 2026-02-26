library(dplyr)
library(tidyr)
library(stringr)
library(quantmod)


# apple_df <- getSymbols('AAPL', src='yahoo', auto.assign=FALSE)
# 
# chartSeries(apple_df, name="AAPL", subset="last 6 months", theme=chartTheme("white"))
# 
# gold_df <- getSymbols(c('GC=F', 'SI=F'), src='yahoo', auto.assign=TRUE)
# 
# chartSeries(c(`GC=F`, `SI=F`), name="Gold", subset="last 6 months", theme=chartTheme("white"))


getSymbols(c('APLD', 'CRWV', 'NBIS', 'ORCL'), src='yahoo', auto.assign=TRUE)

chartSeries(c(APLD, CRWV, NBIS, ORCL), 
            name="Cloud GPU Computing", subset="last 6 months", theme=chartTheme("white"))

for (ticker in c('APLD', 'CRWV', 'NBIS', 'ORCL')){
  df <- getSymbols(ticker, src='yahoo', auto.assign=FALSE)
  chartSeries(df, name=str_interp("${ticker}"), subset="last 12 months", theme=chartTheme("white"))
}

