library(quantmod)
library(tseries)
getSymbols(c("MSFT"), from ="2015-01-01", to="2020-01-01")

returns = dailyReturn(MSFT$MSFT.Adjusted, type = "log")
ar(returns)

adf.test(returns, k=8)
