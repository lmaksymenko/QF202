data_gen <- arima.sim(list(ar = c(0.8)), n = 100000, sd = sqrt(0.25), mean = 0)+0.26
acf(data_gen, lag.max = 50, plot = FALSE)

library(quantmod)
getSymbols(c('SPY','AAPL','MSFT','AMZN','FB'), from = "2015-01-01", to = "2020-12-31", src = "yahoo")

SPY_log<- dailyReturn(SPY, type = "log")
AAPL_log<- dailyReturn(AAPL, type = "log")
MSFT_log<- dailyReturn(MSFT, type = "log")
AMZN_log<- dailyReturn(AMZN, type = "log")
FB_log <- dailyReturn(FB, type = "log")

summary(SPY_log)
summary(AAPL_log)
summary(MSFT_log)
summary(AMZN_log)
summary(FB_log)

#c)
FB_log_bin <- as.numeric(FB_log > 0)

#regression.relation = lm(FB_log_bin~MSFT_log)

mean(MSFT_log*FB_log_bin)
#0.004161572

#d)
#lm(response ~ explanitory)
reg = lm(AAPL_log~SPY_log)
summary(reg)
plot(as.vector(AAPL_log),as.vector(SPY_log))
plot(reg)

t.test(as.vector(AAPL_log),as.vector(SPY_log), 
                  alternative = "two.sided" , conf.level = 0.95)
#95% confidence  -0.0004634244  0.0017612484

#with a p val of 0.25, there is a significat slope thus no significant explanitory relation

#e)
acf(SPY_log)
pacf(SPY_log)

#the correlations are significant to lag 10

#f
SPY_arima <- arima(SPY_log, order = c(10,0,0))
SPY_arima #coeficients, order is 10



SPY_ar <-arima(SPY_log, method = "ML")
SPY_ar

#g
res <- SPY_arima$resid
library(fBasics)
normalTest(res, method = "jb")
qqnorm(res)
qqline(res, col = 2)
#our p value is very very small, out residuals are not N(0,1)

#bonus 2
Box.test(res, type = "Ljung-Box")
#large p value, residuals are independent



mean(rnorm(100000, mean =0, sd = 0.25))
