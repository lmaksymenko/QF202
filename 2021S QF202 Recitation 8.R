library("forecast")
library("ggplot2")
library("quantmod")
# MA(q) model
data_generate <- arima.sim(list(ma = c(0.8, 0.6)), n = 5000, sd = sqrt(0.01))
data_generate

acf(data_generate)
pacf(data_generate)$acf

Box.test(data_generate, lag = 10, type = "Ljung-Box")

model = arima(data_generate, order=c(0,0,2), include.mean = F)
model
# Try different model, which one is better?:
arima(data_generate, order=c(0,0,3), include.mean = F)
arima(data_generate, order=c(0,0,1), include.mean = F)

# Check independence of residuals
tsdiag(model)          
Box.test(model$resid,lag=10,type='Ljung') 

# Predict
predict(model, n.ahead = 20)
forecast(model, 100)
plot(forecast(model, 100), xlim = c(4500, 5200))


# Use actual data

getSymbols("GOOG", from = "2010-01-01", to = "2019-12-31", src = "yahoo")
ret<- dailyReturn(GOOG$GOOG.Adjusted)
ret<- ret[2:length(ret)]
head(ret)

acf(ret)
pacf(ret)

max.q=20
model.aic=c()
for (q in 1:max.q){
  model.aic = append(model.aic, arima(ret,order=c(0,0,q))$aic)
}

model.aic

# [1] -13843.21 -13842.08 -13840.26 -13838.29 -13839.75 -13837.83 -13835.84 -13838.54
# [9] -13836.68 -13835.52 -13836.69 -13834.71 -13832.72 -13831.33 -13829.33 -13827.40
# [17] -13835.38 -13833.42 -13832.04 -13830.26

Box.test(ret, lag = 20)

