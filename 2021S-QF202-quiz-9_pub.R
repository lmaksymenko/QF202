
load("quiz-9-data.RData")
data_generate # here is the data

library(quantmod)
library(fUnitRoots)
library(forecast)
library(lubridate)
library(TSA)
library(pracma)


acf(data_generate)
pacf(data_generate)


model3 = arima(data_generate, order=c(5,0,0), include.mean = F)
model3

plot(model3$resid,type='l')                
Box.test(model3$resid,lag=10,type='Ljung') 

aic.matrix = function(data, ar_order, ma_order)
{
  AIC_matrix <- matrix(NA, nrow = ar_order+1, ncol = ma_order+1)
  for(i in 0 : ar_order)
  {
    for(j in 0 : ma_order)
    {
      tem <- tryCatch(arima(data, order = c(i, 0, j))$aic, 
                      error = function(cond)
                      {
                        message(cond)
                        message(". AR: ", i, "; MA: ",j)
                        return (NA)
                      }
      )
      
      AIC_matrix[i+1, j+1] <- tem
    }
  }
  AIC_matrix
}


matrix = aic.matrix(data_generate, 3, 3)
which(matrix == min(na.omit(matrix)), arr.ind = TRUE) -1 #will find the best fit ARMA model AR = row MA = col
matrix


auto.arima(data_generate)
