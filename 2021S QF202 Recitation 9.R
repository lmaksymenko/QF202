library(quantmod)
library(fUnitRoots)
library(forecast)
library(lubridate)
library(TSA)
library(pracma)



data_generate <- arima.sim(list(ar = c(0.5, -0.3, -0.2, 0.2), 
                                ma=c(0.6, -0.5, 0.5)), 
                           n = 5000, 
                           sd = sqrt(0.01))
data_generate

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


matrix = aic.matrix(data_generate, 10, 10)
which(matrix == min(na.omit(matrix)), arr.ind = TRUE) -1

eacf(data_generate, 10, 10) 


# > which(matrix == min(na.omit(matrix)), arr.ind = TRUE) -1
#       row col
# [1,]  10   4
# > matrix
#       [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]      [,8]      [,9]     [,10]     [,11]
# [1,] -4344.904 -5944.397 -5966.380 -6074.164 -6080.919 -6211.028 -6259.333 -6258.314 -6284.367 -6285.243 -6284.587
# [2,] -5077.265 -5955.527 -6013.148 -6074.130 -6105.060 -6239.127 -6257.682 -6268.860 -6284.156 -6283.765 -6282.943
# [3,] -6026.472 -6146.162 -6144.233 -6278.507 -6277.648 -6296.581 -6295.063 -6302.148 -6301.893 -6299.903 -6299.485
# [4,] -6165.371 -6213.772 -6212.079 -6278.429 -6278.608 -6297.018 -6295.541 -6301.508 -6299.880 -6297.934 -6297.462
# [5,] -6181.130 -6211.874 -6256.648 *-6301.90* -6300.024 -6298.369 -6296.626 -6300.506 -6297.527 -6297.702 -6296.389
# [6,] -6279.439 -6288.802 -6289.515 -6299.996 -6297.920 -6296.463 -6294.465 -6303.807 -6296.832 -6300.035 -6297.064
# [7,] -6287.028 -6293.661 -6292.001 -6298.809 -6297.314 -6294.619 -6292.750 -6301.436 -6299.816 -6298.199 -6296.251
# [8,] -6285.036 -6292.313 -6295.787 -6296.446 -6294.223 -6292.528 -6290.177 -6298.362 -6297.888 -6295.996 -6295.115
# [9,] -6298.352 -6297.935 -6297.483 -6295.489 -6293.459 -6291.756 -6298.230 -6297.417 -6295.665 -6300.078 -6294.207
# [10,] -6297.216 -6297.829 -6295.555 -6293.613 -6304.260 -6297.230 -6299.869 -6296.076 -6294.197 -6293.922 -6291.491
# [11,] -6295.735 -6295.677 -6293.874 -6291.871 *-6305.552* -6297.836 -6295.660 -6297.402 -6292.246 -6291.117 -6290.504
# > eacf(data_generate, 10, 10) 
# AR/MA
#    0 1 2 3 4 5 6 7 8 9 10
# 0  x x x x x x x x o x o 
# 1  x x x x x x x x o x o 
# 2  x x x x x o o o o x o 
# 3  x x x x x o o o o o o 
# 4  x x x x o o x o o o o 
# 5  x x x o o o o x o o o 
# 6  x x x o o o o o o o o 
# 7  o x x x o o o o x o o 
# 8  x x x x x o x o o o o 
# 9  x o o x x o x o o x o 
# 10 x o x x x o x x x o x 

which(Reshape(rank(na.omit(matrix))<8, 11)==T, arr.ind = T)
#       row col
# [1,]   5   4
# [2,]  10   5
# [3,]  11   5
# [4,]   3   8
# [5,]   4   8
# [6,]   6   8
# [7,]   3   9

model <- arima(data_generate, order = c(10, 0, 4))
summary(model)

tsdisplay(model$residuals)
tsdiag(model)

# Call:
#   arima(x = data_generate, order = c(10, 0, 4))
# 
# Coefficients:
#       ar1      ar2     ar3     ar4      ar5     ar6     ar7      ar8      ar9     ar10      ma1     ma2     ma3      ma4
#       0.6056  -1.0348  0.2731  0.2617  -0.0806  0.1612  0.0356  -0.0443  -0.0442  -0.0250  -0.0235  0.5268  0.2629  -0.4279
# s.e.  0.3090   0.1629  0.3199  0.2971   0.1555  0.1039  0.0880   0.0435   0.0453   0.0254   0.3090  0.2545  0.2581   0.3061
# intercept
# -0.0052
# s.e.     0.0027
# 
# sigma^2 estimated as 0.01649:  log likelihood = 3167.78,  aic = -6305.55





model2 <- arima(data_generate, order = c(4, 0, 3))
summary(model2)

# Call:
#   arima(x = data_generate, order = c(4, 0, 3))
# 
# Coefficients:
#       ar1      ar2      ar3     ar4     ma1      ma2     ma3  intercept
#       0.5331  -0.3264  -0.1915  0.2050  0.0500  -0.1422  0.3012    -0.0051
# s.e.  0.0749   0.0871   0.0515  0.0332  0.0745   0.0718  0.0356     0.0028
# 
# sigma^2 estimated as 0.01655:  log likelihood = 3158.95,  aic = -6301.9

tsdisplay(model2$residuals)
tsdiag(model2)






# Consumer Price Index(CPI-U) for all Urban Consumers
# CPI from 1913-01 to 2018-03
getSymbols('CPIAUCNS',src='FRED')

plot(CPIAUCNS, main="Consumer Price Index", type="l")



idx = as.Date(rownames(as.data.frame(CPIAUCNS))) > as.Date("1996-01-01") & as.Date(rownames(as.data.frame(CPIAUCNS))) < as.Date("2016-12-31")
CPI_tr <- CPIAUCNS[idx]

Rt_CPI <- diff(log(CPI_tr))[-1]
adfTest(Rt_CPI)

tsdisplay(Rt_CPI)

# Brute force method

AIC_matrix <- aic.matrix(Rt_CPI, 10, 10)


which(Reshape(rank(na.omit(matrix))<8, 11)==T, arr.ind = T) -1 # give you the smallest 8 fits.
which(AIC_matrix == min(na.omit(AIC_matrix)), arr.ind = TRUE) - 1 # give you the smallest AIC fit ARMA(8,5)



eacf(Rt_CPI, 24, 24) # ARMA(8,7)

model.cpi <- arima(Rt_CPI, order = c(8, 0, 5))
summary(model.cpi)

tsdisplay(model.cpi$residuals)
tsdiag(model.cpi)




