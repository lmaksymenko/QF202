#install.packages("forecast")

library("forecast")
library("ggplot2")

# AR model:
# Example from the last lecture

data_generate <- arima.sim(list(ar = c(0.5, 0.3)), n = 5000, sd = sqrt(0.01))
data_generate

acf(data_generate)
pacf(data_generate)$acf

Box.test(data_generate, lag = 2, type = "Ljung-Box")

model3 = arima(data_generate, order=c(2,0,0), include.mean = F)
model3
plot(model3$resid,type='l')                
Box.test(model3$resid,lag=10,type='Ljung') 

# Predict

predict(model3, n.ahead = 20)
forecast(model3, 100)
plot(forecast(model3, 100), xlim = c(4500, 5200))

# more ar models

## MA models
epsilon=rnorm(500,sd=1)

Delta.y=diff(epsilon) # r_t = a_t-a_{t-1}

plot(Delta.y, type = 'l')
## for more details about the labels type ?plotmath

acf(Delta.y)

# r_{t-1} = a_{t-1}-a_{t-2}
# => Cov[r_t, r_{t-1}] = E[r_t*r_{t-1}] = -E[a_{t-1}^2]= -1  (why?)
# => Var[r_t]=Var[r_{t-1}]=2
# => Corr[r_t, r_{t-1}]=-1/2

pacf(Delta.y)

?arima
m1=arima(Delta.y,order=c(0,0,1)) #and MA10 model would be 0,0,10
m1        

#Call:
#arima(x = Delta.y, order = c(0, 0, 1))

#Coefficients:
#          ma1  intercept
#      -0.5265     0.0148             ## The model is Delta.y(t) =  0.0148+a(t)-0.5265a(t-1).
#s.e.   0.0391     0.0290

m2=arima(Delta.y,order=c(0,0,1),include.mean=F)
m2 

tsdiag(m2)
predict(m2,5)
forecast(m2, 100)
plot(forecast(m2, 100), xlim =c(400, 600))

# E[r_{t+1}|F_t] = E[a_{t+1}-a_{t}|F_t]=-a_t
# E[r_{t+2}|F_t] = E[a_{t+2}-a_{t+1}|F_t]=0

# Excercise (20 mins): generate data from the following model
### r_t - 3 = a_t - 0.8*a_{t-1}
  ### r_t = a_t - 0.8*a_{t-1} + 3
### a_t~N(0,1) for all t
# Then apply acf(), pacf(), arima(), tsdiag() and predict() for 10 steps

a_t = rnorm(500, mean = 0,sd=1)


###ANSWER###
n=10
epsilon = rnorm(n, sd = 1) #   \/ exclude first element
rt = epsilon[-n]-0.8*epsilon[-1]+3
#            ^exclude last element

acf(rt)

pacf(rt)

m4 = arima(rt, order = c(0,0,1))
m4
tsdiag(m4) #tells us info for residuals
predict(m4, n.ahead = 10)

