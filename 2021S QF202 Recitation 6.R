#AR models

# Use historical data
sp500daily=read.csv("GSPCdailyJan162017.csv",header=T)        ## these is a file I downloaded from yahoo finance
MSFT.day.data=read.csv("TestMSFT20051111.csv",header=T)            ## First file is daily data, secon file is minute data from some random day


sp500daily$Date             ## note that the first file contains data in inverse chronological order

sp500.price= sp500daily$Adj.Close[length(sp500daily$Adj.Close):1]      ## this puts the data in proper order (first line is the oldest)
sp500.date= sp500daily$Date[length(sp500daily$Adj.Close):1]

sp500.dayreturn=diff(sp500.price)/ sp500.price[-length(sp500.price)] ## daily simple return

sp500.logreturn=diff(log(sp500.price))  ## Cont compounded return

library(fBasics) ## This loads the package fBasic


## Linear time series models

acf(sp500.dayreturn,lag=15) # Obtain the ACF plot
s1=acf(sp500.dayreturn,lag=15) # Obtain the ACF plot and more.
names(s1)
s1$acf

s2=pacf(sp500.dayreturn,lag=15) # also called conditional autocorrelation
names(s2)
s2$acf
s2=pacf(sp500.dayreturn,lag=25)

Box.test(sp500.dayreturn,lag=25) #null is that all the values 1-25 are all zero

Box.test(sp500.dayreturn,lag=2,type="Ljung")

length(sp500.dayreturn)

par(mfcol=c(2,2)) # put 4 plots on one page
plot(sp500.dayreturn,type='l') # first plot
plot(sp500.dayreturn[1:(length(sp500.dayreturn)-1)],sp500.dayreturn[2:length(sp500.dayreturn)]) # lag 1 plot
plot(sp500.dayreturn[1:(length(sp500.dayreturn)-2)],sp500.dayreturn[3:length(sp500.dayreturn)]) # lag 2 plot  
acf(sp500.dayreturn,lag=15)
par(mfcol=c(1,1))

model1=ar( sp500.dayreturn ,method="mle") # Automatic AR fitting using AIC criterion.
model1             ## AR(5) is specified
names(model1)
tsdiag(model1) ## diagnostic plots - does not work for the automatic fitting function
plot(model1$resid,type='l')                ## checks residuals
Box.test(model1$resid,lag=10,type='Ljung') #if the model is good you are supposed to get a high p value, that means we reject the null (that residuals are not white noise)

## Other fitting methods:
model1.yule=ar( sp500.logreturn ,method="yule-walker") # Automatic AR fitting using AIC criterion and Yule Walker.
model1.yule$order
model1.yule
model1.ols=ar( sp500.logreturn ,method="ols") # Automatic AR fitting using AIC criterion and Ordinary Least Squares.
model1.ols$order
model1.ols
model1.burg=ar( sp500.logreturn ,method="burg") # Automatic AR fitting using AIC criterion and Levinson-Durbin recursion.
model1.burg$order
model1.burg



  ## Use generated data ##
# Repeat the above procedure for a generated dataset.
# Recall the stationary condition
data_generate <- arima.sim(list(ar = c(0.5, 0.3)), n = 5000, sd = sqrt(0.01))#generate a sequence for a arima model #this is arima 2 model
data_generate #                       2 len vector (means AR[2] model)



acf(data_generate)
pacf(data_generate)$acf # theoretically if you apply pacf to an AR(2) model, all values after the second should be zero
                        #if you apply pacf and observe the cuttoff corresponding to type of ar model then you can condlude that the data is a an ar model

Box.test(data_generate, lag = 1, type = "Ljung-Box")

par(mfcol=c(2,2)) # put 4 plots on one page
plot(data_generate,type='l') # first plot
plot(data_generate[1:(length(data_generate)-1)],data_generate[2:length(data_generate)]) # lag 1 plot
plot(data_generate[1:(length(data_generate)-2)],data_generate[3:length(data_generate)]) # lag 2 plot
acf(data_generate,lag=15)
par(mfcol=c(1,1))

# Check residual
model3 = ar(data_generate, method = "yule-walker")
plot(model3$resid,type='l')                
Box.test(model3$resid,lag=10,type='Ljung') 

# In class practice (10 min)
# Apply acf(), pacf(), box.test() to the following sequence

data_generate <- arima.sim(list(ar = c(0., 0.5)), n = 5000, sd = sqrt(0.01))
data_generate#                         ^this zero makes the correlation jump for evens (sum of coefficients has to be less than 1)

acf(data_generate)
pacf(data_generate)$acf #strong correlation for order 2 
Box.test(data_generate, lag = 1, type = "Ljung-Box")
