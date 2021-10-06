library(quantmod)


getSymbols(c("KO"), from ="2015-01-01", to="2019-12-31")

prices = KO$KO.Adjusted
returns = dailyReturn(KO$KO.Adjusted, type = "log")

library(tseries)

# we firstly take a look at the price sequence
adf.test(prices) # a high p value indicates the existance of unit root.

acf(prices) 
pacf(prices)

# Now we check log(prices) because we are going to compare it with
# its differenced term - returns
adf.test(log(prices))

acf(log(prices))
pacf(log(prices))

# We further check the return sequence.
adf.test(returns) # small p-value means rejecting the Null hypothesis (unit root exists)

acf(returns)
pacf(returns)

# Another case study:
#US GDP table from the bureau of economic analysis
#http://www.bea.gov//national/nipaweb/DownSS2.asp
#The excel NIPA table Section1All.xls contains all tables in section 1
# I extracted the GDP annual from table 10105

library(tseries)
library(timeSeries)
gdp=read.csv("NIPAannualGDP.csv",header=T)
gdp[1,]

gdp=read.csv("NIPAannualGDP.csv",skip=7, header=F)
gdp[1,]
gdp.annual=gdp[2,4:length(gdp[2,])]
gdp.annual
gdp.annual=t(gdp.annual)
gdp.annual

### let us keep the year too

gdp.annual=gdp[1:2,4:length(gdp[2,])]
gdp.annual
gdp.annual=t(gdp.annual)
gdp.annual

#they are characters now. Need to make them numbers
data.frame(year=as.numeric(gdp.annual[,1]),GDP=as.numeric(paste(gdp.annual[,2])))

#We have an issue with the numbers which are read in an accounting format
#using function gsub
?gsub
gsub(",","",gdp.annual[,2])
as.numeric(gsub(",","",gdp.annual[,2]))

gdp.annual=data.frame(year=as.numeric(gdp.annual[,1]),GDP=as.numeric(gsub(",","",gdp.annual[,2])))


plot(gdp.annual[,1], gdp.annual[,2],type="l")
gdp.return=returns(gdp.annual[,2])[-1]
plot(gdp.annual[-1,1], gdp.return,type="l")
### Annual GDP:

ar(gdp.annual[,2])
adf.test(gdp.annual[,2],k = 1)

adf.test(log(gdp.annual[,2]),lags=5)

adf.test(gdp.return,lags=5)  ## We assume AR(5)

# Title:
#   Augmented Dickey-Fuller Test
# 
# Test Results:
#   PARAMETER:
#   Lag Order: 5
# STATISTIC:
#   Dickey-Fuller: -1.3141
# P VALUE:
#   0.1933  ## Seems that the null hypothesis of existence of a unit root cannot be rejected



### LOOK AT: adfTest(log(gdp.annual[,2]),lags=5) ## What does this do and what does the result mean?


## A better analysis (more caferful).

ord=ar(gdp.return)

ord

# > ord
# 
# Call:
#   ar(x = gdp.return)
# 
# Coefficients:
#   1  
# 0.6784  
# 
# Order selected 1  sigma^2 estimated as  0.0004598            ## Seems that an order 1 is estimated

adf.test(gdp.return,lags=2)            ## same as we did before

## What about the difference series

ar(diff(gdp.return))    ##  2

adf.test(diff(gdp.return),lags=3)    ## This gives a p-value much smaller that means that the unit root is rejected for the difference series. Means that the I in ARIMA is only 1

ar(diff(diff(gdp.return)))    ##  5

adf.test(diff(gdp.return),lags=6)

##NOW we go back to the ARMA type fitting

acf(gdp.annual[,2])
acf(gdp.return)
acf(diff(gdp.return))

## Pure unit root model:
d = cumsum(rnorm(2000,0,sd=0.01))
acf(d)
adf.test(d)

acf(diff(d))
adf.test(diff(d))



