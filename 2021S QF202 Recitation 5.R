library(quantmod)

name_list <- c("GOOG",  "AAPL", "AMZN")
getSymbols(name_list, from = "2018-01-01", to = "2018-12-31", src = "yahoo")

# Simple return

aapl_close = as.vector(AAPL$AAPL.Close)
aapl_ret = diff(aapl_close) / aapl_close[-length(aapl_close)]

#(1+R_1)(1+R_2)...(1+R_n)-1 # formula to calcualate total retruns using simple returns

# One year simple total return
# 1 (use price at start and end of year)

end_price = as.numeric(AAPL$AAPL.Close[length(AAPL$AAPL.Close)])
start_price = as.numeric(AAPL$AAPL.Close[1])
end_price
start_price

(end_price - start_price)/start_price

# 2 (using the above equation)
aapl_ret+1
prod(as.vector(aapl_ret+1))-1


# Simple return 
goog_close = as.vector(GOOG$GOOG.Close)
amzn_close = as.vector(AMZN$AMZN.Close)

goog_ret = diff(goog_close) / goog_close[-length(goog_close)]
amzn_ret = diff(amzn_close) / amzn_close[-length(amzn_close)]

n = length(aapl_close)

aapl_total_return = prod(as.vector(aapl_ret+1))-1
goog_total_return = prod(as.vector(goog_ret+1))-1
amzn_total_return = prod(as.vector(amzn_ret+1))-1

aapl_total_return
goog_total_return
amzn_total_return
  
# Assume we have a portfolio which consists of 1 share AAPL, 2 share GOOG, 3 share AMZN
start_value = aapl_close[1]+2*goog_close[1]+3*amzn_close[1]
start_value

end_value = aapl_close[n]+2*goog_close[n]+3*amzn_close[n]
end_value

total_return_1 = (end_value - start_value)/start_value
total_return_1


# Let's take a look at the return
weight = c(aapl_close[1], 2*goog_close[1], 3*amzn_close[1])/start_value #calculate the initial weight of the portfolio holdings
returns = c(aapl_total_return, goog_total_return, amzn_total_return)

total_return_2 = sum(weight*returns) #summing the weighted returns
total_return_2


###### Autocorrelation function (ACF) ######

getSymbols(Symbols = "GOOG", from = "2010-01-01", to = "2019-12-31")
Return_GOOG <- dailyReturn(GOOG)
Return_GOOG_22 <- monthlyReturn(GOOG)

acf(Return_GOOG) # Notice the plot is to show the correlation coefficients(bar height) so the value is always between -1 and 1
acf(Return_GOOG_22)#dotted lines show the significance thershold

acf(Return_GOOG_22, plot = FALSE) # show the autocorrelations in console instead of plotting them.

#autocorrelation
#X_1, X_2, X_3
#X_1, X_2, X_3

#lag 1 autocorrelation
#X_1, X_2, X_3
#X_2, X_3, X_4


###### Box test ######


?Box.test()
data = rnorm(10000)#gaussian white noise 
Box.test(data, lag = 25, type = "Box-Pierce")
Box.test(data, lag = 25, type = "Ljung-Box")#universally better 
#null: correlation coef for rh0 1 to l are all zero (large p, cannot reject -> are independent) we test the independence of the time series 
#alt: 
acf(data)


###### AR model ######

?ar()
#AR(1)
#X_t = c a*X_{t-1} + e_t           

                            #check first 35 orders and find the best
ar(rnorm(10000), aic = TRUE, order.max = 35, method = "yule-walker") # default method is "yule-walker"
# If aic argument is FALSE, then it will automatically use the max order you provide

# The methods includes different types "yule-walker" (or "yw"), 
# "ols" (short for ordinary least square), 
# "mle" (short for maximum likelihood estimation)
# Notice that different methods may give your different orders and coefficients

?arima.sim
#X_t = 0.5*X_{t-1} + e_t    
#X_{t-1} = 0.5*X_{t-2} + e_{t-1}   
data_generate <- arima.sim(list(ar = c(0.5)), n = 5000, sd = sqrt(0.01))
data_generate

acf(data_generate)

Box.test(data_generate, lag = 2, type = "Ljung-Box")

ar(data_generate, method = "yule-walker")

###### Example ######

# S&P 500

getSymbols("^GSPC", from = "2010-01-01", to = "2019-12-31")
SP500_return <- dailyReturn(GSPC, type = "log")

Box.test(SP500_return, lag = 25, type = "Ljung-Box")#fitting the box test, and the p val is small,

acf(SP500_return)







