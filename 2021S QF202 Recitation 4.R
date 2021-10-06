library(fBasics)#nromality test methods
library(moments)#ways to calculate moments
library(quantmod)
#when you load library you load all the library functions into the global namespace of your workspace
#if functions have same names the function loaded last will overload the previous versions

###### Third and fourth moments ######

# We have covered the third moment (Skewness) and fourth moment (Kurtosis) before using the package "moments", "fBasics" and so on

skewness(rexp(10000, rate = 1)) #rexp -> random num gen for exonential dist
skewness(rt(10000, df = 5))

skewness(rnorm(10000))
kurtosis(rnorm(10000)) # These two are central moments  #rnorm -> random normal numbers

#be careful, some libraries return central moments and some return excess

# excess kurtosis
kurtosis(rnorm(10000))-3
# -3 because that is theoretical 3rd moment of the normal distribution, thus we subtract 3 to find the difference 


kurtosis(rt(10000, df = 10000))- 3 #as the df of the t dist goes to inf it approaches the normal dist (the tails get slimmer)

2*(1-pnorm(skewness(rexp(10000, rate = 1)), 0, sqrt(6/10000)))
#  ^1-the cdf of the area up the the tail = the area of the tail. The multiply by 2 to get area of both tails
#this returns 0, meaning that the chance that the numbers rexp(10000, rate =1) came from a normal dist is 0



####### Compare t distribution with normal distribution (fat tail/heavy tail) ######

curve(dt(x, df = 4), from = - 4, to = 4)
xx <- seq(-4, 4, 0.1)
yy <- dnorm(xx)
lines(xx, yy, col = "red")
lines(xx, dt(xx, df =50), col = "dark green")
title("Black: t distribution; Red: normal distribution")



####### Quantile-Quantile Plot(Q-Q plot) ###########

?qqnorm
?qqline

# Normal distribution

data = rnorm(10000, mean = 0, sd = 1)
qqnorm(data)
qqline(data, col = 2)



qqnorm(rnorm(10000, mean = 5, sd = 2))
qqline(rnorm(10000, mean = 5, sd = 2), col = 2)

# Exponential distribution

qqnorm(rexp(10000, rate = 1))
qqline(rexp(10000, rate = 1), col = 2)

# t-distribution

qqnorm(rt(10000, df = 1))
qqline(rt(10000, df = 1), col = 2)




qqnorm(rt(10000, df = 10))

qqnorm(rt(10000, df = 100)) # Notice that when the degree of freedom is large, t distribution is close to the normal distribution
qqline(rt(10000, df = 100), col = 2)

qqplot(rexp(1000, rate = 1), rexp(1000, rate = 5)) # provide 2 samples and it gives you a qqplot of 2 separate distributions
                                                  # if the line is straight then the distributions are the same

qqplot(rexp(1000, rate = 1), rchisq(10000, df=5))

###### Chi square distribution (like the continuous distribution before) ######

# mean value is k, variance is 2k, skewness is sqrt(8/k), and kurtosis is 12/k (k is the degree of freedom)
# More properties see https://en.wikipedia.org/wiki/Chi-squared_distribution

?dchisq # pdf of chi square distribution
?pchisq # cdf of chi square
?qchisq # quantile of chi square
?rchisq # random numbers of chi square

pchisq(14.06714, df=7)
qchisq(.95, df=7)
rchisq(10, df = 7)

# Different degrees of freedom

curve(dchisq(x, df = 1), from = 0, to = 20) # degree of freedom equals to 1
xxx <- seq(0, 20, 0.1)
yy1 <- dchisq(xxx, df = 3)
yy2 <- dchisq(xxx, df = 5)
lines(xxx, yy1, col = "red") # degree of freedom equals to 3
lines(xxx, yy2, col = "green") # degree of freedom equals to 5
title('DF: black-1; red-3; green-5')




######## Normality test ########
#to test if the dist you have is normal


?normalTest
normalTest(rnorm(10), method = "jb") # Jarque-Bera test (see https://en.wikipedia.org/wiki/Jarque%E2%80%93Bera_test)
normalTest(rnorm(100), method = "jb")
normalTest(rnorm(1000), method = "jb")
# according to the Wikipedia, the test statistic is n/6*(S^2 + 1/4 * (K-3)^2) [n is the number of observations]



normalTest(rexp(10, rate = 2), method = "jb")
normalTest(rexp(100, rate = 2), method = "jb")
#small p value means we can reject the null hypothesis (not coming from normal dist)

normalTest(rexp(10, rate = 2), method = "sw")
normalTest(rexp(100, rate = 2), method = "sw")
# Shapiro-Wilk test (see https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test)
# Null hypothesis of these tests is the sample is normally distributed. (Of course the alternative hypothesis is the opposite)

normalTest(rexp(10, rate = 2), method = "ks")
normalTest(rexp(100, rate = 2), method = "ks")

normalTest(rt(100, df=2), method = "ks")
normalTest(rt(100, df=10), method = "ks")

# Kolmogorov-Smirnov test: https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test
#calculate distance between throretical normal cdf and the empirical cdf of the data you have 
normalTest(rt(100, df=2), method = "da")
normalTest(rt(100, df=10), method = "da")


# Also exists Kolmogorov-Smirnov normality test (ks), and D'Agostino normality test (da). 



name_list <- c("GOOG",  "AAPL", "AMZN")
getSymbols(name_list, from = "2018-01-01", to = "2018-12-31", src = "yahoo")

# Simple return

aapl_close = as.vector(AAPL$AAPL.Close)
aapl_ret = diff(aapl_close) / aapl_close[-length(aapl_close)]

#(1+R_1)(1+R_2_...(1+R_n)-1

# One year total return
# 1 

end_price = as.numeric(AAPL$AAPL.Close[length(AAPL$AAPL.Close)])
start_price = as.numeric(AAPL$AAPL.Close[1])
end_price
start_price

(end_price - start_price)/start_price

# 2
aapl_ret+1
prod(as.vector(aapl_ret+1))-1

###for next lecture
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
weight = c(aapl_close[1], 2*goog_close[1], 3*amzn_close[1])/start_value
returns = c(aapl_total_return, goog_total_return, amzn_total_return)

total_return_2 = sum(weight*returns)
total_return_2







