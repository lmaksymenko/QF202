# Problem: X_1, X_2, ..., X_100 iid follow N(mu, sigma^2), where sigma = 2 is known. 
# Now we need to estimate parameter mu

# The true mu is 1 but let's assume we don't know it.

data <- rnorm(100, 1, 2)

# Sample mean estimate:
mean(data)

# Sample sd:
sd(data)

# Let's focus on sample mean. Write the experiment into a function, 
# so that we can repeat it by simply calling the function
simple.exp <- function()
{
  data <- rnorm(100, 1, 2)
  mean(data)
}

simple.exp()
simple.exp()
simple.exp()


# Given the hypothesis that iid sample comes from N(1, 2^2), sample mean, as a r.v., 
# follows N(1, 2^2/n) (std=2/sqrt(n)). 
# Recall that for a normal distribution, roughly 95% density concentrates 
# within interval (mu-1.959964*sigma, mu+1.959964*sigma). 
# In fact, for standard normal distribution
# the 0.025 and 0.975 quantiles are the following

qnorm(0.975)
qnorm(0.025)

# For our distribution with sigma=2, 95% of population falls into (mu-1.959964*2, mu+1.959964*2)
# Note that mu is the true mean but unknown. 
# 95% of all sample means will be within roughly 1.96 standard deviations (1.96*2/√n).
# Because distances are symmetrical, this implies that 
# the population parameter must be within roughly 1.96 standard deviations from the sample average, in 95% of all samples.

# Let's confirm both ways
# 1
sd <- 2/sqrt(100)
c(1-1.959964*sd, 1+1.959964*sd)
sample.mean.1000 <- replicate(1000, simple.exp())
sum(sample.mean.1000>1-1.959964*sd & sample.mean.1000 < 1+1.959964*sd)/1000

# 2
cbind(sample.mean.1000-1.959964*sd, sample.mean.1000+1.959964*sd)

lower.bounds <- sample.mean.1000-1.959964*sd
upper.bounds <- sample.mean.1000+1.959964*sd
confidence.intervals <- cbind(lower.bounds<1, 1<upper.bounds)
confidence.intervals

lower.bounds<1 & 1<upper.bounds

sum(lower.bounds<1 & 1<upper.bounds)/1000

# Therefore, for each experiment, we will have an interval estimate for mu at 95% confidence level
# 95% chance that this interval will capture the true mean.
exper <- function(c)
{
  n <- 100
  data <- rnorm(n, 1, 2)
  sample.mean <- mean(data)
  c(sample.mean-qnorm(c)*2/sqrt(n), sample.mean+qnorm(c)*2/sqrt(n))
}
exper(0.95)


#### PART TWO ######
###### Download stock prices and calculate returns ######

# install.packages("quantmod")

library(quantmod)

name_list <- c("^GSPC",  "AAPL")
getSymbols(name_list, from = "2010-01-01", to = "2019-12-31", src = "yahoo")

# Simple return

diff(AAPL$AAPL.Adjusted)
diff(AAPL$AAPL.Adjusted) / AAPL$AAPL.Adjusted[-length(AAPL$AAPL.Adjusted)]

# Log return

diff(log(AAPL$AAPL.Adjusted))

# Weekly, Monthly, Quarterly and Annual return (type of returns: arithmetic (discrete) or log (continuous))

?periodReturn
dailyReturn(AAPL, type = "arithmetic")
weeklyReturn(AAPL, type = "log")
monthlyReturn(AAPL)
quarterlyReturn(AAPL)
annualReturn(AAPL)

AAPL_log_daily_rtn <- dailyReturn(AAPL, type = "log")
SP500_log_daily_rtn <- dailyReturn(GSPC, type = "log")

# Calculate return moments

# install.packages("e1071", "moments", "fBasics", "PerformanceAnalytics") 
# Either of these packages has the command to calculate skewness and kurtosis

library(e1071)
library(moments)
library(fBasics) # loading this package will automatically load the 'timeDate' package
library(PerformanceAnalytics)

detach("package:e1071", unload=TRUE)
detach("package:moments", unload=TRUE)
detach("package:fBasics", unload=TRUE)
detach("package:PerformanceAnalytics", unload=TRUE)

mean(AAPL_log_daily_rtn)
var(AAPL_log_daily_rtn)
skewness(AAPL_log_daily_rtn)
kurtosis(AAPL_log_daily_rtn)

###### Test mean return equal to 0 and calculate confidence interval ######

?t.test
t.test(as.vector(AAPL_log_daily_rtn), alternative = "two.sided" , conf.level = 0.95)


###### Linear Regression between two stocks ######

plot(AAPL_log_daily_rtn)
plot(SP500_log_daily_rtn)


plot(as.vector(SP500_log_daily_rtn),as.vector(AAPL_log_daily_rtn) )

regression.relation=lm(SP500_log_daily_rtn~AAPL_log_daily_rtn)
summary(regression.relation) # Details of the linear regression
plot(regression.relation)

