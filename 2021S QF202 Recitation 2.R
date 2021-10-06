######  Create correlated random variable ######

x <- rnorm(10000)
y <- rnorm(10000)
rho <- 0.3

# Test correlation (both close to 0)

cov(x, y)
cor(x, y)

# Design correlated random variable

z <- rho * x + sqrt(1 - rho ^ 2) * y

cov(x, z)
cor(x, z)
#these numbers are the same because they follow the same normal dist

###### 3-d plot for joint distribution ######

#install.packages("LaplacesDemon")
#install.packages("plot3D")
#install.packages("rgl")

library(LaplacesDemon)
library(plot3D)
library(rgl)

# Level set graph

joint.density.plot(x, y, Title=NULL, contour=TRUE, color=FALSE, Trace=NULL)

# 3-dimensional histogram 

##  Create cuts:
x_c <- cut(x, 40)
y_c <- cut(y, 40)

x_c

##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)

##  Plot as a 3D histogram:
hist3D(z=z, border="black")

# probability density function

x_pdf <- seq(-5, 5, length=100) # set boundary for the axis
y_pdf <- seq(-5, 5, length=100)

z_pdf <- outer(x_pdf, y_pdf, function(x,y) dnorm(x,0,1)*dnorm(y,0,1))


persp3d(x_pdf, y_pdf, z_pdf, col = rainbow(100))

# probability density function for dependent r.v.s
install.packages("mvtnorm")
library(mvtnorm)

x_pdf <- seq(-5, 5, length=100) # set boundary for the axis
y_pdf <- seq(-5, 5, length=100)



z_pdf <- outer(x_pdf, y_pdf, function(x,y) dmvnorm(cbind(x, y), mean = c(0,0), sigma = cbind(c(1, 0.2), c(0.2, 1))))

persp3d(x_pdf, y_pdf, z_pdf, col = rainbow(100))

create.3d <- function(rho)
{
  z_pdf <- outer(x_pdf, y_pdf, function(x,y) dmvnorm(cbind(x, y), mean = c(0,0), sigma = cbind(c(1, rho*1*1), c(rho*1*1, 1))))
  
  persp3d(x_pdf, y_pdf, z_pdf, col = rainbow(100))
}

create.3d(0.0)

create.3d(0.3)

create.3d(0.6)

create.3d(0.9)

######  Create correlated random variable ######

x <- rnorm(10000)
y <- rnorm(10000)
rho <- 0.3

# Test correlation (both close to 0)

cov(x, y)
cor(x, y)

# Design correlated random variable

z <- rho * x + sqrt(1 - rho ^ 2) * y

cov(x, z)
cor(x, z)


###### Conditional probability & Conditional expectation ######

pair_xy <- cbind(x, y)
head(pair_xy)
class(pair_xy)

is.matrix(pair_xy)

which(x >= 0)
y[which(x >= 0)]

# Conditional probability Prob(y>0|x>0)
# Recall the equality P(A|B)P(B)=P(A & B)

Conditional_Prob <- length(which(y[which(x > 0)] > 0)) / length(which(x > 0))
Conditional_Prob

# Probability Prob(x>0, y>0)

intersect(which(x>0), which(y>0))

sum(x>0 & y>0)

Inter_Prob <- length(intersect(which(x>0), which(y>0))) / length(x)
Inter_Prob

# Probability Prob(x>0)

Prob_x <- length(which(x>0)) / length(x)
Prob_x

Inter_Prob / Prob_x # Prob(y>0 | x>0) same as the conditional probability


####### Conditional expectation ########

# Conditional expectation E[Y|x>0]

mean(y)
mean(y[which(x>0)])

### Let's do the same thing for X and Z.
# Conditional probability Prob(z>0|x>0)
# Recall the equality P(A|B)P(B)=P(A & B)

Conditional_Prob <- length(which(z[which(x > 0)] > 0)) / length(which(x > 0))
Conditional_Prob


# Probability Prob(z>0)
Prob_z <- length(which(z>0)) / length(z)
Prob_z


####### Conditional expectation ########

# Conditional expectation E[Z|x>0]

mean(z)
mean(z[which(x>0)])


###### Download stock prices and calculate returns ######

install.packages("quantmod")

library(quantmod)

name_list <- c("MSFT", "GOOG", "AAPL")
getSymbols(name_list, from = "2010-01-01", to = "2019-12-31", src = "yahoo")


# Simple return

diff(AAPL$AAPL.Adjusted)
diff(AAPL$AAPL.Adjusted) / AAPL$AAPL.Adjusted[-length(AAPL$AAPL.Adjusted)]

# Log return

log_returns = diff(log(AAPL$AAPL.Adjusted))
log_returns

n <- length(log_returns)
x <- as.vector(log_returns[2:(n-1)])
y <- as.vector(log_returns[3:n])
head(cbind(x, y))

# Check covariance and correlation

cov(x, y)
cor(x, y)

cov(cbind(x, y))
cor(cbind(x, y))

# 

### Crossectional correlation
aapl_ret = diff(log(AAPL$AAPL.Adjusted))[-1]
msft_ret = diff(log(MSFT$MSFT.Adjusted))[-1]
goog_ret = diff(log(GOOG$GOOG.Adjusted))[-1]

data <- cbind(aapl_ret, msft_ret, goog_ret)
data

cov(data)
cor(data)


mean(msft_ret)
mean(msft_ret[aapl_ret>0])





