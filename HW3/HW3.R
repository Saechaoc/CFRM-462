#Homework 3

#a
A = matrix(
c(1,4,7,
  2,4,8,
  6,1,3), nrow = 3, ncol = 3, byrow = TRUE)

B = matrix(c(4,4,0,5,9,1,2,2,5), nrow = 3, ncol = 3, byrow = TRUE)

x = matrix(c(1,2,3), nrow = 3, ncol = 1)
y = matrix(c(5,2,7), nrow = 3, ncol = 1)

#b

t(A)
t(B)
t(x)
t(y)

#C
A+B
A-B
2*A
A%*%x
t(y)%*%A%*%x

#D
# y = 1 - x
# y = 1/2 - 1/2x
# plot both lines
x = seq(-1,2,length = 100)
plot(x,1-x, lwd=2, ylab="y", type = "l",col = "red",main = "Linear System")

abline(a=0.5, b=-0.5, lwd=2)
abline(h = 0,v=0)

# represent system in matrix algebra notation
matA = matrix(c(1,1,2,4), 2, 2, byrow=TRUE)
vecB = c(1,2)
matA.inv = solve(matA)
matA.inv
matA.inv%*%matA
matA%*%matA.inv
z = matA.inv%*%vecB
z

mu = matrix(c(0.01,0.04,0.02),nrow = 3, ncol = 1)

sigma = matrix(c(0.1,0.3,0.1,0.3,0.15,-0.2
,0.1,-0.2,0.08), nrow = 3, ncol = 3, byrow = TRUE)

#expected value
w = c(rep(1/3,3))
e.x = crossprod(w,mu)

#variance E[X^2] - E[x]^2

t(w) %*% sigma %*% w

#simulate a MA(1) model using a for loop
n.obs = 250
mu = 1
theta = 0.9
sigma.e = 1
set.seed(123)

#simulation of error
e = rnorm(n.obs, sd = sigma.e)

y = rep(0,n.obs)
y[1] = mu + e[1]

for(i in 2:n.obs) {
	y[i] = mu + e[i] + theta * e[i-1]
}

#simulate a MA(1) model using vectors
set.seed(123)
e = rnorm(n.obs, sd = sigma.e)
em1 = c(0, e[1:(n.obs-1)])
y = mu + e + theta*em1

#Simulate a MA(1) model using arima.sim()
set.seed(123)
ma1.model = list(0.5,0.9)
y = mu + arima.sim(model = ma1.model, n = 250, n.start = 1, start.innov = 0)

#Simulate a MA(1) model Y_t = 0.05 + e_t + theta * e_t-1 where e(t) ~ N(0,0.1^2)
set.seed(123)
mu = 0.05
e = rnorm(n.obs, mean = 0, sd = 0.1)
theta = list(ma = 0.5)
y.1 = mu + arima.sim(model = theta, n = 250, mean = 0, sd = 0.1)

set.seed(123)
theta = list(ma = 0.9)
y.2 = mu + arima.sim(model = theta, n = 250, mean = 0, sd = 0.1)

#Plot Y_t
par(mfrow = c(2,1))
	ts.plot(y.1, main = "MA(1) Process: mu = 0.05, theta = 0.5", xlab = "time", ylab = "y(t)")

plot(y.2, main = "MA(1) Process: mu = 0.05, theta = 0.9", xlab = "time", ylab = "y(t)")

#Mean value for Y_t
mean(y.1)
mean(y.2)

#Variance
var(y.1)
var(y.2)

#Theoretical ACF
ma.acf1 = ARMAacf(ar = 0, ma = 0.5, lag.max = 10)
ma.acf2 = ARMAacf(ar = 0, ma = 0.9, lag.max = 10)

#Plot ACFs
par(mfrow = c(2,1))
plot(0:10,ma.acf1, type = "h", lwd = 2, col = "blue", xlab = "lag", ylab = "rho(j)", main = "ACF for MA(1): θ = 0.5")

plot(0:10,ma.acf2, type = "h", lwd = 2, col = "blue", xlab = "lag", ylab = "rho(j)", main = "ACF for MA(1): θ = 0.9")

#Simulate Y_t - 0.05 = φ(Yt−1 − 0.05) + e_t where e_t ~ N(0,0.1^2)
set.seed(123)

ar1 = mu + arima.sim(model = list(ar = 0.5),n = 250, mean = 0, sd = 0.1)

ar2 = mu + arima.sim(model = list(ar = 0.9), n = 250, mean = 0, sd = 0.1)

#Plot AR
par(mfrow = c(2,1))
ts.plot(ar1, main = "AR(1) Process: mu = 0.05, phi = 0.5", xlab = "time", ylab = "y(t)")
ts.plot(ar2, main = "AR(1) Process: mu = 0.05, phi = 0.9", xlab = "time", ylab = "y(t)")

#Mean
mean(ar1)
mean(ar2)

#Var
var(ar1)
var(ar2)

#Theoretical ACF
ar.acf1 = ARMAacf(ar = 0.5, ma = 0, lag.max = 10)
ar.acf2 = ARMAacf(ar = 0.9, ma = 0, lag.max = 10)

par(mfrow = c(2,1))
plot(0:10,ar.acf1, type = "h", lwd = 2, col = "blue", xlab = "lag", ylab = "rho(j)", main = "ACF for AR(1): ϕ = 0.5")

plot(0:10,ar.acf2, type = "h", lwd = 2, col = "blue", xlab = "lag", ylab = "rho(j)", main = "ACF for AR(1): ϕ = 0.9")

#Load data
data = read.csv('~/Documents/CFRM 462/datasets/MCD_PriceDaily.csv')
head(data)
adjPrice = data[ , 7]
plot(adjPrice, type = "l", lwd = 2)

#Problem 9
#It is not stationary because Y_t != mu where the graph would deviate around the mean 

#Problem 10
n = length(adjPrice)
logRet = log(adjPrice[-1]/adjPrice[-n])
date = as.Date(data[,1],"%m/%d/%Y")
plot(date[-1],logRet, xlab = "Date", ylab = "Return",main = "McDonalds Log Returns", type = "l")

hist(logRet, 80, freq= FALSE)
qqnorm(logRet)
qqline(logRet)

#The returns look stationary
#Problem 3 & 4



# set.seed(123);
# head(arima.sim(model=list(ma=0.5), n=250, innov=rnorm(n=250, mean=0, sd=0.1)), n = 10)
# set.seed(123);
# e = rnorm(n = 250, mean = 0, sd = 0.1)
# set.seed(123);
# head(arima.sim(model = list(ma=0.5), n = 250, innov = e), n = 10)
# set.seed(123);
# head(arima.sim(model = list(ma=0.5), n = 250, sd = 0.1, mean = 0),n = 10)
