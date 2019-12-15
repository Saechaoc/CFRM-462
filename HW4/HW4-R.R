#Chris Saechao
#CFRM 462 - Homework 4 CER Models and Descriptive Statistics

#Create Settings and Get library's
options(digits=3, width=70)
library(IntroCompFinR)
library(corrplot)
library(PerformanceAnalytics)
library(zoo)
library(tseries)
library(quantmod)

#Get VBLTX, FMAGX, SBUX
getSymbols("vbltx", from="1998-01-01", to="2009-12-31")
VBLTX.prices=to.monthly(VBLTX)
monthlyReturn(VBLTX)
index(VBLTX.prices) = as.yearmon(index(VBLTX.prices))
                             
class(VBLTX.prices)
colnames(VBLTX.prices)
start(VBLTX.prices)
end(VBLTX.prices)

#FMAGX
getSymbols("fmagx", from="1998-01-01", to="2009-12-31")
FMAGX.prices=to.monthly(FMAGX)
monthlyReturn(FMAGX)
index(FMAGX.prices) = as.yearmon(index(FMAGX.prices))

#SBUX
getSymbols("sbux", from="1998-01-01", to="2009-12-31")
SBUX.prices=to.monthly(SBUX)
monthlyReturn(SBUX)
index(SBUX.prices) = as.yearmon(index(SBUX.prices))

VBLTX.prices = VBLTX.prices[,6]
FMAGX.prices = FMAGX.prices[,6]
SBUX.prices = SBUX.prices[,6]

# create merged price data
lab4Prices.z = merge(VBLTX.prices, FMAGX.prices, SBUX.prices)
lab4Prices.z = as.zoo(lab4Prices.z)
# rename columns
colnames(lab4Prices.z) = c("VBLTX", "FMAGX", "SBUX")

# calculate cc returns as difference in log prices
lab4Returns.z = diff(log(lab4Prices.z))

#Q1 - Historical VaR - REDO this shit is wrong 
#Consider a 100,000 investment 

w = 100000

VBLTX = quantile(lab4Returns.z[,1],c(0.01,0.05))
FMAGX = quantile(lab4Returns.z[,2],c(0.01,0.05))
SBUX = quantile(lab4Returns.z[,3],c(0.01,0.05))

returns = c(VBLTX,FMAGX,SBUX)

#Calculate the sample var
sample_var = (exp(returns)-1)*w

#Calculate the empirical var
empirical_var = apply(lab4Returns.z,2, quantile, probs = c(0.01,0.05))

# compute all VaR values at once 
p_var = w*(exp(empirical_var) - 1)
p_var


#Q2 - Create Pairwise Scatter Plots of Returns
pairs(coredata(lab4Returns.z),col = 'blue', pch = 16, cex = 1.25,cex.axis = 1.25, main = "Pairwise Scatter Plot")

#Q3
corData = cor(lab4Returns.z)

corrplot(corData)

corrplot.mixed(corData, lower = "number", upper = "ellipse")

#Q4 - Create a sample covariance matrix and the sample correlation matrix Var(x) = cov(x) = E[(x-u)(x-u)'] where x = (x1,x2,x3...xn) and the sample correlation matrix is inv(D)*Sigma*inv(D)

#Sample Covariance Matrix
cov(lab4Returns.z)

#Same as cov matrix 
var(lab4Returns.z)

#Sample Correlation Matrix
cor(lab4Returns.z)

#Plot ACF
acf(lab4Returns.z)

#Part III

mu_hat = apply(lab4Returns.z, 2, mean)

sigma2_hat = apply(lab4Returns.z, 2, var)

sigma_hat = apply(lab4Returns.z, 2, sd)

cov.mat = cov(lab4Returns.z)
cor.mat = cor(lab4Returns.z)

covhat.vals = cov.mat[lower.tri(cov.mat)]
rhohat.vals = cor.mat[lower.tri(cor.mat)]

names(covhat.vals) <- names(rhohat.vals) <- 
  c("VBLTX,FMAGX","VBLTX,SBUX","FMAGX,SBUX")
covhat.vals
rhohat.vals

cbind(muhat.vals,sigma2hat.vals,sigmahat.vals,covhat.vals,rhohat.vals)
cbind(covhat.vals,rhohat.vals)

#Part II Q2 - Compute standard error
#SE(mu) = sigma/sqrt(t)

t = nrow(lab4Returns.z)
se.mu = sigma_hat/sqrt(t)

#SE(sigma^2) = sigma^2/sqrt(t/2)
se.sigma2 = sigma2_hat/sqrt(t/2)

#SE(sigma) = sigma/sqrt(2t)
se.sigma = sigma_hat/sqrt(2*t)

#SE(rho) = (1-rho^2)/sqrt(t)
se.rho = rhohat.vals/sqrt(t)

se.mu
se.sigma2
se.sigma
se.rho

#Compute 95 & 99% confidence intervals
mu.lower = mu_hat - 2 * se.mu
mu.upper = mu_hat + 2 * se.mu

mu.lower2 = mu_hat - 3 * se.mu
mu.upper2 = mu_hat + 3 * se.mu

mu.95 = cbind(mu.lower,mu.upper)
mu.99 = cbind(mu.lower2,mu.upper2)

var.lower = sigma2_hat - 2 * se.sigma2
var.upper = sigma2_hat + 2 * se.sigma2

var.lower2 = sigma2_hat - 3 * se.sigma2
var.upper2 = sigma2_hat + 3 * se.sigma2

var.95 = cbind(var.lower, var.upper)
var.99 = cbind(var.lower2, var.upper2)

sd.lower = sigma_hat - 2 * se.sigma
sd.upper = sigma_hat + 2 * se.sigma

sd.lower2 = sigma_hat - 3 * se.sigma
sd.upper2 = sigma_hat + 3 * se.sigma

sd.95 = cbind(sd.lower, sd.upper)
sd.99 = cbind(sd.lower2, sd.upper2)

rho.lower = rhohat.vals - 2 * se.rho
rho.upper = rhohat.vals + 2 * se.rho

rho.lower2 = rhohat.vals - 3 * se.rho
rho.upper2 = rhohat.vals + 3 * se.rho

rho.95 = cbind(rho.lower, rho.upper)
rho.99 = cbind(rho.lower2, rho.upper2)

mu.95
mu.99
var.95
var.99
sd.95
sd.99
rho.95
rho.99

#Var is mu + sigma * qnorm(p)
obs_var.05 = mu_hat + sigma_hat * qnorm(0.05)
obs_var.01 = mu_hat + sigma_hat * qnorm(0.01)

obs_var.95 = (exp(obs_var.05)-1)*w
obs_var.99 = (exp(obs_var.01)-1)*w

obs_var.95
obs_var.99