# econ424lab4.r			script file for amath 462/econ 424 lab4 calculations
#
# author: Eric Zivot
# created: October 20, 2003
# revision history: 
# July 7, 2016
#   Revised for summer 2016
# revised: January 28, 2016
#
# comments:
# Data for the lab are  
# monthly continuously compounded returns on Vanguard long term bond index fund 
#(VBLTX), Fidelity Magellan stock mutual fund (FMAGX), and Starbucks stock (SBUX) 
#
# This lab requires the following packages
# PerformanceAnalytics  return and risk analytics
# zoo			              Zeilie's ordered observations
# tseries               various time series functions
# make sure you install these packages before you load them.

options(digits=3, width=70)
library(IntroCompFinR)
library(corrplot)
library(PerformanceAnalytics)
library(zoo)
library(tseries)
library(quantmod)

# get monthly adjusted closing price data on VBLTX, FMAGX and SBUX from Yahoo
# using the tseries function get.hist.quote(). Set sample to Jan 1998 through
# Dec 2009. Note: if you are not careful with the start and end dates
# or if you set the retclass to "ts" then results might look weird

# look at help on get.hist.quote
#?get.hist.quote

# get the adjusted closing prices from Yahoo!
#VBLTX.prices = get.hist.quote(instrument="vbltx", start="1998-01-01",
#                             end="2009-12-31", quote="AdjClose",
#                             provider="yahoo", origin="1970-01-01",
#                             compression="m", retclass="zoo")
# change class of time index to yearmon which is appropriate for monthly data
# index() and as.yearmon() are functions in the zoo package 
#     
getSymbols("vbltx", from="1998-01-01", to="2009-12-31")
VBLTX.prices=to.monthly(VBLTX)
monthlyReturn(VBLTX)
index(VBLTX.prices) = as.yearmon(index(VBLTX.prices))
                             
class(VBLTX.prices)
colnames(VBLTX.prices)
start(VBLTX.prices)
end(VBLTX.prices)

#FMAGX.prices = get.hist.quote(instrument="fmagx", start="1998-01-01",
#                             end="2009-12-31", quote="AdjClose",
#                             provider="yahoo", origin="1970-01-01",
#                             compression="m", retclass="zoo")
getSymbols("fmagx", from="1998-01-01", to="2009-12-31")
FMAGX.prices=to.monthly(FMAGX)
monthlyReturn(FMAGX)
index(FMAGX.prices) = as.yearmon(index(FMAGX.prices))

#SBUX.prices = get.hist.quote(instrument="sbux", start="1998-01-01",
#                             end="2009-12-31", quote="AdjClose",
#                             provider="yahoo", origin="1970-01-01",
#                             compression="m", retclass="zoo")
getSymbols("sbux", from="1998-01-01", to="2009-12-31")
SBUX.prices=to.monthly(SBUX)
monthlyReturn(SBUX)
index(SBUX.prices) = as.yearmon(index(SBUX.prices))

#subset the series to adjusted close only
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

#
# See the document "Working with Time Series in R" on the
# class webpage for more details on zoo objects
#    

# look at the return data
start(lab4Returns.z)
end(lab4Returns.z)
colnames(lab4Returns.z) 
head(lab4Returns.z)


#
# 3. Create time plots of data
#

# 3 panel plot (each y axis has different scale)
# note: here, the generic plot() function invokes the plot method for objects
# of class zoo. See the help on plot.zoo
# 
# panel function for plot.zoo to add horizontal line at zero in each panel
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}
plot(lab4Returns.z,col="blue", lwd=2, main="Monthly cc returns on 3 assets",
     panel=my.panel)


# all on the same graph 
plot(lab4Returns.z, plot.type="single", col=c("black","blue","red"), lwd=2,
     main="Monthly cc returns on 3 assets", 
     ylab="Return")
legend(x="bottom", legend=colnames(lab4Returns.z), col=c("black","blue","red"), lwd=2)    
abline(h=0)

# plot returns using the PerformanceAnalytics function chart.TimeSeries()
# this create a slightly nicer looking plot that plot.zoo()
?chart.TimeSeries
chart.TimeSeries(lab4Returns.z, legend.loc="bottom", main="") 

# the previous charts are a bit hard to read. the PerformanceAnalytics function
# chart.Bar makes it easier to compare the returns of different assets on the 
# same plot
?chart.Bar
chart.Bar(lab4Returns.z, legend.loc="bottom", main="")


# cumulative return plot - must use simple returns and not cc returns for this
# use PerformanceAnalytics function chart.CumReturns()
?chart.CumReturns
chart.CumReturns(diff(lab4Prices.z)/lag(lab4Prices.z, k=-1), 
                 legend.loc="topleft", wealth.index=TRUE,
                 main="Future Value of $1 invested")
#
# 4. Create matrix of return data. some core R functions don't work
#    correctly with zoo objects 
#

ret.mat = coredata(lab4Returns.z)
class(ret.mat)
colnames(ret.mat)
head(ret.mat)

#
# 5. Create graphical summaries of each data series
#

# online help on hist, boxplot, density, qqnorm
?hist
?boxplot
?density
?qqnorm

# here are the 4 panel plots for viewing the empirical distribution
# drop=FALSE preserves the column name
fourPanelPlot(ret.mat[, "VBLTX", drop=FALSE])
fourPanelPlot(ret.mat[, "FMAGX", drop=FALSE])
fourPanelPlot(ret.mat[, "SBUX", drop=FALSE])


# show boxplot of three series on one plot
boxplot(ret.mat, col="cornflowerblue")

# do the same thing using the PerformanceAnalytics function chart.Boxplot
chart.Boxplot(lab4Returns.z)

#
# 6. Compute univariate descriptive statistics
#

summary(ret.mat)

# compute descriptive statistics by column using the base R function apply()
# note: skewness and kurtosis are in the package PerformanceAnalytics
# note: kurtosis returns excess kurtosis

?apply
args(apply)
apply(ret.mat, 2, mean)
apply(ret.mat, 2, var)
apply(ret.mat, 2, sd)
apply(ret.mat, 2, skewness)
apply(ret.mat, 2, kurtosis)

# A nice PerformanceAnalytics function that computes all of the relevant
# descriptive statistics is table.Stats
?table.Stats
table.Stats(lab4Returns.z)

#
# 7. Annualize monthly estimates
#

# annualized cc mean 
12*apply(ret.mat, 2, mean)

# annualized simple mean
exp(12*apply(ret.mat, 2, mean)) - 1

# annualized sd values
sqrt(12)*apply(ret.mat, 2, sd)

#
# historical VaR
#
w0 = 100000
# matrix of empirical quantiles
q.hat = apply(ret.mat, 2, quantile, probs=c(0.01, 0.05))
q.hat

# compute all VaR values at once - isn't matrix programming great?
VaR.vals = w0*(exp(q.hat) - 1)
VaR.vals

#
# 8. Compute bivariate descriptive statistics
#

# online help on pairs
?pairs
pairs(ret.mat, col="cornflowerblue", pch=16, cex=1.5)

# online help on var and cor
?var
?cor

# compute 3 x 3 covariance and correlation matrices
var(ret.mat)
cov(ret.mat)
cor(ret.mat)

# visualize the correlation matrix with corrplot()
cor.mat = cor(ret.mat)
corrplot(cor.mat, method="ellipse")
corrplot.mixed(cor.mat, lower="number", upper="ellipse")

# if using Rstudio, press the Clear All button on the graph pane to reset
# the graphics window before making the next plots


#
# 9. Compute time series diagnostics
#

# autocorrelations

# online help on acf
?acf

par(mfrow=c(3,1))
	acf.msft = acf(ret.mat[,"VBLTX"], main="VBLTX")
	acf.sbux = acf(ret.mat[,"FMAGX"], main="FMAGX")
	acf.sp500 = acf(ret.mat[,"SBUX"], main="SBUX")
par(mfrow=c(1,1))


#
# 10. Compute estimates of CER model parameters
#
muhat.vals = apply(ret.mat, 2, mean)
muhat.vals
sigma2hat.vals = apply(ret.mat, 2, var)
sigma2hat.vals
sigmahat.vals = apply(ret.mat, 2, sd)
sigmahat.vals
cov.mat = var(ret.mat)
cov.mat
cor.mat = cor(ret.mat)
cor.mat
covhat.vals = cov.mat[lower.tri(cov.mat)]
rhohat.vals = cor.mat[lower.tri(cor.mat)]
names(covhat.vals) <- names(rhohat.vals) <- 
  c("VBLTX,FMAGX","VBLTX,SBUX","FMAGX,SBUX")
covhat.vals
rhohat.vals

cbind(muhat.vals,sigma2hat.vals,sigmahat.vals)
cbind(covhat.vals,rhohat.vals)

#
# 11. Compute standard errors for estimated parameters
#

# compute estimated standard error for mean
nobs = nrow(ret.mat)
nobs
se.muhat = sigmahat.vals/sqrt(nobs)
se.muhat

cbind(muhat.vals,se.muhat)

# compute approx 95% confidence intervals
mu.lower = muhat.vals - 2*se.muhat
mu.upper = muhat.vals + 2*se.muhat
cbind(mu.lower,mu.upper)

# compute estimated standard errors for variance and sd
se.sigma2hat = sigma2hat.vals/sqrt(nobs/2)
se.sigma2hat
se.sigmahat = sigmahat.vals/sqrt(2*nobs)
se.sigmahat

cbind(sigma2hat.vals,se.sigma2hat)
cbind(sigmahat.vals,se.sigmahat)

# compute approx 95% confidence intervals
sigma2.lower = sigma2hat.vals - 2*se.sigma2hat
sigma2.upper = sigma2hat.vals + 2*se.sigma2hat
cbind(sigma2.lower,sigma2.upper)

sigma.lower = sigmahat.vals - 2*se.sigmahat
sigma.upper = sigmahat.vals + 2*se.sigmahat
cbind(sigma.lower,sigma.upper)

# compute estimated standard errors for correlation
se.rhohat = (1-rhohat.vals^2)/sqrt(nobs)
se.rhohat
cbind(rhohat.vals,se.rhohat)

# compute approx 95% confidence intervals
rho.lower = rhohat.vals - 2*se.rhohat
rho.upper = rhohat.vals + 2*se.rhohat
cbind(rho.lower,rho.upper)

#
# 12. Compute 5% and 1% Value at Risk
#

# function to compute Value-at-Risk
# note: default values are selected for 
# the probability level (p) and the initial
# wealth (w). These values can be changed
# when calling the function. Highlight the entire
# function, right click and select run line or selection
Value.at.Risk = function(x,p=0.05,w=100000) {
  x = as.matrix(x)
  q = apply(x, 2, mean) + apply(x, 2, sd)*qnorm(p)
  VaR = (exp(q) - 1)*w
  VaR
}

# 5% and 1% VaR estimates based on W0 = 100000

Value.at.Risk(ret.mat,p=0.05,w=100000)
Value.at.Risk(ret.mat,p=0.01,w=100000)

