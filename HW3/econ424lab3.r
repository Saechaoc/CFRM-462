# econ424lab3.R			script file for econ 424 lab3 calculations
#
# author: Eric Zivot
# created: September 17, 2008
# revision history: 
# July 7, 2016
#   Revised for summer 2016
# January 19, 2016
#   Revised for Winter 2016
# July 7, 2015
#   Revised for Summer 2015
# January 20, 2015
#   Revised for Winter 2015
# July 8, 2014
#   Revised for summer 2014
# July 6, 2012
#   Revised for summer 2012

# first install the packages from CRAN if you have not done so already.
# In Rstudio, go to the package tab and click the Install button, 
# type the package names then press the install button
options(digits=3)
library(IntroCompFinR)
library(PerformanceAnalytics)
library(zoo)
library(xts)
library(tseries)
library(quantmod)

#
# Matrix algebra 
#

# part a)
matA = matrix(c(1,4,7,2,4,8,6,1,3), 3, 3, byrow=T)
matA
matB = matrix(c(4,4,0,5,9,1,2,2,5), 3, 3, byrow=T)
matB
vecx = matrix(c(1,2,3), 3, 1)
vecx
vecy = matrix(c(5,2,7), 3, 1)
vecy

# part b)
t(matA)
t(matB)
t(vecx)
t(vecy)

# part c)

matA + matB
matA - matB
2*matA
matA%*%vecx
t(vecy)%*%matA%*%vecx

# d) x + y = 1, 2x + 4y = 2
# 1st line: y = 1 - x; 2nd line: y = 0.5 - 0.5 x
x.vals = seq(-1, 2, length = 20)
y.vals = 1 - x.vals
plot(x.vals, y.vals, type="l", col="blue", lwd=2)
abline(a=0.5,b=-0.5, lwd=2)
abline(v=1)
abline(h=0)

matA = matrix(c(1,1,2,4), 2, 2, byrow=T)
vecb = matrix(c(1,2), 2, 1)
matA
vecb
matA.inv = solve(matA)
matA.inv
z = matA.inv%*%vecb
z

# e) portfolio problem

vecmu = matrix(c(0.01,0.04,0.02), 3, 1)
matSigma = matrix(c(0.1,0.3,0.1,0.3,0.15,-0.2,0.10,-0.20, 0.08), 3, 3, byrow=T)
vecx = matrix(c(1/3,1/3,1/3), 3, 1)
vecmu
matSigma
vecx

crossprod(vecmu, vecx)
t(vecmu)%*%vecx
crossprod(vecx, matSigma%*%vecx)
t(vecx)%*%matSigma%*%vecx

#
# simulate time series data
#

# simulate MA(1) process with theta > 0
ma1.model.5 = list(ma=0.5)
mu = 0.05
set.seed(123)
ma1.sim.5 = mu + arima.sim(model=ma1.model.5, n=250, mean=0, sd=0.1)
                         
acf.ma1.model.5 = ARMAacf(ma=0.5, lag.max=10)

par(mfrow=c(3,1))
	ts.plot(ma1.sim.5, main="MA(1) Process: mu=0.05, theta=0.5",
	       xlab="time",ylab="y(t)")
	abline(h=0)
	plot(1:10, acf.ma1.model.5[2:11], type="h", col="blue", main="theoretical ACF", ylab="ACF")
	acf(ma1.sim.5, lag.max=10, main="Sample ACF")
par(mfrow=c(1,1))

# do the same for the the MA model with theta = 0.9. Copy above code and change theta to 0.9

# simulate AR(1) process with phi > 0
ar1.model.5 = list(ar=0.5)
mu = 0.05
set.seed(123)
ar1.sim.5 = mu + arima.sim(model=ar1.model.5, n = 250,
                         innov=rnorm(n=250, mean=0, sd=0.1))
acf.ar1.model.5 = ARMAacf(ar=0.5, lag.max=10)

par(mfrow=c(3,1))
	ts.plot(ar1.sim.5,main="AR(1) Process: mu=0.05, phi=0.5",
	       xlab="time",ylab="y(t)")
	abline(h=0)
	plot(1:10, acf.ar1.model.5[2:11], type="h", col="blue", main="Theoretical ACF")
	tmp=acf(ar1.sim.5, lag.max=10, main="Sample ACF")
par(mfrow=c(1,1))

# do the same for the model with phi = 0.9. copy above code and change phi to 0.9

#
# Descriptive Statistics
#

# get monthly adjusted closing price data on VBLTX, FMAGX and SBUX from Yahoo
# using the tseries function get.hist.quote(). Set sample to Jan 1998 through
# Dec 2009. Note: if you are not careful with the start and end dates
# or if you set the retclass to "ts" then results might look weird

# look at help on get.hist.quote
?get.hist.quote

# get the adjusted closing prices from Yahoo!
#VBLTX.prices = get.hist.quote(instrument="vbltx", start="1998-01-01",
#                              end="2009-12-31", quote="AdjClose",
#                              provider="yahoo", origin="1970-01-01",
#                              compression="m", retclass="zoo")
# New Code
getSymbols("vbltx", from="1998-01-01", to="2009-12-31")
VBLTX.prices=to.monthly(VBLTX)
monthlyReturn(VBLTX)

# change class of time index to yearmon which is appropriate for monthly data
# index() and as.yearmon() are functions in the zoo package 
#                             
index(VBLTX.prices) = as.yearmon(index(VBLTX.prices))

class(VBLTX.prices)
colnames(VBLTX.prices)
start(VBLTX.prices)
end(VBLTX.prices)

#FMAGX.prices = get.hist.quote(instrument="fmagx", start="1998-01-01",
#                              end="2009-12-31", quote="AdjClose",
#                              provider="yahoo", origin="1970-01-01",
#                              compression="m", retclass="zoo")
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
lab3Prices.z = merge(VBLTX.prices, FMAGX.prices, SBUX.prices)
lab3Prices.z = as.zoo(lab3Prices.z)
# rename columns
colnames(lab3Prices.z) = c("VBLTX", "FMAGX", "SBUX")

# calculate cc returns as difference in log prices
lab3Returns.z = diff(log(lab3Prices.z))
lab3Returns.z = as.zoo(lab3Returns.z)
lab3Returns.z=lab3Returns.z[complete.cases(lab3Returns.z), ]
#
# See the document "Working with Time Series in R" on the
# class webpage for more details on zoo objects
#    

# look at the return data
start(lab3Returns.z)
end(lab3Returns.z)
colnames(lab3Returns.z) 
head(lab3Returns.z)


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
plot(lab3Returns.z,col="blue", lwd=2, main="Monthly cc returns on 3 assets",
     panel=my.panel)


# all on the same graph 
plot(lab3Returns.z, plot.type="single", col=c("black","blue","red"), lwd=2,
     main="Monthly cc returns on 3 assets", 
     ylab="Return")
legend(x="bottom", legend=colnames(lab3Returns.z), col=c("black","blue","red"), lwd=2)    
abline(h=0)

# plot returns using the PerformanceAnalytics function chart.TimeSeries()
# this create a slightly nicer looking plot that plot.zoo()
?chart.TimeSeries
chart.TimeSeries(lab3Returns.z, legend.loc="bottom", main="") 

# the previous charts are a bit hard to read. the PerformanceAnalytics function
# chart.Bar makes it easier to compare the returns of different assets on the 
# same plot
?chart.Bar
chart.Bar(lab3Returns.z, legend.loc="bottom", main="")


# cumulative return plot - must use simple returns and not cc returns for this
# use PerformanceAnalytics function chart.CumReturns()
?chart.CumReturns
chart.CumReturns(diff(lab3Prices.z)/lag(lab3Prices.z, k=-1), 
                 legend.loc="topleft", wealth.index=TRUE,
                 main="Future Value of $1 invested")
#
# 4. Create matrix of return data. some core R functions don't work
#    correctly with zoo objects 
#

ret.mat = coredata(lab3Returns.z)
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
chart.Boxplot(lab3Returns.z)

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
table.Stats(lab3Returns.z)

#
# 7. Annualize monthly estimates
#

# annualized cc mean 
12*apply(ret.mat, 2, mean)

# annualized simple mean
exp(12*apply(ret.mat, 2, mean)) - 1

# annualized sd values
sqrt(12)*apply(ret.mat, 2, sd)



