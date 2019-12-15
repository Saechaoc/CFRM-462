# econ424lab5.r			script file for econ 424 lab5 calculations
#
# author: Eric Zivot
# created: October 20, 2003
# revised: 
#   July 18, 2017 for summer 2017 - Anthony Sanford
#   July 21, 2016 for summer 2016
#   July 29, 2015 for summer 2015
#   July 17, 2012
#
# comments:

options(digits=3, width=70)

# make sure packages are installed prior to loading them
library(PerformanceAnalytics)
library(zoo)
library(boot)
library(tseries)

# get monthly adjusted closing price data on VBISX, FBGRX and GOOGL from Yahoo
# using the tseries function get.hist.quote(). Set sample to Sept 2005 through
# Sep 2010. Note: if you are not careful with the start and end dates
# or if you set the retclass to "ts" then results might look weird

# get the last five years of monthly adjusted closing prices from Yahoo!
VBISX.prices = get.hist.quote(instrument="vbisx", start="2005-09-01",
                             end="2010-09-30", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
# change class of time index to yearmon which is appropriate for monthly data
# index() and as.yearmon() are functions in the zoo package 
#                             
index(VBISX.prices) = as.yearmon(index(VBISX.prices))
                             
class(VBISX.prices)
colnames(VBISX.prices)
start(VBISX.prices)
end(VBISX.prices)

FBGRX.prices = get.hist.quote(instrument="fbgrx", start="2005-09-01",
                             end="2010-09-30", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
index(FBGRX.prices) = as.yearmon(index(FBGRX.prices))

GOOGL.prices = get.hist.quote(instrument="googl", start="2005-09-01",
                             end="2010-09-30", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
index(GOOGL.prices) = as.yearmon(index(GOOGL.prices))

# create merged price data
lab5Prices.z = merge(VBISX.prices, FBGRX.prices, GOOGL.prices)
# rename columns
colnames(lab5Prices.z) = c("VBISX", "FBGRX", "GOOGL")

# calculate cc returns as difference in log prices
lab5Returns.z = diff(log(lab5Prices.z))

#
# 3. Create timePlots of data
#

plot(lab5Returns.z, plot.type="single", lty=1:3, col=1:3, lwd=2)
legend(x="bottomleft", legend=colnames(lab5Returns.z), lty=1:3, col=1:3, lwd=2)
abline(h=0)
title("Monthly cc returns")


#
# 4. Create matrix of return data and compute pairwise scatterplots
#

ret.mat = coredata(lab5Returns.z)
colnames(ret.mat)
head(ret.mat)
VBISX = ret.mat[,"VBISX"]
FBGRX = ret.mat[,"FBGRX"]
GOOGL = ret.mat[,"GOOGL"]
pairs(ret.mat, col="blue")

#
# 5. Compute estimates of CER model parameters
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
c("VBISX,FBGRX","VBISX,GOOGL","FBGRX,GOOGL")
covhat.vals
rhohat.vals

# summarize the CER model estimates
cbind(muhat.vals,sigma2hat.vals,sigmahat.vals)
cbind(covhat.vals,rhohat.vals)

# plot mean vs. sd values
plot(sigmahat.vals, muhat.vals, pch=1:3, cex=2, col=1:3, 
     ylab = "mean", xlab="sd (risk)")
abline(h=0)     
legend(x="topleft", legend=names(muhat.vals), pch=1:3, col=1:3, cex=1.5)     

#
# 6. Compute stndard errors for estimated parameters
#

# compute estimated standard error for mean
nobs = nrow(ret.mat)
nobs
se.muhat = sigmahat.vals/sqrt(nobs)
se.muhat
# show estimates with SE values underneath
rbind(muhat.vals,se.muhat)

# compute approx 95% confidence intervals
mu.lower = muhat.vals - 2*se.muhat
mu.upper = muhat.vals + 2*se.muhat
cbind(mu.lower,mu.upper)

# compute estimated standard errors for variance and sd
se.sigma2hat = sigma2hat.vals/sqrt(nobs/2)
se.sigma2hat
se.sigmahat = sigmahat.vals/sqrt(2*nobs)
se.sigmahat

rbind(sigma2hat.vals,se.sigma2hat)
rbind(sigmahat.vals,se.sigmahat)

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
rbind(rhohat.vals,se.rhohat)

# compute approx 95% confidence intervals
rho.lower = rhohat.vals - 2*se.rhohat
rho.upper = rhohat.vals + 2*se.rhohat
cbind(rho.lower,rho.upper)


#
# 7. Evaluate bias and SE formulas using Monte Carlo
#

# generate 1000 samples from CER and compute sample statistics

mu = -0.0008 	#muhat.vals["FBGRX"]
sd = 0.06716
n.obs = 60
set.seed(123)
n.sim = 1000

sim.means = rep(0,n.sim)
sim.vars = rep(0,n.sim)
sim.sds = rep(0,n.sim)
for (sim in 1:n.sim) {
	sim.ret = rnorm(n.obs,mean=mu,sd=sd)
	sim.means[sim] = mean(sim.ret)
	sim.vars[sim] = var(sim.ret)
	sim.sds[sim] = sqrt(sim.vars[sim])
}

par(mfrow=c(2,2))
hist(sim.means,xlab="mu hat", col="slateblue1")
abline(v=mu, col="white", lwd=2)
hist(sim.vars,xlab="sigma2 hat", col="slateblue1")
abline(v=sd^2, col="white", lwd=2)
hist(sim.sds,xlab="sigma hat", col="slateblue1")
abline(v=sd, col="white", lwd=2)
par(mfrow=c(1,1))

# 
# 8. compute MC estimates of bias and SE
#

c(mu, mean(sim.means))
mean(sim.means) - mu
c(sd^2, mean(sim.vars))
mean(sim.vars) - sd^2
c(sd, mean(sim.sds))
mean(sim.sds) - sd

# compute MC SE value and compare to SE calculated from simulated data

c(se.muhat["FBGRX"], sd(sim.means))
c(se.sigma2hat["FBGRX"], sd(sim.vars)) 
c(se.sigmahat["FBGRX"], sd(sim.sds))


#
# 9. bootstrapping SE for mean, variance, sd and correlation
#

?boot
# note: boot requires user-supplied functions that take
# two arguments: data and an index. The index is created
# by the boot function and represents random resampling with
# replacement

# function for bootstrapping sample mean
mean.boot = function(x, idx) {
# arguments:
# x 		data to be resampled
# idx		vector of scrambled indices created by boot() function
# value:
# ans		mean value computed using resampled data
     ans = mean(x[idx])
     ans
}

VBISX.mean.boot = boot(VBISX, statistic = mean.boot, R=999)
class(VBISX.mean.boot)
names(VBISX.mean.boot)

# print, plot and qqnorm methods
VBISX.mean.boot
se.muhat["VBISX"]

# plot bootstrap distribution and qq-plot against normal
plot(VBISX.mean.boot)


# compute bootstrap confidence intervals from normal approximation
# basic bootstrap method and percentile intervals
boot.ci(VBISX.mean.boot, conf = 0.95, type = c("norm","perc"))

#
# boostrap SD estimate
#
# function for bootstrapping sample standard deviation
sd.boot = function(x, idx) {
# arguments:
# x 		data to be resampled
# idx		vector of scrambled indices created by boot() function
# value:
# ans		sd value computed using resampled data
     ans = sd(x[idx])
     ans
}

VBISX.sd.boot = boot(VBISX, statistic = sd.boot, R=999)
VBISX.sd.boot
se.sigmahat["VBISX"]

# plot bootstrap distribution
plot(VBISX.sd.boot)

# compute confidence intervals
boot.ci(VBISX.sd.boot, conf=0.95, type=c("norm", "basic", "perc"))

var.boot = function(x,idx) {
	ans = var(x[idx])
	ans
}

VBISX.var.boot = boot(VBISX, statistic = var.boot, R = 999)
VBISX.var.boot
se.sigma2hat["VBISX"]

# bootstrap correlation

# function to compute correlation between 1st 2 variables in matrix
rho.boot = function(x.mat, idx) {
# x.mat	n x 2 data matrix to be resampled
# idx		vector of scrambled indices created by boot() function
# value:
# ans		correlation value computed using resampled data

	ans = cor(x.mat[idx,])[1,2]
	ans
}
VBISX.FBGRX.cor.boot = boot(ret.mat[,c("VBISX","FBGRX")],
                           statistic=rho.boot, R = 999)
VBISX.FBGRX.cor.boot
se.rhohat[1]

# plot bootstrap distribution
plot(VBISX.FBGRX.cor.boot)

# bootstrap confidence intervals
boot.ci(VBISX.FBGRX.cor.boot, conf=0.95, type=c("norm", "perc"))

#plot the boostrap distributions
plot(VBISX.mean.boot)
plot(VBISX.sd.boot)
plot(VBISX.var.boot)

#
# 10. Bootstrap VaR
#

# 5% Value-at-Risk
ValueAtRisk.boot = function(x, idx, p=0.05, w=100000) {
# x.mat	data to be resampled
# idx		vector of scrambled indices created by boot() function
# p		probability value for VaR calculation
# w		value of initial investment
# value:
# ans		Value-at-Risk computed using resampled data

	q = mean(x[idx]) + sd(x[idx])*qnorm(p)
	VaR = (exp(q) - 1)*w
	VaR
}

VBISX.VaR.boot = boot(VBISX, statistic = ValueAtRisk.boot, R=999)
VBISX.VaR.boot
boot.ci(VBISX.VaR.boot, conf=0.95, type=c("norm", "perc"))
plot(VBISX.VaR.boot)

VBISX.mean.boot
VBISX.sd.boot