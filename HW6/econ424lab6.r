# econ424lab6.r			script file for econ 424 lab6 calculations
#
# author: Eric Zivot
# created: October 20, 2003
# revised: 
# July 27, 2016 for summer 2016
# February 17, 2016 for Winter 2016
# August 4, 2015 for Summer 2015
# May 18, 2015 for Spring 2015
#
# comments:

options(digits=6, width=70)

# make sure packages are installed prior to loading them
library(corrplot)
library(IntroCompFinR)
library(PerformanceAnalytics)
library(zoo)


################################################################################
# introduction to portfolio theory
################################################################################

# load data from class website
lab6returns.df = read.csv(file="http://faculty.washington.edu/ezivot/econ424/424lab7returns.csv",
                          stringsAsFactors=FALSE)
# 7/31/12: fix to problem with the yearmon class
dates = seq(as.Date("1992-07-01"), as.Date("2000-10-01"), by="months")
lab6returns.df$Date = dates
# create zoo object
lab6returns.z = zoo(lab6returns.df[,-1], lab6returns.df$Date)
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}
plot(lab6returns.z, lwd=2, col="blue", panel = my.panel)

# compute estimates of CER model and annualize
muhat.annual = apply(lab6returns.z,2,mean)*12   
sigma2.annual = apply(lab6returns.z,2,var)*12
sigma.annual = sqrt(sigma2.annual)
covmat.annual = cov(lab6returns.z)*12 
covhat.annual = cov(lab6returns.z)[1,2]*12   
rhohat.annual = cor(lab6returns.z)[1,2]

mu.b = muhat.annual["rboeing"]
mu.m = muhat.annual["rmsft"]
sig2.b =  sigma2.annual["rboeing"]
sig2.m = sigma2.annual["rmsft"]
sig.b = sigma.annual["rboeing"]
sig.m = sigma.annual["rmsft"]
sig.bm = covhat.annual
rho.bm = rhohat.annual


#
# 3. create portfolios and plot
#
x.b = seq(from=-1, to=2, by=0.1)
x.m = 1 - x.b
mu.p = x.b*mu.b + x.m*mu.m
sig2.p = x.b^2 * sig2.b + x.m^2 * sig2.m + 2*x.b*x.m*sig.bm
sig.p = sqrt(sig2.p)

plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=c(rep("green", 18), rep("red", 13)))
text(x=sig.b, y=mu.b, labels="Boeing", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)

# now compute portfolios with assets and T-bills as well as Sharpe slopes

r.f = 0.03
# T-bills + Boeing
x.b = seq(from=0, to=2, by=0.1)
mu.p.b = r.f + x.b*(mu.b - r.f)
sig.p.b = x.b*sig.b
sharpe.b = (mu.b - r.f)/sig.b
sharpe.b

# T-bills + MSFT
x.m = seq(from=0, to=2, by=0.1)
mu.p.m = r.f + x.m*(mu.m - r.f)
sig.p.m = x.m*sig.m
sharpe.m = (mu.m - r.f)/sig.m
sharpe.m

plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=c(rep("green", 18), rep("red", 13)))
text(x=sig.b, y=mu.b, labels="Boeing", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)
points(sig.p.b, mu.p.b, type="b", col="blue")
points(sig.p.m, mu.p.m, type="b", col="orange")

plot(sig.p.b,mu.p.b, type = "b", pch = 16, ylim = c(0,max(mu.p)), 
     xlim = c(0,max(sig.p)), col = "red",
     xlab = expression(sigma[p]), ylab = expression(mu[p]))
points(sig.p.m, mu.p.m, type="b", col="green")
text(x=sig.b, y=mu.b, labels="Boeing", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)


#
# 4. compute global minimum variance portfolio
#

gmin.port = globalMin.portfolio(muhat.annual,
                                covmat.annual) 
gmin.port
summary(gmin.port, risk.free=0.03)
plot(gmin.port)

pie(gmin.port$weights)

sharpe.gmin = (gmin.port$er - r.f)/gmin.port$sd
sharpe.gmin

plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=c(rep("green", 18), rep("red", 13)))
text(x=sig.b, y=mu.b, labels="Boeing", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)
text(x=gmin.port$sd, y=gmin.port$er, labels="Global min", pos=2)

#
# 5. compute tangency portfolio
#

tan.port = tangency.portfolio(muhat.annual,
                              covmat.annual,
                              risk.free=0.03) 
tan.port
summary(tan.port,risk.free=0.03)
plot(tan.port)
pie(tan.port$weights)                              

# T-bills + tangency
x.t = seq(from=0, to=2, by=0.1)
mu.p.t = r.f + x.t*(tan.port$er - r.f)
sig.p.t = x.t*tan.port$sd
sharpe.t = (tan.port$er - r.f)/tan.port$sd
sharpe.t


plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=c(rep("green", 18), rep("red", 13)))
text(x=sig.b, y=mu.b, labels="Boeing", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)
text(x=tan.port$sd, y=tan.port$er, labels="Tangency", pos=2)
points(sig.p.t, mu.p.t, type="b", col="blue", pch=16)

#
# 6 
#

x.t = 0.1
x.f = 1 - x.t

mu.e = r.f + x.t*(tan.port$er - r.f)
sd.e = x.t*tan.port$sd
sharpe.e = (mu.e - r.f)/sd.e
sharpe.e


plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=c(rep("green", 18), rep("red", 13)))
text(x=sig.b, y=mu.b, labels="Boeing", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)
text(x=tan.port$sd, y=tan.port$er, labels="Tangency", pos=2)
points(sig.p.t, mu.p.t, type="b", col="blue", pch=16)
points(sd.e, mu.e, type="p", col="orange", pch=16, cex=2)
text(x=sd.e, y=mu.e, labels="Efficient Portfolio with 10% Tangency", pos=4, cex=0.75)


