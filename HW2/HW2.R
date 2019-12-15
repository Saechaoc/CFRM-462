#Plotting Distributions

x.vals = seq(-4,4,length = 150)* 0.1 + 0.05

plot(x.vals,dnorm(x.vals,mean = 0.05,sd = 0.1),type = "l",lwd = 2, col = "blue", xlab = "x", ylab = "pdf",main = "Combined PDF")

#Add Starbucks
lines(x.vals,dnorm(x.vals,mean = 0.025,sd = 0.05))


mu.R = 0.04
sd.R = 0.09

w0 = 10000

#Simple monthly return
q.01.R = mu.R + sd.R *qnorm(0.01)
q.05.R = mu.R + sd.R *qnorm(0.05)

var.01 = abs(q.01.R * w0)
var.05 = abs(q.05.R * w0)

var.01
var.05

#Continuously Compounded Return

e.01.R = exp(mu.R + sd.R qnorm(0.01)) - 1
e.05.R = exp(mu.R + sd.R * qnorm(0.05)) - 1

e.Var.01 = abs(e.01.R * w0)
e.Var.05 = abs(e.05.R * w0)

e.Var.01
e.Var.05

#12 Month Return

abs(e.01.R * 12 * w0)
abs(e.05.R * 12 * w0)

#T distribution
df = c(1,2,5,10)
x.vals = seq(-5,5, length = 100)
pos = seq(0,5,length = 100)

plot(x.vals,dt(x.vals,1),type = "l", lwd = 2, col = 1, ylim = c(0,0.6), xlab = "X", ylab = "PDF", main = "Student T Distribution")
lines(x.vals,dt(x.vals,2),type = "l", lwd = 1, col = 2)
lines(x.vals,dt(x.vals,5),type = "l", lwd = 1, col = 3)
lines(x.vals,dt(x.vals,10),type = "l", lwd = 1, col = 4)

#Chi Squared
plot(pos,dchisq(pos,1),type = "l", lwd = 1, col = 5, xlab = "X", ylab = "PDF", main = "Chi-Squared")
lines(pos,dchisq(pos,2),type = "l", lwd = 1, col = 6)
lines(pos,dchisq(pos,5),type = "l", lwd = 1, col = 7)
lines(pos,dchisq(pos,10),type = "l", lwd = 1, col = 8)

#Problem 6
e.x = 1*.3 + 2*.3 + 3 * 0.4
var.x = 1*.3 + 4*.3 + 9 * .4 - E.x^2
sd.x = sqrt(var.x)

e.y = 1 * 0.2 + 2*0.3 + 3*0.5
var.y = 1* 0.2 + 4 * 0.3 + 9 * 0.5 - e.y^2
sd.y = sqrt(var.y)

e.xy = (1*1*0.1) + (1*2*0.2) + (2*1*0.1) + (2*3*0.2) + (3*2*0.1) + (9*0.3)

cov.xy = e.xy - e.y*e.x
cor.xy = cov.xy/(sd.x*sd.y)

#a
answers <- c(e.x,var.x,sd.x,e.y,var.y,sd.y)

#Problem 7
b.amzn = 38.23
b.costco = 41.11

s.amzn = 41.29
s.costco = 41.74

#Simple Monthly Return
r.amzn = (s.amzn - b.amzn)/b.amzn
r.costco = (s.costco - b.costco)/b.costco

#CC Return
cc.amzn = log(s.amzn/b.amzn)
cc.costco = log(s.costco/b.costco)

#dividend
div = 0.1
rd.costco = (s.costco + div)/(b.costco) - 1
rd.yield = div/b.costco

#annualized
annual.amzn = r.amzn^12 - 1
annual.costco = r.costco^12 - 1

annual.amzn.cc = 12 * cc.amzn
annual.costco.cc = 12 * cc.costco

#Portfolio Returns
x_a = 0.8
x_c = 0.2

r.combined = x_a * r.amzn + x_c * r.costco
cc.combined = log(1+r.combined)

#problem 8
R_e = (1.3-1.5)/1.5
R_uk = (45-40)/40
R_us = (58.5-60)/60


