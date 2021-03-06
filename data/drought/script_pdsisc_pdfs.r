
library(tidyverse)
library(MASS)

dmon = read.table("./data/drought/pdsisc.monthly.maps.1900-2099.r2.5x2.5.EnsAvg14Models.TP2.ipe=2.nc.output.batch.boise.allmonths", header=TRUE)
d1 = dmon %>% filter(dmon$year<1950)
d2 = dmon %>% filter(dmon$year>=1950 & dmon$year<2000)
d3 = dmon %>% filter(dmon$year>=2020 & dmon$year<2040)
d4 = dmon %>% filter(dmon$year>=2040 & dmon$year<2060)
d5 = dmon %>% filter(dmon$year>=2060 & dmon$year<2080)
d6 = dmon %>% filter(dmon$year>=2080 & dmon$year<2100)

histbreaks = seq(-5,5, by=1.0)
histxlim = c(-5,5)
histylim = c(0,250)

# Fit normal distributions to the data and plot without histograms.
fit2 <- fitdistr(d2$monthly_value, "normal")
para2 <- fit2$estimate
curve(dnorm(x, para2[1], para2[2]), from=-5, to=5, col = 3, ylim=c(0,0.5), ylab="Probability", xlab="Drought Severity Index (lower value = worse drought)")
# 2-yr RP:  pnorm(0, 0, 1) evaluated to be 0.5.
# 5-yr RP:  pnorm(-0.84, 0, 1) evaluated to be 0.2004542.
# 10-yr RP:  pnorm(-1.28, 0, 1) evaluated to be 0.1002726.
# 20-yr RP:  pnorm(-1.65, 0, 1) evaluated to be 0.04947147.
# 50-yr RP:  pnorm(-2.05, 0, 1) evaluated to be 0.02018222.
# 100-yr RP:  pnorm(-2.32, 0, 1) evaluated to be 0.01017044.
# These are the values returned by qnorm(x,0,1)), where x is any of the fractions 0.5, 0.2, 0.1, etc.
#valueRP10yr <- -1.28*as.numeric(para2[2]) + as.numeric(para2[1])
valueRP10yr <- qnorm(0.1)*as.numeric(para2[2]) + as.numeric(para2[1])
abline(v=valueRP10yr, col = 3, lty=2)
par(new=TRUE)

fit3 <- fitdistr(d3$monthly_value, "normal")
para3 <- fit3$estimate
curve(dnorm(x, para3[1], para3[2]), from=-5, to=5, col = "blue", ylim=c(0,0.5), xlab=NA, ylab=NA)
par(new=TRUE)

fit4 <- fitdistr(d4$monthly_value, "normal")
para4 <- fit4$estimate
curve(dnorm(x, para4[1], para4[2]), from=-5, to=5, col = "orange", ylim=c(0,0.5), xlab=NA, ylab=NA)
par(new=TRUE)

fit5 <- fitdistr(d5$monthly_value, "normal")
para5 <- fit5$estimate
curve(dnorm(x, para5[1], para5[2]), from=-5, to=5, col = "red", ylim=c(0,0.5), xlab=NA, ylab=NA)
#par(new=TRUE)

#fit6 <- fitdistr(d6$monthly_value, "normal")
#para6 <- fit6$estimate
#curve(dnorm(x, para6[1], para6[2]), from=-5, to=5, col = "black", ylim=c(0,0.5), xlab=NA, ylab=NA)
#legend('right', c('1950-99', '2020-39', '2040-59', '2060-79', '2080-99'), fill=c(3,"blue","orange","red","black"), border=NA)

legend('right', c('1950-99', '2020-39', '2040-59', '2060-79'), fill=c(3,"blue","orange","red"), border=NA)



