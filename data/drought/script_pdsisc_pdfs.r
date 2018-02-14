
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



