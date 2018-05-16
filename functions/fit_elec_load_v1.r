
# References:   auffhammer_peak_demand_electricity_2017.pdf, figure 1
#		https://www.r-bloggers.com/polynomial-regression-techniques/
#		https://stackoverflow.com/questions/29999900/poly-in-lm-difference-between-raw-vs-orthogonal

# Average and peak load for ERCOT (Auffhammer, figure 1, upper) 
# Loads are in MW relative to 15-18 degC; temperature variable is average daily temperature.
tempC = c(1,4,7,10,14,15,16,17,18,20,22,25,28,31)
avgload = c(14000, 7000, 4000, 2000, 500, 0, 0, 0, 0, 2500, 5000, 9000, 13000, 17000)
model <- lm(avgload ~ poly(tempC,3,raw=TRUE))
summary(model)
peakload = c(15000, 7500, 4500, 2000, 700, 0, 0, 0, 0, 3000, 7000, 13000, 19000, 25000)
model2 <- lm(peakload ~ poly(tempC,3,raw=TRUE))
summary(model2)

# Coefficients are intercept and the cofficient of each power of t from 1 to 3.
# Could also use coef(summary(model))[1,1], [2,1], [3,1], and [4,1]
f4avgload = function(t) {summary(model)$coefficients[1,1] + summary(model)$coefficients[2,1]*t + summary(model)$coefficients[3,1]*t^2 + summary(model)$coefficients[4,1]*t^3 }
f4peakload = function(t) {summary(model2)$coefficients[1,1] + summary(model2)$coefficients[2,1]*t + summary(model2)$coefficients[3,1]*t^2 + summary(model2)$coefficients[4,1]*t^3 }

# Plot function and original points.
curve(f4peakload, 1, 31, lwd=3, col="red", main="ERCOT Peak and\n Average loads", xlab="Average Daily Temperature (degC)", ylab="Load (Mw) Relative to 15-18 degC ")
par(new=TRUE)
curve(f4avgload, 1, 31, lwd=3, col="blue", add=T)
par(new=TRUE)
#points(tempC,peakload, col="red")
par(new=TRUE)
#points(tempC,avgload, col="blue")
legend("topleft", c("Peak load", "Average load"), fill=c("red","blue"))
