
# References:   carleton_hsiang_climate_dose_response_2016.pdf, figure 3f.  This is derived from reference 153:  F. Hidalgo, S. Naidu, S. Nichter, N. Richardson, Economic determinants of land invasions. Rev. Econ. Stat. 92, 505–523 (2010). doi: 10.1162/REST_a_00007 .
#		https://www.r-bloggers.com/polynomial-regression-techniques/
#		https://stackoverflow.com/questions/29999900/poly-in-lm-difference-between-raw-vs-orthogonal

# Agricultural income in Brazil as function of rainfall (Carleton and Hsiang, 2016, figure 3f) 
# Rainfall is given in standard deviations; response is log(agricultural income).
rainfallSD = c(-3,-2,-1,0,1,2,3)
log_income = c(-0.13,-0.07,-0.01,0.02,0.04,0.025,0.015)
model <- lm(log_income ~ poly(rainfallSD,3,raw=TRUE))
summary(model)

# Coefficients are intercept and the cofficient of each power of t from 1 to 3.
# Could also use coef(summary(model))[1,1], [2,1], [3,1], and [4,1]
f4log_income = function(t) {summary(model)$coefficients[1,1] + summary(model)$coefficients[2,1]*t + summary(model)$coefficients[3,1]*t^2 + summary(model)$coefficients[4,1]*t^3 }

# Plot function and original points.
curve(f4log_income, -3, 3, col="red", main="Agricultural income in Brazil", xlab="Rainfall Standard Deviation", ylab="Log(agricultural income)")
#par(new=TRUE)
#points(rainfallSD,log_income, col="red")
