
# References:   carleton_hsiang_climate_dose_response_2016.pdf, figure 3E.  This is derived from reference 21: W. Schlenker, M. J. Roberts, Nonlinear temperature effects indicate severe damages to U.S. crop yields under climate change. Proc. Natl. Acad. Sci. U.S.A. 106, 15594–15598 (2009). doi: 10.1073/pnas.0906865106; pmid: 19717432
#		https://www.r-bloggers.com/polynomial-regression-techniques/
#		https://stackoverflow.com/questions/29999900/poly-in-lm-difference-between-raw-vs-orthogonal

# Agricultural income in Brazil as function of rainfall (Carleton and Hsiang, 2016, figure 3f) 
# Rainfall is given in standard deviations; response is log(agricultural income).
thourlyC = seq(0,42,2)
log_yield = thourlyC
log_yield[1]=0
log_yield[2]=-0.005
log_yield[3]=0.005
log_yield[4]=0
log_yield[5]=-0.005
log_yield[6]=-0.0075
log_yield[7]=-0.005
log_yield[8]=0
log_yield[9]=0.005
log_yield[10]=0.0075
log_yield[11]=0.005
log_yield[12]=0
log_yield[13]=-0.005
log_yield[14]=-0.015
log_yield[15]=0
log_yield[16]=0.015
log_yield[17]=0.025
log_yield[18]=0.040
log_yield[19]=0.045
log_yield[20]=0.040
log_yield[21]=0.040
log_yield[22]=0.040

model <- lm(log_yield ~ poly(thourlyC,10,raw=TRUE))
summary(model)

# Coefficients are intercept and the cofficient of each power of t from 1 to 3.
# Could also use coef(summary(model))[1,1], [2,1], [3,1], and [4,1]
f4log_yield = function(t) {summary(model)$coefficients[1,1] + summary(model)$coefficients[2,1]*t + summary(model)$coefficients[3,1]*t^2 + summary(model)$coefficients[4,1]*t^3 + summary(model)$coefficients[5,1]*t^4 + summary(model)$coefficients[6,1]*t^5 + summary(model)$coefficients[7,1]*t^6 + summary(model)$coefficients[8,1]*t^7 + summary(model)$coefficients[9,1]*t^8 + summary(model)$coefficients[10,1]*t^9 + summary(model)$coefficients[11,1]*t^10 }

# Plot function and original points.
curve(f4log_yield, 0, 42, col="red", main="Maize yields (US)", xlab="Hourly temperature (degC)", ylab="Log(maize yield)")
#par(new=TRUE)
#points(thourlyC,log_yield, col="red")

