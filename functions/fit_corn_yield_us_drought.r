
# References:   wang_crop_productivity_climate_midwestUS_2016.pdf, figure 13.  R.Wang, et al, "Estimation of the effects of climate variability on crop yield in the Midwest USA", Agricultural and Forest Meteorology, 216:141-156, 2016.
#		https://www.r-bloggers.com/polynomial-regression-techniques/
#		https://stackoverflow.com/questions/29999900/poly-in-lm-difference-between-raw-vs-orthogonal

# Corn yield reduction as function of log10(drought return period) based on historical data.
return_period_yrs = c(1.01,2,5,10,20,50,100)
yield_reduction_pct = c(0,5,12,15,20,24,31)
model <- lm(yield_reduction_pct ~ poly(log10(return_period_yrs),1,raw=TRUE))
summary(model)

# NOTE THAT THE FUNCTION IS DESIGNED TO OPERATE ON LOG10 OF THE RETURN PERIOD.
# Example:  f4yield_reduction_pct(2.0) = 30.07868
# Coefficients are intercept and the cofficient of each power of t from 1 to 1.
# Could also use coef(summary(model))[1,1], [2,1], [3,1], and [4,1]
f4yield_reduction_pct = function(t) {summary(model)$coefficients[1,1] + summary(model)$coefficients[2,1]*t }

# Plot function and original points.
curve(f4yield_reduction_pct, 0.004, 2, log="x", xaxt="n", col="red", main="Yield Reduction as Function\n of Drought Return Period", xlab="Return Period (years)", ylab="Yield Reduction (percent)")
axis(1, at=log10(return_period_yrs), labels=floor(return_period_yrs))
# The following adds a line below the xaxis label.
# mtext("wang_crop_productivity_climate_midwestUS_2016", side=1, line=4)
#par(new=TRUE)
#points(log10(return_period_yrs), yield_reduction_pct, col="black")

