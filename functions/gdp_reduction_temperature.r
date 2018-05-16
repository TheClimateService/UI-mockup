
# S. Hsiang, R. Kopp, et al, "Estimating Economic Damage in the United States from Climate Change", Science 356, 1362-1369 (2017).  Using table S16 from the supplementary information, quadratic-fit column.  t is change in GMST in degC.  Value returned is change in annual GDP in percent.

gdp2 <- function(t) {-0.0547 + 0.283*t + 0.146*t^2}
gdp3 <- function(t) {0.182 -0.0325*t + 0.249*t^2 - 0.00901*t^3}

# Make plot.
plot(gdp2,0,10, lwd=3, col="orange", main="GDP Total Direct Damages (Hsiang, 2017)", xlab="Change in GMST (degC)", ylab="Reduction in Annual GDP (%)")
par(new=TRUE)

