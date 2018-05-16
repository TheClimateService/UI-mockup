
# S. Hsiang, R. Kopp, et al, "Estimating Economic Damage in the United States from Climate Change", Science 356, 1362-1369 (2017).  Using table S15 from the supplementary information, quadratic-fit column.  t is change in GMST in degC.  Value returned is change in crime in percent.

# Define functions for violent and property crime.  Last number in function name indicates order of fit.
vcrime3 <- function(t) {-0.462 + 1.177*t - 0.0621*t^2 + 0.00281*t^3}
pcrime3 <- function(t) {-0.163 + 0.466*t - 0.0695*t^2 + 0.00364*t^3}

# Make plot.
ylimits <- c(-1,10)
plot(vcrime3,0,6, ylim=ylimits, main="Crime (Hsiang, 2017)", col="blue", lwd=3, xlab="Change in GMST (degC)", ylab="Crime Change (%)")
par(new=TRUE)
plot(pcrime3,0,6, ylim=ylimits, col="red", lwd=3, xlab="Change in GMST (degC)", ylab="Crime Change (%)")
par(new=TRUE)
legend("topright", c("Violent", "Property"), fill=c("blue","red"))
