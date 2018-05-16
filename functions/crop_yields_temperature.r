
# S. Hsiang, R. Kopp, et al, "Estimating Economic Damage in the United States from Climate Change", Science 356, 1362-1369 (2017).  Using tables S10 and S11 from the supplementary information, quadratic-fit column.  t is change in GMST in degC.  Value returned is percent change in yield.

# Functions from table above.  Last number in function name refers to the order of the fit.
maize3 <- function(t) {1.723 - 8.139*t - 2.758*t^2 + 0.289*t^3}
wheat3 <- function(t) {-5.356 + 9.310*t + 0.00313*t^2 - 0.112*t^3}
totgrains2 <- function(t) {2.814 + -6.317*t - 0.0973*t^2}
totgrains3 <- function(t) {-1.059 - 1.403*t - 1.632*t^2 + 0.128*t^3}
cotton3 <- function(t) {-9.606 + 15.99*t - 6.667*t^2 + 0.422*t^3}
soy3 <- function(t) {-3.731 + 9.224*t - 5.900*t^2 + 0.425*t^3}
totcrop3 <- function(t) {-2.94 + 4.566*t - 3.653*t^2 + 0.259*t^3}

# Make plot.
xlimits <- c(0,6)
ylimits <- c(-80,40)
plot(maize3, xlim=xlimits, ylim=ylimits, main="Crop Yields (Hsiang, 2017)", col="blue", lwd=3, xlab="Change in GMST (degC)", ylab="Yield Change (percent)")
par(new=TRUE)
plot(wheat3, xlim=xlimits, ylim=ylimits, col="red", lwd=3, xlab="Change in GMST (degC)", ylab="Yield Change (percent)")
par(new=TRUE)
plot(totgrains3, xlim=xlimits, ylim=ylimits, col="orange", lwd=3, xlab="Change in GMST (degC)", ylab="Yield Change (percent)")
par(new=TRUE)
plot(cotton3, xlim=xlimits, ylim=ylimits, col="green", lwd=3, xlab="Change in GMST (degC)", ylab="Yield Change (percent)")
par(new=TRUE)
plot(soy3, xlim=xlimits, ylim=ylimits, col="black", lwd=3, xlab="Change in GMST (degC)", ylab="Yield Change (percent)")
par(new=TRUE)
plot(totcrop3, xlim=xlimits, ylim=ylimits, col="cyan", lwd=3, xlab="Change in GMST (degC)", ylab="Yield Change (percent)")
par(new=TRUE)
legend("topleft", c("Maize", "Wheat", "Total Grains", "Cotton", "Soy", "Total Crops"), fill=c("blue","red","orange","green","black","cyan"))
