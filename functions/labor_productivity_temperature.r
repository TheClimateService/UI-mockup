
# S. Hsiang, R. Kopp, et al, "Estimating Economic Damage in the United States from Climate Change", Science 356, 1362-1369 (2017).  Using table S14 from the supplementary information, quadratic-fit column.  t is change in GMST in degC.  Value returned is change in labor productivity in percent.

# Define low-sensitivity and high-sensitivity functions
lowlabor <- function(t) {0.0443 - 0.0650*t -0.00658*t^2}
highlabor <- function(t) {0.215 - 0.422*t -0.0154*t^2}

# Make plot.
plot(lowlabor,0,10, ylim=c(-6,0.2), main="Indoor and Outdoor Labor Productivity (Hsiang, 2017)", col="blue", lwd=3, xlab="Change in GMST (degC)", ylab="Productivity Change (%)")
par(new=TRUE)
plot(highlabor,0,10, ylim=c(-6,0.2), col="red", lwd=3, xlab="Change in GMST (degC)", ylab="Productivity Change (%)")
par(new=TRUE)
legend("topright", c("Indoor", "Outdoor"), fill=c("blue","red"))
