
# S. Hsiang, R. Kopp, et al, "Estimating Economic Damage in the United States from Climate Change", Science 356, 1362-1369 (2017).  Using table S12 from the supplementary information, quadratic-fit column.  t is change in GMST in degC.  Value returned is deaths per 100,000 population.

# Define low-sennsitivity and high-sensitivity functions
mall2 <- function(t) {-1.56 + 0.67*t + 0.692*t^2}
mall3 <- function(t) {1.516 - 3.233*t + 1.911*t^2 - 0.102*t^3}
mall4 <- function(t) {0.65 - 1.623*t + 1.045*t^2 + 0.0688*t^3 - 0.0108*t^4}
minfant3 <- function(t) {1.012 - 1.972*t + 1.384*t^2 - 0.0671*t^3}
m1_44_3 <- function(t) {-0.449 + 1.228*t + 0.0978*t^2 - 0.00571*t^3}
m45_64_3 <- function(t) {1.287 - 2.572*t + 1.432*t^2 - 0.0804*t^3}
m65plus_3 <- function(t) {16.61 - 38.65*t + 13.62*t^2 - 0.682*t^3}

# Make plot.
xlimits <- c(0,6)
ylimits <- c(-10,150)
plot(mall3, xlim=xlimits, ylim=ylimits, main="Mortality (Hsiang, 2017)", col="blue", lwd=3, xlab="Change in GMST (degC)", ylab="Deaths per 100,000 Population")
par(new=TRUE)
plot(minfant3, xlim=xlimits, ylim=ylimits, col="red", lwd=3, xlab="Change in GMST (degC)", ylab="Deaths per 100,000 Population")
par(new=TRUE)
plot(m1_44_3, xlim=xlimits, ylim=ylimits, col="orange", lwd=3, xlab="Change in GMST (degC)", ylab="Deaths per 100,000 Population")
par(new=TRUE)
plot(m45_64_3, xlim=xlimits, ylim=ylimits, col="green", lwd=3, xlab="Change in GMST (degC)", ylab="Deaths per 100,000 Population")
par(new=TRUE)
plot(m65plus_3, xlim=xlimits, ylim=ylimits, col="black", lwd=3, xlab="Change in GMST (degC)", ylab="Deaths per 100,000 Population")
par(new=TRUE)
legend("topleft", c("All ages", "Infants", "Ages 1-44", "Ages 45-64", "Ages 65+"), fill=c("blue","red","orange","green","black"))
