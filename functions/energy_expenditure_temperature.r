
# S. Hsiang, R. Kopp, et al, "Estimating Economic Damage in the United States from Climate Change", Science 356, 1362-1369 (2017).  Using table S13 from the supplementary information, quadratic-fit column.  t is change in GMST in degC.  Value returned is change in energy expediture in percent.

enex2 <- function(t) {-1.061 + 2.540*t + 0.412*t^2}
enex3 <- function(t) {0.18 + 0.928*t + 0.923*t^2 - 0.0431*t^3}

# Make plot.
plot(enex3,0,6, lwd=3, main="Energy Expenditures (Hsiang, 2017)", xlab="Change in GMST (degC)", ylab="Change in Energy Expenditures (%)")
par(new=TRUE)

