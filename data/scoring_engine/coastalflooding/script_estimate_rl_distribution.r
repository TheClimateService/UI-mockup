
# SCRIPT TO DEVELOP AND TEST PROBABILITY DISTRIBUTIONS FROM RP/RL DATA.
# TT - March 2018

source("./data/sealevel_world/load_sealevel_data_world.r")

scenario="RCP8.5"
loc <- "5105_-122.645_75.918_RESOLUTE"
loc <- "10244_103.919_1.401_SEMBAWANG"
source("./data/sealevel_world/plot_sealevel_data_world_ewl_slr.r")

# Return periods from the EWL data in muis_global_storm_surge_reanalysis are defined in source above.
#rphist <- c(2,5,10,25,50,100,250,500,1000)

rlmodel <- lm(z ~ poly(log(rphist,10),2,raw=TRUE))
f4rlmodel = function(t) {summary(rlmodel)$coefficients[1,1] + summary(rlmodel)$coefficients[2,1]*t + summary(rlmodel)$coefficients[3,1]*t^2  }

prob = seq(0.995, 0.001, -0.02)
rp = 1/prob
rlvals = f4rlmodel(log(rp,10))
plot(rlvals, 1-prob)
oneminusprob = 1-prob

ndeltas <- length(rlvals)-1
rlvalsdelta <- matrix(0, ncol=ndeltas)
oneminusprobdelta <- rlvalsdelta
for(i in 1:ndeltas) rlvalsdelta[1,i] = rlvals[i+1] - rlvals[i]
for(i in 1:ndeltas) oneminusprobdelta[1,i] = oneminusprob[i+1] - oneminusprob[i]
gradient = oneminusprobdelta/rlvalsdelta

rlvals2 <- rlvalsdelta
for(i in 1:length(rlvals)-1) rlvals2[1,i] = rlvals[i+1]
#plot(rlvals2, gradient/sum(gradient), type="l", xlab="Return Level (m)", ylab="Probability")
plot(rlvals2, gradient/sum(gradient), xlab="Return Level (m)", ylab="Probability")

# Fit a function to the probability versus rl data.
# Note use of transpose below.
p <- t(gradient/sum(gradient))
rldistmodel <- lm(p ~ poly(t(rlvals2), 4, raw=TRUE))
f4rldistmodel = function(t) {summary(rldistmodel)$coefficients[1,1] + summary(rldistmodel)$coefficients[2,1]*t + summary(rldistmodel)$coefficients[3,1]*t^2 + summary(rldistmodel)$coefficients[4,1]*t^3 + summary(rldistmodel)$coefficients[5,1]*t^4 }

# Plot the function for the rl probability distribution and the points from which it was derived.
curve(f4rldistmodel, min(rlvals), max(rlvals))
points(rlvals2, gradient/sum(gradient))
