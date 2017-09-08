library(fitdistrplus)

fit_distributions <- function(variable) {

#	fitg <- fitdist(variable, "gamma")
#	fitln <- fitdist(variable, "lnorm")
#	plotfitg <- plot(fitg)
#	plotfitln <- plot(fitln)

	 fitW <- fitdist(variable, "weibull")
	 plotfitW <- plot(fitW)
	 distname <- as.character(fitW$distname)
	 estimate <- as.numeric(fitW$estimate)
	 sd <- as.numeric(fitW$sd)
         outlist <- list("plotfitW"=plotfitW, "distname"=distname, "estimate"=estimate, "sd"=sd)

         return(outlist)
         }

