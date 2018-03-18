
# This prepares an empty plot of EWL with SLR when there is no associated data available.

	plot(1, type="l",lwd=3,col="black", xlab="Return Period (years)", ylab="Return Level (m)", xaxt="n")
	axis(1, at=c(1:9), labels=c("2","5","10","25","50","100","250","500","1000"))
     	legend("center", legend="Does not apply; location not close to coast.")
     	#legend("topleft", inset=.05, title="Periods",legend=c("Historical","2030","2050","2100"), lwd=3, col=c("black","yellow","orange","red"))

