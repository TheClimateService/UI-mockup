
# This code is the same as plot_sealevel_data_world_ewl_slr.r, but with the plots removed.

# This prepares the plot of EWL with SLR for each scenario using worldwide data and also converts this to probabilities.  The type of plot depends on input$checkbox_showRLvRP (see ui.R).

# Return periods from the EWL data in muis_global_storm_surge_reanalysis.
	rphist <- c(2,5,10,25,50,100,250,500,1000)

	# Prepare data for plot of RL versus RP.
	rtnlevels_m_hist = filter(world_ewl_with_slr_stations,CLSFID_LONGI_LATI_station==loc) %>% select(7:15)
	slr_cm_rcp85 = t( filter(world_ewl_with_slr_stations,CLSFID_LONGI_LATI_station==loc) %>% select(26:28) )
	slr_cm_rcp45 = t( filter(world_ewl_with_slr_stations,CLSFID_LONGI_LATI_station==loc) %>% select(29:31) )
	slr_cm_rcp26 = t( filter(world_ewl_with_slr_stations,CLSFID_LONGI_LATI_station==loc) %>% select(32:34) )
	if(scenario=="RCP8.5") {inc1=0.01*as.numeric(slr_cm_rcp85[1]); inc2=0.01*as.numeric(slr_cm_rcp85[2]); inc3=0.01*as.numeric(slr_cm_rcp85[3]) }
	if(scenario=="RCP4.5") {inc1=0.01*as.numeric(slr_cm_rcp45[1]); inc2=0.01*as.numeric(slr_cm_rcp45[2]); inc3=0.01*as.numeric(slr_cm_rcp45[3]) }
	if(scenario=="RCP2.6") {inc1=0.01*as.numeric(slr_cm_rcp26[1]); inc2=0.01*as.numeric(slr_cm_rcp26[2]); inc3=0.01*as.numeric(slr_cm_rcp26[3]) }
	rtnlevels_m_rcp85_2030 = rtnlevels_m_hist + inc1
	rtnlevels_m_rcp85_2050 = rtnlevels_m_hist + inc2
	rtnlevels_m_rcp85_2100 = rtnlevels_m_hist + inc3
	z = t(rtnlevels_m_hist)
	z2 = t(rtnlevels_m_rcp85_2030)
	z3 = t(rtnlevels_m_rcp85_2050)
	z4 = t(rtnlevels_m_rcp85_2100)
	y1 = min(z,z2,z3,z4)
	y2 = max(z,z2,z3,z4)

	# Prepare data for plot of probability versus return level for each time period, as defined for z RL values above.
	rlmodel <- lm(z ~ poly(log(rphist,10),2,raw=TRUE))
	rlmodel2 <- lm(z2 ~ poly(log(rphist,10),2,raw=TRUE))
	rlmodel3 <- lm(z3 ~ poly(log(rphist,10),2,raw=TRUE))
	rlmodel4 <- lm(z4 ~ poly(log(rphist,10),2,raw=TRUE))
	f4rlmodel = function(t) {summary(rlmodel)$coefficients[1,1] + summary(rlmodel)$coefficients[2,1]*t + summary(rlmodel)$coefficients[3,1]*t^2  }
	f4rlmodel2 = function(t) {summary(rlmodel2)$coefficients[1,1] + summary(rlmodel2)$coefficients[2,1]*t + summary(rlmodel2)$coefficients[3,1]*t^2  }
	f4rlmodel3 = function(t) {summary(rlmodel3)$coefficients[1,1] + summary(rlmodel3)$coefficients[2,1]*t + summary(rlmodel3)$coefficients[3,1]*t^2  }
	f4rlmodel4 = function(t) {summary(rlmodel4)$coefficients[1,1] + summary(rlmodel4)$coefficients[2,1]*t + summary(rlmodel4)$coefficients[3,1]*t^2  }

	prob = seq(0.995, 0.001, -0.02)
	#p2 <- seq(2,0.2,-0.1)
	#p3 <- seq(0.19, 0.0001, -0.001)
	#prob <- c(p2,p3)
	#prob = seq(2, 0.0001, -0.001)
	rp = 1/prob
	oneminusprob = 1-prob
	rlvals = f4rlmodel(log(rp,10))
	rlvals2 = f4rlmodel2(log(rp,10))
	rlvals3 = f4rlmodel3(log(rp,10))
	rlvals4 = f4rlmodel4(log(rp,10))
	#plot(rlvals, 1-prob)

	ndeltas <- length(rlvals)-1
	rlvalsdelta <- matrix(0, ncol=ndeltas)
	rlvalsdelta2 <- rlvalsdelta
	rlvalsdelta3 <- rlvalsdelta
	rlvalsdelta4 <- rlvalsdelta
	oneminusprobdelta <- rlvalsdelta

	for(i in 1:ndeltas) rlvalsdelta[1,i] = rlvals[i+1] - rlvals[i]
	for(i in 1:ndeltas) rlvalsdelta2[1,i] = rlvals2[i+1] - rlvals2[i]
	for(i in 1:ndeltas) rlvalsdelta3[1,i] = rlvals3[i+1] - rlvals3[i]
	for(i in 1:ndeltas) rlvalsdelta4[1,i] = rlvals4[i+1] - rlvals4[i]

	for(i in 1:ndeltas) oneminusprobdelta[1,i] = oneminusprob[i+1] - oneminusprob[i]
	gradients = oneminusprobdelta/rlvalsdelta
	gradients2 = oneminusprobdelta/rlvalsdelta2
	gradients3 = oneminusprobdelta/rlvalsdelta3
	gradients4 = oneminusprobdelta/rlvalsdelta4

	shift_rlvals <- rlvalsdelta
	shift_rlvals2 <- rlvalsdelta
	shift_rlvals3 <- rlvalsdelta
	shift_rlvals4 <- rlvalsdelta
	for(i in 1:ndeltas) shift_rlvals[1,i] = rlvals[i+1]
	for(i in 1:ndeltas) shift_rlvals2[1,i] = rlvals2[i+1]
	for(i in 1:ndeltas) shift_rlvals3[1,i] = rlvals3[i+1]
	for(i in 1:ndeltas) shift_rlvals4[1,i] = rlvals4[i+1]

	# Fit a function to the probability versus rl data.
	# This is not plotted here, but is used in the loss-curve calculation in server.R.
	# Note use of transpose below.
	p <- t(gradients/sum(gradients))
	rldistmodel <- lm(p ~ poly(t(shift_rlvals), 4, raw=TRUE))
	f4rldistmodel = function(t) {summary(rldistmodel)$coefficients[1,1] + summary(rldistmodel)$coefficients[2,1]*t + summary(rldistmodel)$coefficients[3,1]*t^2 + summary(rldistmodel)$coefficients[4,1]*t^3 + summary(rldistmodel)$coefficients[5,1]*t^4 }


      # Use the following two lines for testing outside the app.
      # plot_rl_rp <- "yes"
      # if(plot_rl_rp=="yes") {

      if(input$checkbox_showRLvRP=="TRUE") {
	# Plot return levels versus return periods for each time period.
	plot(z, type="l",lwd=3,col="black", xlab="Return Period (years)", ylab="Return Level (m)", xaxt="n",ylim=c(y1,y2))
	lines(z2, lwd=3, col="yellow")
	lines(z3, lwd=3, col="orange")
	lines(z4, lwd=3, col="red")
	axis(1, at=c(1:9), labels=c("2","5","10","25","50","100","250","500","1000"))
     	legend("topleft", inset=.05, title="Periods",legend=c("Historical","2030","2050","2100"), lwd=3, col=c("black","yellow","orange","red"))
      } else {

	# Plot probability versus return level for each time period, as defined for z RL values above.
	# y1 and y2 are the lower and upper values of the return levels, defined above.
	plot(shift_rlvals, gradients/sum(gradients), type="l", lwd=3,col="black", xlab="Return Level (m)", ylab="Probability", xlim=c(y1,y2))
	lines(shift_rlvals2, gradients2/sum(gradients2), lwd=3, col="yellow")
	lines(shift_rlvals3, gradients3/sum(gradients3), lwd=3, col="orange")
	lines(shift_rlvals4, gradients4/sum(gradients4), lwd=3, col="red")
     	legend("topright", inset=.05, title="Periods",legend=c("Historical","2030","2050","2100"), lwd=3, col=c("black","yellow","orange","red"))
     } # endif on plot_rl_rp or input$checkbox_showRLvRP


