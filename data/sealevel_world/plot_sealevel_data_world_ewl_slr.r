
# This prepares the plot of EWL with SLR for each scenario using worldwide data.

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
	plot(z, type="l",lwd=3,col="black", xlab="Return Period (years)", ylab="Return Level (m)", xaxt="n",ylim=c(y1,y2))
	lines(z2, lwd=3, col="yellow")
	lines(z3, lwd=3, col="orange")
	lines(z4, lwd=3, col="red")
	axis(1, at=c(1:9), labels=c("2","5","10","25","50","100","250","500","1000"))
     	legend("topleft", inset=.05, title="Periods",legend=c("Historical","2030","2050","2100"), lwd=3, col=c("black","yellow","orange","red"))

