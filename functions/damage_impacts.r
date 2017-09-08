
# Rows in the impacts and damage matrices correspond to the climate-variable values in the intervals defined by thresholds, while columns correspond to the periods covered.

# thresholds has length corresponding to the variable intervals, while shapes and scales have lengths corresponding to periods.
# Examples:  thresholds <- c(285,290,295,300,305,310)
#	     periods <- c("1976-05", "2016-25", "2026-35", "2036-45", "2046-55", "2056-65", "2066-75", "2076-85", "2086-95")
#	     shapes <- c(81,82,83,84,85,86,87,88,89)
#	     scales <- c(292,293,294,295,296,297,298,299,300)

# Impacts are the product of the probability that the climate variable will lie in a certain interval and the damage corresponding to the midpoint of the climate variable in that interval.
# impactbyperiod is, for each period, the sum across the impacts in each climate-variable interval defined by thresholds.

      # wt1 is sigmoid weight; wt2 is quadratic weight.
        wt1 = input$impactfunctionweight
        wt2 = 1 - wt1

	damage = matrix(1,length(thresholds)-1,length(periods))
	for(j in 1:length(thresholds)-1) { for(i in 1:length(periods)) {
         damagej1 = wt1*sigmoid(thresholds[j],input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(thresholds[j], input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint)
         damagej2 = wt1*sigmoid(thresholds[j+1],input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(thresholds[j+1],input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint)
         damage[j,i] <- 0.5*(damagej2 + damagej1)
        } }

	# pweibull is the CDF for dweibull.
	impacts = matrix(0,length(thresholds)-1,length(periods))
	for(j in 1:length(thresholds)-1) { for(i in 1:length(periods)) {
        	impacts[j,i] <- damage[j,i]*( pweibull(thresholds[j+1],shapes[i],scales[i]) - pweibull(thresholds[j],shapes[i],scales[i])  ) } }

    	impactbyperiod <- c(0,0,0,0,0,0,0,0,0)
    	for(i in 1:length(impactbyperiod)) {impactbyperiod[i]=sum(impacts[,i])  }
    	impactbyperiod_relative2baseperiod = impactbyperiod - impactbyperiod[1]

