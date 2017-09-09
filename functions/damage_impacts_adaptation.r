
# Rows in the impacts and damage matrices correspond to the climate-variable values in the intervals defined by thresholds, while columns correspond to the periods covered.

# thresholds has length corresponding to the variable intervals, while shapes and scales have lengths corresponding to periods.
# Examples:  thresholds <- c(285,290,295,300,305,310)
#	     periods <- c("1976-05", "2016-25", "2026-35", "2036-45", "2046-55", "2056-65", "2066-75", "2076-85", "2086-95")
#	     shapes <- c(81,82,83,84,85,86,87,88,89)
#	     scales <- c(292,293,294,295,296,297,298,299,300)

# Impacts are the product of the probability that the climate variable will lie in a certain interval and the damage corresponding to the midpoint of the climate variable in that interval.
# impactbyperiod is, for each period, the sum across the impacts in each climate-variable interval defined by thresholds.

# wt1 is sigmoid weight; wt2 is quadratic weight.

  # Asset risk below is sum of values by asset, weighted by sensitivity to given impact function.  Sensitivity varies with adaptation plan.
  #   Impact functions give percent impact, so we allow different assets to have different sensitivities to a given impact function.
  #   This is a simplified way to get different impacts by scaling by asset type; ideally, they would have different impact functions. 
  #   This is also a simplified (linear) way to connect asset value with loss of capacity or condition in the impact function.

  #    assetrisk = (input$assetvalue1*input$assetsensitivity1) + (input$assetvalue2*input$assetsensitivity2) + (input$assetvalue3*input$assetsensitivity3)
    assetrisk1 = (input$assetvalue1*input$assetsensitivity1)
    assetrisk2 = (input$assetvalue2*input$assetsensitivity2)
    assetrisk3 = (input$assetvalue3*input$assetsensitivity3)
    assetrisk1_plan1 = (input$assetvalue1*input$assetsensitivity1_plan1)
    assetrisk2_plan1 = (input$assetvalue2*input$assetsensitivity2_plan1)
    assetrisk3_plan1 = (input$assetvalue3*input$assetsensitivity3_plan1)
    assetrisk1_plan2 = (input$assetvalue1*input$assetsensitivity1_plan2)
    assetrisk2_plan2 = (input$assetvalue2*input$assetsensitivity2_plan2)
    assetrisk3_plan2 = (input$assetvalue3*input$assetsensitivity3_plan2)

        wt1 = input$impactfunctionweight
        wt2 = 1 - wt1

	damage = matrix(1,length(thresholds)-1,length(periods))
	damage_plan1 = damage
        damage_plan2 = damage
	for(j in 1:length(thresholds)-1) { for(i in 1:length(periods)) {
         damagej1 = wt1*sigmoid(thresholds[j],input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(thresholds[j], input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint)
         damagej2 = wt1*sigmoid(thresholds[j+1],input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(thresholds[j+1],input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint)

	 damageji1 <- max( -input$assetvalue1, 0.01 * assetrisk1 * 0.5*(damagej2 + damagej1) )
         damageji2 <- max( -input$assetvalue2, 0.01 * assetrisk2 * 0.5*(damagej2 + damagej1) )
         damageji3 <- max( -input$assetvalue3, 0.01 * assetrisk3 * 0.5*(damagej2 + damagej1) )
         damage[j,i] <- damageji1 + damageji2 + damageji3

         damageji1_plan1 <- max( -input$assetvalue1, 0.01 * assetrisk1_plan1 * 0.5*(damagej2 + damagej1) )
         damageji2_plan1 <- max( -input$assetvalue2, 0.01 * assetrisk2_plan1 * 0.5*(damagej2 + damagej1) )
         damageji3_plan1 <- max( -input$assetvalue3, 0.01 * assetrisk3_plan1 * 0.5*(damagej2 + damagej1) )
         damage_plan1[j,i] <- damageji1_plan1 + damageji2_plan1 + damageji3_plan1

         damageji1_plan2 <- max( -input$assetvalue1, 0.01 * assetrisk1_plan2 * 0.5*(damagej2 + damagej1) )
         damageji2_plan2 <- max( -input$assetvalue2, 0.01 * assetrisk2_plan2 * 0.5*(damagej2 + damagej1) )
         damageji3_plan2 <- max( -input$assetvalue3, 0.01 * assetrisk3_plan2 * 0.5*(damagej2 + damagej1) )
         damage_plan2[j,i] <- damageji1_plan2 + damageji2_plan2 + damageji3_plan2

        } }

	# impactbyperiod is, for each period, the sum across the impacts in each climate-variable interval defined by thresholds.
	# pweibull is the CDF for dweibull.
	impacts = matrix(0,length(thresholds)-1,length(periods))
	impacts_plan1 = impacts
        impacts_plan2 = impacts
	for(j in 1:length(thresholds)-1) { for(i in 1:length(periods)) {
        	impacts[j,i] <- damage[j,i]*( pweibull(thresholds[j+1],shapes[i],scales[i]) - pweibull(thresholds[j],shapes[i],scales[i])  ) 
		impacts_plan1[j,i] <- damage_plan1[j,i]*( pweibull(thresholds[j+1],shapes[i],scales[i]) - pweibull(thresholds[j],shapes[i],scales[i]) )
        	impacts_plan2[j,i] <- damage_plan2[j,i]*( pweibull(thresholds[j+1],shapes[i],scales[i]) - pweibull(thresholds[j],shapes[i],scales[i]) )
        } }

    	impactbyperiod <- c(0,0,0,0,0,0,0,0,0)
	impactbyperiod_plan1 = impactbyperiod
    	impactbyperiod_plan2 = impactbyperiod

	for(i in 1:length(impactbyperiod)) {
	    impactbyperiod[i]=sum(impacts[,i])
            impactbyperiod_plan1[i]=sum(impacts_plan1[,i])
            impactbyperiod_plan2[i]=sum(impacts_plan2[,i])
        }

    	impactbyperiod_relative2baseperiod = impactbyperiod - impactbyperiod[1]
	impactbyperiod_relative2baseperiod_plan1 = impactbyperiod_plan1 - impactbyperiod_plan1[1]
    	impactbyperiod_relative2baseperiod_plan2 = impactbyperiod_plan2 - impactbyperiod_plan2[1]

	impactbyperiod_relative2noadaptation_plan1 = impactbyperiod_plan1 - impactbyperiod
	impactbyperiod_relative2noadaptation_plan2 = impactbyperiod_plan2 - impactbyperiod


