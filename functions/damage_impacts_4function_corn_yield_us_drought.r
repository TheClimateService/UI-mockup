
# Rows in the impacts and damage matrices correspond to the climate-variable values in the intervals defined by thresholds, while columns correspond to the periods covered.

# thresholds has length corresponding to the variable intervals, while shapes and scales have lengths corresponding to periods.
# Examples:  thresholds <- c(285,290,295,300,305,310)
#	     periods <- c("1976-05", "2016-25", "2026-35", "2036-45", "2046-55", "2056-65", "2066-75", "2076-85", "2086-95")
#	     shapes <- c(81,82,83,84,85,86,87,88,89)
#	     scales <- c(292,293,294,295,296,297,298,299,300)

# Impacts are the product of the probability that the climate variable will lie in a certain interval and the damage corresponding to the midpoint of the climate variable in that interval.
# impactbyperiod is, for each period, the sum across the impacts in each climate-variable interval defined by thresholds.

# periods are set as a constant in server.R

# Thresholds for hazus flood depth in feet.
thresholds <- c(0,5,10,15,20,25)
	damage = matrix(1,length(thresholds)-1,length(periods))
	for(j in 1:length(thresholds)-1) { for(i in 1:length(periods)) {
         damagej1 = f4damage_flood_depth(thresholds[j])
         damagej2 = f4damage_flood_depth(thresholds[j+1])
         damage[j,i] <- 0.5*(damagej2 + damagej1)
        } }

# server.R reads the table of annual probabilities created by annual_probability_withslr.r.  Note that this is a dynamic table created by the selection of location and return level from the SLR section of the localized climate probabilities tab.  The table has rows for each GMSL scenario and colums for years 2020-2100 in 10-year increments.
# The table is returned by server.R as annual_prob_given_return_level.
# Annual probabilities in the table are in percent.
# XXX For the present, select the GMSL scenario corresponding to +1m by 2100 in row 3.
# XXX Also assume that the return level of concern in the SLR tab has been set to 2m.
ann_prob_selected = as.numeric( annual_prob_given_return_level[3,] ) / 100

	impacts = matrix(0,length(thresholds)-1,length(periods))
	for(j in 1:length(thresholds)-1) { for(i in 1:length(periods)) {
        	#impacts[j,i] <- damage[j,i]*( pweibull(thresholds[j+1],shapes[i],scales[i]) - pweibull(thresholds[j],shapes[i],scales[i])  ) } }
        	#impacts[j,i] <- damage[j,i]*( 1  ) } }
        	impacts[j,i] <- damage[j,i]*( ann_prob_selected[i]  ) } }

    	impactbyperiod <- c(0,0,0,0,0,0,0,0,0)
    	#for(i in 1:length(impactbyperiod)) {impactbyperiod[i]=sum(impacts[,i])  }

# XXX For the present, select the second row of the impacts matrix.  This is appropriate when the return level of concern is set to 2m and the flood-depth thresholds set above have 5-10ft as the second interval.
# XXX This will need to change when the flood depths and return levels are made consistent in units.
    	for(i in 1:length(impactbyperiod)) {impactbyperiod[i]=impacts[2,i]  }
    	impactbyperiod_relative2baseperiod = impactbyperiod - impactbyperiod[1]

# Calculate weighted vector of impact by period for climate score.
weightbyperiod = c(0,80,10,5,1,1,1,1,1)
#weightbyperiod = c(0,100,0,0,0,0,0,0,0)
#score_input_flood = sum( weightbyperiod*impactbyperiod_relative2baseperiod )/100 
percent_change = 100*(impactbyperiod/impactbyperiod[1] - 1.0)
score_input_flood = sum( weightbyperiod*percent_change)/100
write.table(score_input_flood, "./output/score_input_flood.csv", row.names=FALSE, col.names=FALSE)




