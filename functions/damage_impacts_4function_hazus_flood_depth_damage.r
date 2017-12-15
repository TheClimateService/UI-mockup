
# Rows in the impacts and damage matrices correspond to the climate-variable values in the intervals defined by thresholds, while columns correspond to the periods covered.

# thresholds has length corresponding to the variable intervals, while shapes and scales have lengths corresponding to periods.
# Examples:  thresholds <- c(285,290,295,300,305,310)
#	     periods <- c("1976-05", "2016-25", "2026-35", "2036-45", "2046-55", "2056-65", "2066-75", "2076-85", "2086-95")
#	     shapes <- c(81,82,83,84,85,86,87,88,89)
#	     scales <- c(292,293,294,295,296,297,298,299,300)

# Impacts are the product of the probability that the climate variable will lie in a certain interval and the damage corresponding to the midpoint of the climate variable in that interval.
# impactbyperiod is, for each period, the sum across the impacts in each climate-variable interval defined by thresholds.

# periods are set as a constant in server.R
	     #periods <- c("1976-05", "2016-25", "2026-35", "2036-45", "2046-55", "2056-65", "2066-75", "2076-85", "2086-95")

# Source the function that builds f4damage_flood_depth.  This is done in server.R.
#source("./data/hazus/function_extract_hazus_flood_depth_damage_return_damage_at_depth.r")
#value = get_hazus_damage_function_return_damage_at_depth(110,20)

# Thresholds for hazus flood depth in feet and meters.
thresholds <- c(0,5,10,15,20,25)
thresholds <- c(0,1.5,3,4.5,6,7.5)

	# get_hazus_damage_function_return_damage_at_depth is defined by ./data/hazus/function_extract_hazus_flood_depth_damage_return_damage_at_depth.r, sourced in server.R.
	damage = matrix(1,length(thresholds)-1,length(periods))
	for(j in 1:length(thresholds)-1) { for(i in 1:length(periods)) {
         #damagej1 = f4damage_flood_depth(thresholds[j])
         #damagej2 = f4damage_flood_depth(thresholds[j+1])
         #damagej1 = get_hazus_damage_function_return_damage_at_depth(damage_function_id,thresholds[j])
         #damagej2 = get_hazus_damage_function_return_damage_at_depth(damage_function_id,thresholds[j+1])
         #damage[j,i] <- 0.5*(damagej2 + damagej1)
	 depth_value = 0.5*(thresholds[j+1] + thresholds[j])
         damage[j,i] = get_hazus_damage_function_return_damage_at_depth(damage_function_id,depth_value)
        } }

# server.R reads the table of annual probabilities created by annual_probability_withslr.r.  Note that this is a dynamic table created by the selection of location and return level from the SLR section of the localized climate probabilities tab.  The table has rows for each GMSL scenario and colums for years 2020-2100 in 10-year increments.
# The table is returned by server.R as annual_prob_given_return_level.
# In server.R, function_annual_probability_withslr.r is called with each of the return levels associated with the threshold-band midpoints and produces on table for each of these levels (e.g., 1-5).  These tables are read back in when damage is to be calculated as annual_prob_given_return_level1, annual_prob_given_return_level2, etc.
# Annual probabilities in the tables are in percent.

# We need this table to be computed across each of the depth values defined by the midpoints of the threshold bands.
# XXX For the present, select the GMSL scenario corresponding to +1m by 2100 in row 3.
# XXX Also assume that the return level of concern in the SLR tab has been set to 2m.

  ann_prob_selected = as.numeric( annual_prob_given_return_level[3,] ) / 100

	impacts = matrix(0,length(thresholds)-1,length(periods))
	for(j in 1:length(thresholds)-1) { for(i in 1:length(periods)) {
        	#impacts[j,i] <- damage[j,i]*( pweibull(thresholds[j+1],shapes[i],scales[i]) - pweibull(thresholds[j],shapes[i],scales[i])  ) } }
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




