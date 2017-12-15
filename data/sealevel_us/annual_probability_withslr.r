
slrScenarios = c("0.3_-_MED","0.5_-_MED","1.0_-_MED","1.5_-_MED","2.0_-_MED","2.5_-_MED")
slrYears = c(2020,2030,2040,2050,2060,2070,2080,2090,2100)
#slr_scenario_year = rep(0, length(slrScenarios)*length(slrYears) ) 
slr_scenario_year = matrix(0, nrow=length(slrScenarios), ncol=length(slrYears) ) 
annual_probability_withslr = slr_scenario_year

	location_parameters = filter(ewl,name==input$extremewaterLocation) %>% select(3:8)
      # z contains the location, scale, and shape parameters in rows 1, 3, and 5 of column 1.
      # z contains the +/-95% confidence interval of these parameters in rows 2, 4, and 6 of column 1.
	z = t(location_parameters)
	loc = z[1,1]
	sc = z[3,1]
	sh = z[5,1]

# Return level of concern in meters.
	return_level = as.numeric(input$returnLevel)
	#return_level = 2
	#return_levels = c(1,2,3,4,5,6,7,8,9,10)  # units = meters
	# XXX need to handle sh==0
	if(sh != 0) {return_period = ((sh/sc)*(return_level-loc) + 1)^(1/sh);
		    a = 1/(1+exp(1/return_period)) }

# Build matrix of annual probabilities as function of slr scenario (rows) and period (columns).
for(i in 1:length(slrScenarios)) { 
   for(j in 1:length(slrYears)) {
	column=j+8;
	#slr_scenario_year[i,j] = filter(proj,Site==input$sealevelProjectionLocation) %>% filter(Scenario==slrScenarios[i]) %>% select(column) 
	temp = filter(proj,Site==input$sealevelProjectionLocation) %>% filter(Scenario==slrScenarios[i]) %>% select(column) 
	slr_scenario_year[i,j] = temp[1,]

	# Local slr change is in centimeters.
	return_level_withslr = return_level - slr_scenario_year[i,j]/100
	# XXX need to handle sh==0
	if(sh != 0) {return_period_withslr = ((sh/sc)*(return_level_withslr-loc) + 1)^(1/sh) }
	annual_probability = round(100*1/return_period, digits=2)
	annual_probability_withslr[i,j] = min(100, round(100*1/return_period_withslr, digits=2))

   } #endif j 
} #endif i

# Save results as matrix.
write.table(annual_probability_withslr,"./output/output_flood_annual_prob.csv", row.names=slrScenarios, col.names=slrYears)
