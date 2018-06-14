
	# facility_locations list is defined by ./data/financial/load_financial_data.r
	# The most up-to-date locations list is created by the last run of the SE and is located at ./data/scoring_engine/TCSDB_structure.locations.csv.  This is accessed by load_financial_data.r.
	fac_selected = facility_locations %>% filter(LocationID_ParentCorpID_LocationName==input_facility)
	lon=as.numeric(fac_selected[1,2])
	lat=as.numeric(fac_selected[1,3])

	#name_pt1 <- fac_selected[1,1]
	# Note that the bylocation file names have been constructed with the ParentCorpID first and the LocationID second.  This is the reverse of the order in fac_selected above.
	loc_parent_name <- fac_selected[1,1]
	s <- unlist( strsplit(as.character(loc_parent_name), "_") )
	# Handle the string elements at s[i>=3] for the location name.
	location <- ""
	for(i in 3:length(s)) {location=paste(location, s[i], sep="_")}
	name_pt1 <- paste(s[2], "_", s[1], location, sep="")

	if(input_scenario=="Historical") name_pt2 <- "historical"
	if(input_scenario=="RCP8.5") name_pt2 <- "rcp85"
	if(input_scenario=="RCP4.5") name_pt2 <- "rcp45"

	name_pt3 <- input_period

	filename <- paste(name_pt1, name_pt2, name_pt3, sep="_")
	dirname <- paste(datadir, name_pt2, name_pt3, sep="/")
	path2file <- paste(dirname, "/", filename, ".fitdata.csv", sep="")

	# Each row in the location-specfic fit-parameter data below consists of location and scenario/period specific fits to the data for the entire year.
	# fitdata.csv format:
	# type p1 p2 aic bic
	# Normal 289.664660039139 10.3369536723689 575593.951809891 575612.44581966
	# Weibull 30.8358299351416 294.674209570452 580682.83092067 580701.324930439
	# Gamma 785.856910528634 2.71298904761156 575490.315117286 575508.809127054
	# Lnorm 5.66808735166462 0.0356793356023292 575468.023427164 575486.517436933
	# Gumbel 284.556175137456 9.24624845215821 579004.316391756 579022.810401525
	d <- read.table(path2file, header=TRUE)

	bestfit <- d %>% filter(d$aic==min(d$aic))
	#if(input_season=="MAM") d <- d %>% select(V69:V160)

	#d <- d - 273.15
	#dt <- t(d)
        #bins <- seq(min(dt), max(dt), length.out = input_bins + 1)

	#hist(dt, breaks=bins, col = 'skyblue', border = 'white', main=paste(filename,"(32 models)"), xlab="Daily Maximum Temperature (degC)", xlim=c(floor(min(dt)), ceiling(max(dt)) ) )

