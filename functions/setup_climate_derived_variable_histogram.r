
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

	#filename <- paste(fac_selected[1,1], "_historical_1980s", sep="")
	filename <- paste(name_pt1, name_pt2, name_pt3, sep="_")
	#dirname <- "./data/temperature/nex-gddp/facilities/"
	#dirname <- paste("./data/temperature/nex-gddp/facilities/tasmax", name_pt2, name_pt3, sep="/")
	dirname <- paste(datadir, name_pt2, name_pt3, sep="/")
	path2file <- paste(dirname, "/", filename, ".derived_variables", sep="")

	# Each row in the location-specfic nex-gddp derived-variable data below consists of 9 id/name fields followed by a number of derived-variable values for a single year at a given location from a single model.  The histogram includes values from all rows.
	#d <- read.table("./data/temperature/nex-gddp/facilities/1_1_Boise_historical_1980s", header=FALSE)
	d <- read.table(path2file, header=TRUE)
	# XXX selections below need to be adaptable to changing set of derived variables.
	# XXX initial set of temp variables = max_degC min_degC daysabove_degC_25 daysabove_degC_30 daysabove_degC_35
	d <- d %>% select(max_degC:daysabove_degC_35)  # get all derived variables
	if(input_derived_variable=="Maximum") d <- d %>% select(max_degC)
	if(input_derived_variable=="Minimum") d <- d %>% select(min_degC)
	if(input_derived_variable=="Days Above 25C") d <- d %>% select(daysabove_degC_25)
	if(input_derived_variable=="Days Above 30C") d <- d %>% select(daysabove_degC_30)
	if(input_derived_variable=="Days Above 35C") d <- d %>% select(daysabove_degC_35)

	#d <- d - 273.15
	#dt <- t(d)
        #bins <- seq(min(dt), max(dt), length.out = input_bins + 1)

	#hist(dt, breaks=bins, col = 'skyblue', border = 'white', main=paste(filename,"(32 models)"), xlab="Daily Maximum Temperature (degC)", xlim=c(floor(min(dt)), ceiling(max(dt)) ) )

