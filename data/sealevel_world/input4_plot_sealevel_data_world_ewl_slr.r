
# This generates location and scenario inputs for plot_sealevel_data_world_ewl_slr.r .

        locID <- corpLocations %>% filter(ParentCorpID==USER$ParentCorpID & LocationName==input$inputLocations_overall) %>% select(LocationID)
	key <- paste(locID,USER$ParentCorpID,input$inputLocations_overall)
	key <- gsub(" ","_",key)
	nd = read.table("./data/scoring_engine/coastalflooding/TCSDB_structure.locations.csv.nearest.gtsr.segment", header=TRUE)
	# V17 (RLm2yr) is the first historical return level in the nd table; if the location is outside the coastal distance threshold in the SE, this value will be a string (e.g., "TooFarFromCoast_threshold_10km") rather than a number.
	# Force the elements of RLM2yr that are not numbers to be "NA".
	  nd$RLm2yr <- as.numeric(as.character(nd$RLm2yr))
	#ele = nd %>% filter(nd$V1==key) %>% select(V14:V17, V27)
	ele = nd %>% filter(nd$LocationID_ParentCorpID_LocationName==key) %>% select(mindistid2, nearestseglon, nearestseglat, RLm2yr, station)

	   # key2 is the string that identifies the element in world_ewl_with_slr_stations (created by /data/sealevel_world/load_sealevel_world.r) that has been associated with the current corporate facility.  It consists of of the id of the nearest coastal segment, the segment's lon/lat, and the name of the EWL station associated with that segement.  An example is 3873_-79.472_8.999_BALBOA.
	   #key2 <- paste(ele$V14, ele$V15, ele$V16, ele$V27)
	   key2 <- paste(ele$mindistid2, ele$nearestseglon, ele$nearestseglat, ele$station)
	   key2 <- gsub(" ","_",key2)
	   #loc <- input$extremewaterLocation2_with_slr_station
	   loc <- key2
	   # The following sets the scenario from the backend, not Corporate/Analyze
	   # scenario <- input$world_slr_scenario
	   # The following sets the scenario from Corporate/Analyze, using uiOutput("selectInput_scenario") in ui.R and its definition in server.R.
	   scenario <- input$selectscenario_overall

