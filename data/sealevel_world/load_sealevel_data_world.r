

# HISTORICAL EXTREME WATER LEVELS (GTSR DATA)

# Read location ids, names, and GEV parameters
#world_ewl = read.table("./data/sealevel_world/GTSR.dbf.csv.cln", header=TRUE)
world_ewl = read.table("./data/scoring_engine/coastalflooding/GTSR.dbf.csv.cln.reordered", header=TRUE)

# Get the location names as strings.
#world_extremes_locations = as.character(paste(world_ewl[,3],"_",world_ewl[,8],"_",world_ewl[,6]))
world_extremes_locations = as.character(world_ewl[,1])

# PROJECTED LOCAL SEA LEVEL (KOPP, 2014, DATA)

# Read data.
# Site ID Long Lat        station id background_rate rcp85_2030cm rcp85_2050cm rcp85_2100cm rcp45_2030cm rcp45_2050cm rcp45_2100cm rcp26_2030cm rcp26_2050cm rcp26_2100cm
# REYKJAVIK 638 -21.94 64.15      REYKJAVIK 638 Bkgd:_-0.33_+/-_0.96_mm/y 4 7 11 5 7 12 4 7 8
#world_slr = read.table("./data/scoring_engine/coastalflooding/kopp_2014_eft237-sup-0006-Table07.tsv.extracted.with.lonlat", header=TRUE)
world_slr = read.table("./data/scoring_engine/coastalflooding/kopp_2014_eft237-sup-0006-Table07.tsv.extracted.with.lonlat.nodupnames.sorted", header=TRUE)

# Get the location names as strings.
#world_slr_locations = as.character(paste(world_slr[,1],"_",world_slr[,3],"_",world_slr[,4]))
world_slr_locations = as.character(world_slr[,1])

# MERGED GTSR DATA WITH NEAREST STATION DATA FROM KOPP

world_ewl_with_slr_stations = read.table("./data/scoring_engine/coastalflooding/GTSR.dbf.csv.nearest.slr.station.cln.reordered", header=TRUE)
#world_extremes_locations_with_slr_stations = as.character(paste(world_ewl_with_slr_stations[,3],"_",world_ewl_with_slr_stations[,8],"_",world_ewl_with_slr_stations[,6],"_",world_ewl_with_slr_stations[,21]))
world_extremes_locations_with_slr_stations = as.character(world_ewl_with_slr_stations[,1])


# Plotting code (done in server.R).
#test_profile = filter(proj,Site=="GMSL") %>% filter(Scenario=="1.0_-_MED") %>% select(7:17)
#z = t(test_profile)
#plot(z, xlab="year", ylab="Relative Local Sea Level Rise (cm)", xaxt="n")
#axis(1, at=c(1:11), labels=c("2000","2010","2020","2030","2040","2050","2060","2070","2080","2090","2100"))


