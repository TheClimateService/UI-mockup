

# HISTORICAL EXTREME WATER LEVELS (NOAA TECHNICAL REPORT 073)

# Read location ids, names, and GEV parameters
ewl = read.table("./data/sealevel_us/NOAA_Technical_Report_NOS_COOPS_067a_appI_gev_highwater.csv", header=TRUE)

# Get the location names as strings.
noaa_extremes_locations = as.character(ewl[,2])

# PROJECTED LOCAL SEA LEVEL

# Read location names, PSMSL ids, lat, lon, scenario, background RSL rate, and RSL by period.
# Site,PSMSL ID,Latitude,Longitude,Scenario,Background RSL rate (mm/yr),RSL in 2000 (cm),RSL in 2010 (cm),RSL in 2020 (cm),RSL in 2030 (cm),RSL in 2040 (cm),RSL in 2050 (cm),RSL in 2060 (cm),RSL in 2070 (cm),RSL in 2080 (cm),RSL in 2090 (cm),RSL in 2100 (cm),RSL in 2120 (cm),RSL in 2150 (cm),RSL in 2200 (cm)
proj = read.table("./data/sealevel_us/noaa_techrpt83_data.csv.4r_nogridpts", header=TRUE)

# Get the location names as strings.
noaa_slr_locations = as.character(proj[,1])

# Plotting code (done in server.R).
#test_profile = filter(proj,Site=="GMSL") %>% filter(Scenario=="1.0_-_MED") %>% select(7:17)
#z = t(test_profile)
#plot(z, xlab="year", ylab="Relative Local Sea Level Rise (cm)", xaxt="n")
#axis(1, at=c(1:11), labels=c("2000","2010","2020","2030","2040","2050","2060","2070","2080","2090","2100"))


