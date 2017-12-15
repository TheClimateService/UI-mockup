
# FIND GRID POINT NEAREST SPECIFIED LONGITUDE AND LATITUDE

# Processed drought data is read into dataframe d by load_drought_data.r, which is sourced at the beginning of server.R.
#d = read.table("./app_tcs_9/data/drought/pdsisc.monthly.maps.1900-2099.r2.5x2.5.EnsAvg14Models.TP2.ipe=2.nc.output.batch", header=FALSE)

# Longitude and latitude are selected by user input.
#lon=170.0
#lat=76.0

# Find nearest grid point lon and lat.
s = filter(d, d$V1>=lon)
upperlon = min(s$V1)
s = filter(d, d$V1<lon)
lowerlon = max(s$V1)
s = filter(d, d$V2>=lat)
upperlat = min(s$V2)
s = filter(d, d$V2<lat)
lowerlat = max(s$V2)
if(upperlon-lon <= lon-lowerlon) longrid=upperlon else longrid=lowerlon
if(upperlat-lat <= lat-lowerlat) latgrid=upperlat else latgrid=lowerlat

# Get data row corresponding to longrid and latgrid.
d3 = d %>% filter(d$V1==longrid & d$V2==latgrid)
d4 = t(d3)

# Write results to output directory.
droughtPeriods = c("1950-99","2016-25","2026-35","2036-45","2046-55","2056-65","2066-75","2076-85","2086-95")
write.table(d3, "./output/output_drought_annual_prob.csv", col.names=c("lon","lat",droughtPeriods), row.names=c("90th_percentile") )

