
library(tidyverse)

# Specified return periods in the GTSR data.
rphist = c(2,5,10,25,50,100,250,500,1000)

# Read table of location information and associated historical return levels.
# When slr stations have been associated with each gtsr segment, this file contains local slr projections in the last 9 fields.
# NOTE THAT THE FORMAT BELOW WILL CHANGE AS ADDITIONAL TYPES OF DAMAGE FUNCTIONS ARE ADDED TO THE FACILITIES LOCATION FILE.  HOWEVER, THE R CODE BELOW IS READING THE RL DATA BY COLUMN NAME AND SHOULD NOT BE AFFECTED BY SUCH CHANGES.
# facility lon lat location df_tx90p df_pdsisc df_coastalflood modifications	mindistkm mindistid mindistid2 nearestseglon nearestseglat RLm2yr RLm5yr RLm10yr RLm25yr RLm50yr RLm100yr RLm250yr RLm500yr RLm1000yr stationdistkm station id lon lat background_rate rcp85_2030cm rcp85_2050cm rcp85_2100cm rcp45_2030cm rcp45_2050cm rcp45_2100cm rcp26_2030cm rcp26_2050cm rcp26_2100cm
# Micron_Boise -116.149136 43.529396 Boise_Idaho tx90p-1 pdsisc-1 coastalflood-1 -	643.327 3556 3556 -124.047 44.646 TooFarFromCoast_threshold_100km - - - - - - - - - - - - - - - - - - - - - - -
# Micron_Manassas -77.503845 38.750829 Manassas_Virginia tx90p-2 pdsisc-2 coastalflood-2 -	28.0292 4315 4315 -77.24 38.606 0.6275800178 0.7106921468 0.765719608 0.8352469511 0.8868263061 0.9380248071 1.0054360032 1.0563367272 1.1072006493 35.0505 WASHINGTON_DC 360 -77.02 38.87 Bkgd:_1.36_+/-_0.29_mm/y 20 38 93 20 35 74 20 34 62

#locfile="./facility_locations.csv.testNA.nearest.gtsr.segment"
locfile="./facility_locations_v2.csv.nearest.gtsr.segment"
data = read.table(locfile, header=TRUE)
nlocations = length(readLines(locfile)) - 1
#rldata = as.numeric( t( select(data,RLm100yr:RLm1000yr) ) )
rldata = data.frame( t( select(data,RLm2yr:RLm1000yr) ) )
slrdata = data.frame( t( select(data,rcp85_2030cm:rcp26_2100cm) ) )

# ------------------------
# Test for single location.
# ------------------------
test=0
if(test==1) {
rlhist = c(0.6275800178, 0.7106921468, 0.765719608, 0.8352469511, 0.8868263061, 0.9380248071, 1.0054360032, 1.0563367272, 1.1072006493)
# Select the 100-year historical rl data.
rlhist100yr = as.numeric( t( select(data,RLm100yr) ) )
rlfut100yr = rlhist100yr
for(i in 1:length(rlhist100yr)) {  
    if(rlhist[i]=="NA") {rlfut100yr[i]=rlhist100yr[i]} 
       else {rlfut100yr[i]=2*rlhist100yr[i]}
    } #endfor
# Assume rl/rp curve shifts upward by amount of local SLR.
change_local_slr_meters = 0.1
rlfut=rlhist + change_local_slr_meters
# Develop interpolation function for future rl values as function of historical rp values.
h = approxfun(rlfut,log(rphist,10))
rpfut = 10^h(rlhist)
# Find the future rp for the historical 100-year rl value; use this as risk indicator.
rp_fut_level_that_was_historical_100yr = rpfut[6]
annprob_fut_level_that_was_historical_100yr = 1/rpfut[6]
} #endif on test

# ------------------------
# Apply to all facility locations in input file.
# ------------------------
# NOTE THAT NUMBER OF RETURN PERIODS IS SET BELOW.
nrtnperiods=9
#nyears=10
nyears=9

# rlfutloc contains, for a given year/decade, slr-shifted rl values for each rp (columns), at each location (rows).
rlfutloc = matrix(0, nrow=nlocations, ncol=nrtnperiods)

# rlfutloc2 contains slr-shifted rl values for each rp (columns), at each location (rows), for each year/decade (slabs).  The slab for a given year is rlfutloc2[,,k].
rlfutloc2 = array(0, c(nlocations, nrtnperiods, nyears) )

# rpfutloc contains the future rp of the historical 100yr rl for each original rphist value (columns), at each location (rows)
rpfutloc = matrix(0, nrow=nlocations, ncol=nrtnperiods)

# rpfutloc2 contains future rp values for each historical rl associated with the rphist values (columns), at each location (rows), for each year/decade (slabs).  The slab for a given year is rpfutloc2[,,k].  The evolution through time of the rp for the rl that had the jth original rp at location i is rpfutloc2[i,j,].  The table of such data for all locations is rpfutloc[,j,].
rpfutloc2 = rlfutloc2

slryrs = c(2030,2050,2100)

for(i in 1:nlocations) {  
    x = select(data[i,], RLm2yr:RLm1000yr)
    x2 = as.numeric(t(x))
    slr = select(data[i,], rcp85_2030cm:rcp85_2100cm)
    slr2 = as.numeric(t(slr))
    if(toString(x2[1])!="NA") {
	model <- lm(slr2 ~ poly(slryrs,2,raw=TRUE))
    	f4slr = function(t) {summary(model)$coefficients[1,1] + summary(model)$coefficients[2,1]*t + summary(model)$coefficients[3,1]*t^2  }
    	#change_local_slr_cm = c(f4slr(2010),f4slr(2020),f4slr(2030),f4slr(2040),f4slr(2050),f4slr(2060),f4slr(2070),f4slr(2080),f4slr(2090),f4slr(2100))
    	change_local_slr_cm = c(f4slr(2010),f4slr(2020),f4slr(2030),f4slr(2040),f4slr(2050),f4slr(2060),f4slr(2070),f4slr(2080),f4slr(2090) )
    	change_local_slr_meters = change_local_slr_cm/100 
       } #endif

    for(k in 1:length(change_local_slr_meters)) {
     for(j in 1:nrtnperiods) {
       if(toString(x2[j])=="NA") {rlfutloc2[i,j,k]=x2[j]} 
         else {rlfutloc2[i,j,k]=x2[j]+change_local_slr_meters[k] }
       } #endfor on j
     if(toString(x2[1])!="NA") { 
       h = approxfun(rlfutloc2[i,,k],log(rphist,10))
       #rpfutloc2[i,,k] = 10^h(rlhist)
       if(toString(x2[1])!="NA") rpfutloc2[i,,k] = 10^h(x2) else rpfutloc2[i,,k] = "NA"
       } #endif
     } #endfor on k
    
#    if(toString(x2[1])!="NA") { 
#       h = approxfun(rlfutloc2[i,,1],log(rphist,10))
#       #h = approxfun(log(rphist,10),rlfutloc[i,])
#       rpfutloc[i,] = 10^h(rlhist)
#       } #endif

    } #endfor on i

# Save results for selected original return period (e.g., 100yr) in local directory as both rp values and annual probabilities.
# Original return periods are defined above as rphist.  100yr rp corresponds to index j=6.
# When rp values written out are all zeros, this means that the location was too far from the coastal segment, depending on the threshold set for this in script_get_nearest_segment_v1.
# When rp values written out are "NA", this means that the future rp is less than 2 years (i.e., the lowest rp in rphist).

write.table(rpfutloc2[,6,],"./future_rp_fromR", sep=" ", row.names=FALSE, 
   col.names=c("RPhist100_rcp85_2010","RPhist100_rcp85_2020","RPhist100_rcp85_2030","RPhist100_rcp85_2040","RPhist100_rcp85_2050","RPhist100_rcp85_2060","RPhist100_rcp85_2070","RPhist100_rcp85_2080","RPhist100_rcp85_2090") )

write.table(1/rpfutloc2[,6,],"./future_annprob_fromR", sep=" ", row.names=FALSE, 
   col.names=c("annPhist100_rcp85_2010","annPhist100_rcp85_2020","annPhist100_rcp85_2030","annPhist100_rcp85_2040","annPhist100_rcp85_2050","annPhist100_rcp85_2060","annPhist100_rcp85_2070","annPhist100_rcp85_2080","annPhist100_rcp85_2090") )


