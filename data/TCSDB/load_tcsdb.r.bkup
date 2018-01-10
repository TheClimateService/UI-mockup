
library(readxl)

# Specify source.
db="./data/TCSDB/TCSDB_structure.xlsx"
#db="./data/TCSDB/TCSDB_structure_v3.xlsx"

# Read each sheet.  
# Current sheet names:  CorpRiskTable, ValueAtRisk, Location, ParentCorp, TCDFCategory, TCFDSubCat, RiskFactor, Scenarios, CorpRiskTable_withSEoutputs, and Users.
dbsheet1 = read_excel(db,1)
dbsheet2 = read_excel(db,2)
dbsheet3 = read_excel(db,3)
dbsheet4 = read_excel(db,4)
dbsheet5 = read_excel(db,5)
dbsheet6 = read_excel(db,6)
dbsheet7 = read_excel(db,7)
dbsheet8 = read_excel(db,8)
dbsheet9 = read_excel(db,9)
dbsheet10 = read_excel(db,10)

# Create sheet 9 by reading in SE outputs and merging with sheet1.
# Set up decadal periods vice annual in the original sheet1.
dbs2 = dbsheet1 %>% filter(ScenarioName=="RCP8.5" & RiskYear<=2026)
for(i in 1:nrow(dbs2) ) {if(dbs2[i,]$RiskYear==2026) dbs2[i,]$RiskYear=2090 }
for(i in 1:nrow(dbs2) ) {if(dbs2[i,]$RiskYear==2025) dbs2[i,]$RiskYear=2080 }
for(i in 1:nrow(dbs2) ) {if(dbs2[i,]$RiskYear==2024) dbs2[i,]$RiskYear=2070 }
for(i in 1:nrow(dbs2) ) {if(dbs2[i,]$RiskYear==2023) dbs2[i,]$RiskYear=2060 }
for(i in 1:nrow(dbs2) ) {if(dbs2[i,]$RiskYear==2022) dbs2[i,]$RiskYear=2050 }
for(i in 1:nrow(dbs2) ) {if(dbs2[i,]$RiskYear==2021) dbs2[i,]$RiskYear=2040 }
for(i in 1:nrow(dbs2) ) {if(dbs2[i,]$RiskYear==2020) dbs2[i,]$RiskYear=2030 }
for(i in 1:nrow(dbs2) ) {if(dbs2[i,]$RiskYear==2019) dbs2[i,]$RiskYear=2020 }
for(i in 1:nrow(dbs2) ) {if(dbs2[i,]$RiskYear==2018) dbs2[i,]$RiskYear=2010 }

# Read in damages without company-wide rollups.  These cover all companies/facilities in the locations file used by the SE.
#SEoutput = read.table("./data/TCSDB/facility_locations_v2.csv.damages.allDFs.facilities.only.4r", header=TRUE)
#SEoutput2 = SEoutput %>% select(facility, df_used:dperiod9)
SEoutput = read.table("./data/TCSDB/TCSDB_structure_v3_locations.csv.damages.allDFs.facilities.only.4r", header=TRUE)
SEoutput2 = SEoutput %>% select(ParentCorpID, LocationID, LocationName, df_used:dperiod9)

# Select company by ParentCorpID.
#selfac = matrix("NA",nrow(SEoutput2),1)
#colnames(selfac)[1] = "shortname"
#for(i in 1:nrow(SEoutput2) ) {if( substr(SEoutput2[i,1],1,6)=="Micron" ) selfac[i] = substr(SEoutput2[i,1],8,nchar(as.character(SEoutput2[i,1]) )) }
#SEoutput3 = SEoutput2 %>% dplyr::mutate(selfac = selfac)
#SEoutput3 = filter(SEoutput2, substr(SEoutput2[,1],1,6)=="Micron")
SEoutput3 = filter(SEoutput2, ParentCorpID==1)

# Discard "allDFs" data for each facility.
SEoutput4 = filter(SEoutput3, substr(SEoutput3[,4],1,9)!="z--allDFs")

# Next remove phyiscal rows from dbs2, structure SEoutput3 rows like dbs2 physical rows, and merge.
# The first step could be avoided with a join.

