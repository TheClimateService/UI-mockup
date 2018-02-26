
# This script is set up to be run from the scoring-engine directory.
# If running with Rscript, the command would be, for example:
#	Rscript ./load_tcsdb_4scoringengine.r 

library(readxl)
library(tidyverse)

# Specify source.
#setwd("./data/scoring_engine/")
db="../TCSDB/TCSDB_structure.xlsx"
#db="./data/TCSDB/TCSDB_structure_v3.xlsx"
#db="./data/scoring_engine/nonphysical/TCSDB_structure.xlsx"
#db="./data/scoring_engine/nonphysical/TCSDB_structure_TT.xlsx"

# Read each sheet.  
# Current sheet names:  CorpRiskTable, ValueAtRisk, Location, ParentCorp, TCDFCategory, TCFDSubCat, RiskFactor, Scenarios, CorpRiskTable_withSEoutputs, users, LocationValues, DamageFunctions, BusinessTypes, BusinessFunctions.
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
dbsheet11 = read_excel(db,11)
dbsheet12 = read_excel(db,12)
dbsheet13 = read_excel(db,13)
dbsheet14 = read_excel(db,14)

locall = dbsheet3
loc= dbsheet3 %>% select(ParentCorpID, LocationID, LocationName)
corp = dbsheet4
cat = dbsheet5
subcat = dbsheet6 %>% select(TCFDSubCatID, TCFDCategoryID, TCFDSubCatName)
riskfac = dbsheet7
locvalues = dbsheet11
damagefunc = dbsheet12

# Create tables of locations,risks, values, and DFs. 
write.table(loc,"./nonphysical/locations.csv", sep=";")
write.table(locall,"./nonphysical/locations4SE.csv", sep=";", row.names=FALSE)
write.table(locvalues,"./nonphysical/locationvalues4SE.csv", sep=";", row.names=FALSE)
write.table(riskfac,"./nonphysical/riskfactors.csv", sep=";")
write.table(subcat,"./nonphysical/subcat.csv", sep=";")
write.table(cat,"./nonphysical/cat.csv", sep=";")
write.table(corp,"./nonphysical/corp.csv", sep=";")
write.table(damagefunc,"./df.csv", sep=",", row.names=FALSE)

# Return to original working directory.
#setwd("../../")

