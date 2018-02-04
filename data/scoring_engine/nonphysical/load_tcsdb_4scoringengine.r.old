
# This script is set up to be run from the current app directory.
# If running with Rscript, the command would be, for example:
#	Rscript ./data/scoring_engine/nonphysical/load_tcsdb_nonphysical.r 

library(readxl)
library(tidyverse)

# Specify source.
db="./data/TCSDB/TCSDB_structure.xlsx"
#db="./data/TCSDB/TCSDB_structure_v3.xlsx"
#db="./data/scoring_engine/nonphysical/TCSDB_structure.xlsx"
#db="./data/scoring_engine/nonphysical/TCSDB_structure_TT.xlsx"

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

locall = dbsheet3
loc= dbsheet3 %>% select(ParentCorpID, LocationID, LocationName)
corp = dbsheet4
cat = dbsheet5
subcat = dbsheet6 %>% select(TCFDSubCatID, TCFDCategoryID, TCFDSubCatName)
riskfac = dbsheet7

# Create tables of locations and risks. 
write.table(loc,"./data/scoring_engine/nonphysical/locations.csv", sep=";")
write.table(locall,"./data/scoring_engine/nonphysical/locations4SE.csv", sep=";")
write.table(riskfac,"./data/scoring_engine/nonphysical/riskfactors.csv", sep=";")
write.table(subcat,"./data/scoring_engine/nonphysical/subcat.csv", sep=";")
write.table(cat,"./data/scoring_engine/nonphysical/cat.csv", sep=";")
write.table(corp,"./data/scoring_engine/nonphysical/corp.csv", sep=";")

