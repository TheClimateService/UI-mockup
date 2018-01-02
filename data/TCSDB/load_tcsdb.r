
library(readxl)

# Specify source.
db="./data/TCSDB/TCSDB_structure.xlsx"

# Read each sheet.  
# Current sheet names:  CorpRiskTable, ValueAtRisk, Location, ParentCorp, TCDFCategory, TCFDSubCat, RiskFactor, Scenarios, plus CorpRiskTable_withSEoutputs.
dbsheet1 = read_excel(db,1)
dbsheet2 = read_excel(db,2)
dbsheet3 = read_excel(db,3)
dbsheet4 = read_excel(db,4)
dbsheet5 = read_excel(db,5)
dbsheet6 = read_excel(db,6)
dbsheet7 = read_excel(db,7)
dbsheet8 = read_excel(db,8)
dbsheet9 = read_excel(db,9)

# Next create sheet 9 by reading in SE outputs and merging with sheet1.
# dbs185 = dbs1 %>% filter(ScenarioName=="RCP8.5" & RiskYear<2019)

