
library(readxl)

# Specify source.
db="./data/TCSDB/TCSDB_structure.xlsx"
#db="./data/TCSDB/TCSDB_structure_v3.xlsx"

# Read each sheet.  
# Current sheet names:  CorpRiskTable, ValueAtRisk, Location, ParentCorp, TCDFCategory, TCFDSubCat, RiskFactor, Scenarios, CorpRiskTable_withSEoutputs, users, LocationValues, DamageFunctions, BusinessTypes.
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

# Create initiator for user_data.csv that will be used by script_apply_userdata4SE to create the first user_data.csv.latest.csv file.  When this initiator is copied to user_data.csv (the historical record of all user data saves), this effectively removes all memory of past user actions.  To preserve this memory, concatenate the initatior file and the current user_data.csv.latest.csv (with pre-pended date_seconds later than the dates in the initiator file). 
# Format of the user_data.csv file:
# 1519139219;4 ; All locations ; Agriculture ; - ; - ; - ; Tue_Feb_20_10:06:59_EST_2018
# 1519139225;4 ; Indooroopilly_Shopping_Centre ; - ; 250 ; 100 ; Clean Room Manufacturing, R&D ; Tue_Feb_20_10:07:05_EST_2018
# The columns above are:  "Date_seconds" "USER.ParentCorpID" "input.rbLocations" "input.cbGroupBizType" "input.txtNumEmployees" "input.txtAssetValue" "cbBusinessFunctions" "Date"

date = system("date | sed 's/ /_/g'",  intern = TRUE)
date_seconds = system("date +'%s'",  intern = TRUE)

# For individual locations:
init= dbsheet3 %>% select(ParentCorpID, LocationName)
init$ParentCorpID = as.integer(init$ParentCorpID)
names(init) = c("USER.ParentCorpID", "input.rbLocations")
init$input.cbGroupBizType <- "-"
init$input.txtNumEmployees <- "250"
init$input.txtAssetValue <- "100"
init$cbBusinessFunctions <- "Clean Room Manufacturing, R&D"
init$Date <- as.character(date)
init$Date_seconds <- date_seconds
init <- init %>% select(Date_seconds, everything())

# For parent-corp "All locations":
init2 <- data.frame(unique(init$USER.ParentCorpID))
names(init2) = c("USER.ParentCorpID")
init2$input.rbLocations <- "All locations"
init2$input.cbGroupBizType <- "Manufacturing"
init2$input.txtNumEmployees <- "-"
init2$input.txtAssetValue <- "-"
init2$cbBusinessFunctions <- "-"
init2$Date <- date
init2$Date_seconds <- date_seconds
init2 <- init2 %>% select(Date_seconds, everything())

# Concatenate the two sets and write as table:
init3 <- rbind(init, init2)
write.table(init3,"./data/TCSDB/user_data_initiator.csv", sep=";", row.names=FALSE, quote=FALSE, col.names=FALSE)

