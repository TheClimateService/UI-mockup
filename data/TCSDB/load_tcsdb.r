
library(readxl)

# Specify source.
db="./data/TCSDB/TCSDB_structure.xlsx"
#db="./data/TCSDB/TCSDB_structure_v3.xlsx"

# Read each sheet.  
# Current sheet names:  CorpRiskTable, ValueAtRisk, Location, ParentCorp, TCDFCategory, TCFDSubCat, RiskFactor, Scenarios, CorpRiskTable_withSEoutputs, users, LocationValues, DamageFunctions, BusinessTypes, BusinessFunctions, Portfolios, Sectors, NOTES.
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
dbsheet15 = read_excel(db,15)
dbsheet_sectors = read_excel(db,16)

# Create file of lon/lat locations for CMIP data extraction.
loc_lonlat= dbsheet3 %>% select(lon, lat, ParentCorpID, LocationID, LocationName)
write.table(loc_lonlat,"./data/TCSDB/locations4cmipdata.csv", sep=" ", row.names=FALSE, col.names=FALSE)

# Create initiator for user_data.csv that will be used by script_apply_userdata4SE to create the first user_data.csv.latest.csv file.  When this initiator is copied to user_data.csv (the historical record of all user data saves), this effectively removes all memory of past user actions.  To preserve this memory, concatenate the initatior file and the current user_data.csv.latest.csv (with pre-pended date_seconds later than the dates in the initiator file). 
# Note that the data being put into the initiator file is NOT the data in the TCSDB_structure.xlsx file.  It should be possible to do this, but we meed to flesh out the how we want user-input data the TCSDB data to be related.  For example, are they to be kept independent?  Should the default values appearing in the user-input panel be populated with TCSDB values?

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

#init$input.txtAssetValue <- "100"
# The following sets the user-data initiator values to what is in the LocationValues sheet in TCSDB_structure.xlsx.
val_tx90p = dbsheet11 %>% select(df_tx90p)
init$input.txtAssetValue_tx90p <- unlist(val_tx90p)
val_pdsisc = dbsheet11 %>% select(df_pdsisc)
init$input.txtAssetValue_pdsisc <- unlist(val_pdsisc)
val_coastalflood = dbsheet11 %>% select(df_coastalflood)
init$input.txtAssetValue_coastalflood <- unlist(val_coastalflood)
ghg = dbsheet11 %>% select(df_carbonprice)
init$input.txtghgEmissions <- unlist(ghg)
val_zindex = dbsheet11 %>% select(df_zindex)
init$input.txtAssetValue_zindex <- unlist(val_zindex)

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
init2$input.txtAssetValue_tx90p <- "-"
init2$input.txtAssetValue_pdsisc <- "-"
init2$input.txtAssetValue_coastalflood <- "-"
init2$input.txtghgEmissions <- "-"
init2$input.txtAssetValue_zindex <- "-"
init2$cbBusinessFunctions <- "-"
init2$Date <- date
init2$Date_seconds <- date_seconds
init2 <- init2 %>% select(Date_seconds, everything())

# Concatenate the two sets and write as table:
init3 <- rbind(init, init2)
write.table(init3,"./data/TCSDB/user_data_initiator.csv", sep=";", row.names=FALSE, quote=FALSE, col.names=FALSE)
write.table(init3,"./data/TCSDB/user_data_initiator.csv.header", sep=" ", row.names=FALSE, quote=TRUE, col.names=TRUE)

