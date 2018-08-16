
library(writexl)

# Specify destination.
db="./data/TCSDB/user_data.xlsx"
csv="./data/TCSDB/user_data.csv"
csvheader="./data/TCSDB/user_data.csv.header"

# Assemble elements of user data.
# Use paste() to place in same row; c() to place in same column.
date = system("date | sed 's/ /_/g'",  intern = TRUE)
date_seconds = system("date +'%s'",  intern = TRUE)
cola = "USER$ParentCorpID"
colb = "input$rbLocations"
colc = "input$cbGroupBizType"
cold = "input$txtNumEmployees"
cole = "input$txtAssetValue_tx90p"
colf = "input$txtAssetValue_pdsisc"
colg = "input$txtAssetValue_coastalflood"
colh = "input$txtghgEmissions"
coli = "input$txtAssetValue_zindex"
colj = "cbBusinessFunctions"
colk = "Date"

if(input$rbLocations=="All locations") {
	a = as.character(USER$ParentCorpID)
	b = as.character(input$rbLocations)
	#c = as.character(input$cbGroupBizType)
	c = as.character(input$industry_sector)
	d = "-"
	e = "-"
	f = "-"
	g = "-"
	h = "-"
	i = "-"
	j = "-"
}

if(input$rbLocations!="All locations") {
	a = as.character(USER$ParentCorpID)
	b = as.character(input$rbLocations)
	c = "-"
	#d = as.character(input$txtNumEmployees)
	d = as.character(input$numEmployees)
	#e = as.character(input$txtAssetValue)
	e = as.character(input$assetValue_tx90p)
	f = as.character(input$assetValue_pdsisc)
	g = as.character(input$assetValue_coastalflood)
	h = as.character(input$ghgEmissions)
	i = as.character(input$assetValue_zindex)
	# Note that the business functions are returned as multiple rows and must be collapsed to a single string.  
	# Also note that the separator is "," and the business functions should not contain semicolons.
	j = toString( as.character(input$cbBusinessFunctions) )
}

USER_ENTRY = paste(a,";",b,";",c,";",d,";",e,";",f,";",g,";",h,";",i,";",j,";",date)
userData = data.frame(USER_ENTRY)

# Write date_seconds, user data, and date to table using append and then to xlsx.
write.table(userData, csv, col.names=FALSE, row.names=date_seconds, sep=";", quote=FALSE, append=TRUE)
#write.table(userData, "./data/TCSDB/junk", col.names=FALSE, row.names=date_seconds, quote=FALSE, append=TRUE)
#system("sort -r --field-separator=';' ./data/TCSDB/user_data.csv > ./data/TCSDB/junk",  intern = TRUE)
userData2 = read.table(csv, header=FALSE, sep=";", col.names=c("Date_seconds",cola,colb,colc,cold,cole,colf,colg,colh,coli,colj,colk))
write.table(userData2[1,], csvheader)
#write.table(colnames(userData2), csvheader)
write_xlsx(userData2, db, col_names=TRUE)



