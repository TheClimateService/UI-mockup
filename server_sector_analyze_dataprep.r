
# This code reads in the byparentcorp data and uses the sector assignments in dbsheet_sectors to build a data structure in the same format as byparentcorp data, but with sectors in place of the parentcorp name.  It aggregates the ValueAtRisk across the companies assigned to each sector using "sum" or "mean" functions (or some other function).

  corpTable2 <- readr::read_csv("./data/scoring_engine/nonphysical/TCSDB_structure.locations.csv.damages.allDFs.withvalues.with.nonphysical.byparentcorp.csv")

  # dbsheet_sectors is loaded by ./data/TCSDB/load_tcsdb.r
list <- names(dbsheet_sectors)
imin <- 5
imax <- length(list)
for(i in imin:imax) {
	name <- as.name(list[i])
	sector_members <- filter(dbsheet_sectors, dbsheet_sectors[[name]]==1)
	d <- subset(corpTable2, Location %in% sector_members$TickerSymbol)
	dagg <- aggregate(ValueAtRisk ~ TCFDCategoryName+TCFDSubCatName+RiskFactorName+ScenarioName+RiskYear, data = d, sum)
	sector <- matrix(list[i], nrow=nrow(dagg) )
	ctbl_sector <- cbind(sector,dagg)
	if(i==imin) {ctbl_sector_all <- ctbl_sector} else {ctbl_sector_all <- rbind(ctbl_sector_all, ctbl_sector) }
   }

write.table(ctbl_sector_all, "./junk")

	# XXX To do this by sector in a portfolio that consists of sectors:  
	# 1.  define sector tab like portfolio tab in TCSDB
	# 2.  use sector_members in subset command above
	# 3.  aggregate across ticker symbols in this sector using:
	#	ind <- filter(dbsheet_sectors, Consumer_Staples==1)
	#	d <- subset(corpTable2, Location %in% ind$TickerSymbol)
	# 	dagg <- aggregate(ValueAtRisk ~ TCFDCategoryName+TCFDSubCatName+RiskFactorName+ScenarioName+RiskYear, data = d, sum)
	#	sector <- matrix("Consumer_Staples", nrow=nrow(dagg) )
	#	dagg2 <- cbind(sector,dagg)
		
