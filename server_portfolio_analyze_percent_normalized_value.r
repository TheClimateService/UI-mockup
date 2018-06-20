

    # Normalized values below are calculated from ./data/scoring_engine/nonphysical/TCSDB_structure.locations.csv.damages.allDFs.withvalues.with.nonphysical.byparentcorp.csv.  The values in this file are, for each parentCorp, total VaR summed across all locations for a given hazard and for a given period.  
    # Percent normalized value:  For the physical hazards, the total VaR values are divided by the corresponding total value for all locations for each hazard.  For non-physical hazards that are linked to physical hazards via the proportions given in the RiskFactor tab of TCSDB_structure_locations, the same division is performed.  For carbonprice, the same division cannot be applied since the result is just the carbon price per ton for the specified year.


      # The following gets total value by risk factor for all elements of the selected portfolio, and adds column to the working risk-factor sheet containing these values.
      corp_location_values <- filter(dbsheet11, ParentCorpID %in% portfolio_members$ParentCorpID) %>% select(starts_with("df") ) %>% select_if(is.numeric)
      rfsheet <- cbind(dbsheet7, total_asset_value=0)
      for(i in 1:nrow(rfsheet)) {dfname<-rfsheet$df_type[[i]]; values<-select(corp_location_values, dfname); rfsheet$total_asset_value[[i]]=colSums(values)}
    # When using the sum() function, the following line gives an error in the app about needing dataframe with all numeric variables.  However, this does not happen directly in r.  The solution is to use the sumCols() function.
    #for(i in 1:nrow(rfsheet)) {dfname<-rfsheet$df_type[[i]]; values<-select(corp_location_values, dfname); rfsheet$total_asset_value[[i]]=sum(values)}

      # The following gets total value by risk factor for each element of the portfolio individually, and adds columns to the working risk-factor sheet containing these values.
     for(j in 1:nrow(portfolio_members) ) {
      corp_location_values_1corp <- filter(dbsheet11, ParentCorpID==portfolio_members$ParentCorpID[j]) %>% select(starts_with("df") ) %>% select_if(is.numeric)
      rfsheet <- cbind(rfsheet, corpval=0)
      for(i in 1:nrow(rfsheet)) {dfname<-rfsheet$df_type[[i]]; values<-select(corp_location_values_1corp, dfname); rfsheet$corpval[[i]]=colSums(values)}
      colnames(rfsheet)[ncol(rfsheet)] <- portfolio_members$TickerSymbol[j]
     } #end for on portfolio_members

       ct2b <- cbind(inputTable, CompanyAssetsThisRisk= 0, NormalizedValue=0)
       for(i in 1:nrow(ct2b)) {
            ticker <- ct2b$Location[[i]]; 
            rf <- ct2b$RiskFactorName[[i]]; 
            rfrow <- filter(rfsheet, RiskFactorName==rf); 
	    corpvalue <- select(rfrow, ticker)
            ct2b$CompanyAssetsThisRisk[[i]] <- corpvalue[[1]]; 
            ct2b$NormalizedValue[[i]]=  ct2b$ValueAtRisk[[i]]/ct2b$CompanyAssetsThisRisk[[i]]
        } # endfor on i

    # Exclude carbon pricing since the way to normalize this is not yet clear.
    ct2b_nocarbon <- ct2b %>% filter(RiskFactorName != "Carbon pricing")    
    
