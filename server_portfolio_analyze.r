
# ----------------------------
#         PORTFOLIO - ANALYZE
# ----------------------------  
   
  # TCSDB in excel format is read in ui.R via:  source("./data/TCSDB/load_tcsdb.r")
  # The version of this table with scoring-engine outputs for RCP8.5 and 9 decades is sheet 9.
  # When using the decadal form, set the sliderInputYear to the decadal version in ui.R.
  # corpTable = dbsheet9
  #corpTable <- readr::read_csv("./data/scoring_engine/nonphysical/TCSDB_structure_locations.csv.cln.damages.allDFs.with.nonphysical.csv")
  #corpTable2 <- readr::read_csv("./data/scoring_engine/nonphysical/TCSDB_structure.locations.csv.damages.allDFs.with.nonphysical.byparentcorp.csv")
  corpTable2 <- readr::read_csv("./data/scoring_engine/nonphysical/TCSDB_structure.locations.csv.damages.allDFs.withvalues.with.nonphysical.byparentcorp.csv")
  parentCorp = dbsheet4

  # UI Input selectors for the corporate finance page, based on the database values  
  output$selectInput_locationPort <- renderUI({
    # Portfolios are defined in sheet 15 of TCSDB_structure.xlsx
    ncols <- length(names(dbsheet15))
    portfolios <- names(dbsheet15)[5:ncols]
    #selectInput('inputLocationsPort',"Portfolios",c('Entire portfolio', portfolios, unique(subset(parentCorp, select = TickerSymbol))),selectize = TRUE)
    selectInput('inputLocationsPort',"Portfolios",c(portfolios),selectize = TRUE)
  })
  
  output$selectInput_scenarioPort <- renderUI({
    selectInput('inputScenariosPort',"Scenario",c(unique(as.character(corpTable2$ScenarioName))),selected='RCP8.5',selectize = TRUE)
  })
  
# ----------------------------
  #barByRiskFactor
# ----------------------------
  output$barByRiskFactorPort <- renderGvis({

    corpTable2 <- readr::read_csv("./data/scoring_engine/nonphysical/TCSDB_structure.locations.csv.damages.allDFs.withvalues.with.nonphysical.byparentcorp.csv")
    #if(input$riskfactor_subset_portfolio=="Chronic physical + Carbon price") corpTable2 <- filter(corpTable2, RiskFactorName=="Temperature extremes" | RiskFactorName=="Drought" | RiskFactorName=="Coastal flooding" | RiskFactorName=="Carbon pricing")
    if(input$riskfactor_subset_portfolio=="Chronic physical + Carbon price") corpTable2 <- filter(corpTable2, TCFDSubCatName=="Chronic" | RiskFactorName=="Carbon pricing")

    parentCorp = dbsheet4
    portfolioSheet <- dbsheet15
    ncols <- length(names(portfolioSheet))
    portfolios <- names(portfolioSheet)[5:ncols]
   
    if (input$inputLocationsPort != 'Entire portfolio' & (input$inputLocationsPort %in% portfolios)=="FALSE") {
      corpTable2 <- corpTable2[which(corpTable2$Location == input$inputLocationsPort & corpTable2$RiskYear == input$sliderInputYearPort),]
    }

    if (input$inputLocationsPort == 'Entire portfolio') {
      #corpTable2 <- corpTable2[which(corpTable2$ParentCorpID == USER$ParentCorpID & corpTable2$RiskYear == input$sliderInputYearPort),]
      corpTable2 <- corpTable2[which(corpTable2$RiskYear == input$sliderInputYearPort),]
    }

    if (input$inputLocationsPort != 'Entire portfolio' & (input$inputLocationsPort %in% portfolios)=="TRUE") {
      portfolio <- input$inputLocationsPort
      portfolio_members <- portfolioSheet %>% filter(portfolioSheet[[portfolio]]==1)
      corpTable2 <- subset(corpTable2, Location %in% portfolio_members$TickerSymbol)
      corpTable2 <- corpTable2[which(corpTable2$RiskYear == input$sliderInputYearPort),]

      corpTable2_full <- corpTable2

      corpTable2 = select(corpTable2, RiskFactorName, ValueAtRisk)
      corpTable2 = as.data.table(corpTable2)
      corpTable2 = corpTable2[,lapply(.SD,sum),by="RiskFactorName"]
    } #endif on input in portfolios

    values <- corpTable2$ValueAtRisk

    if(input$riskmetric_portfolio=="Absolute") {
         #ct2a = select(corpTable2, RiskFactorName, ValueAtRisk)
         #ct2a = as.data.table(ct2a)
         #ct2a = ct2a[,lapply(.SD,sum),by="RiskFactorName"]
	 #values2plot <- ct2a$ValueAtRisk
          values2plot <- values
          rfnames=corpTable2$RiskFactorName;
          xaxisname <- "Impact (Million US Dollars)"
    } # endif

    # Normalized values below are calculated from ./data/scoring_engine/nonphysical/TCSDB_structure.locations.csv.damages.allDFs.withvalues.with.nonphysical.byparentcorp.csv.  The values in this file are, for each parentCorp, total VaR summed across all locations for a given hazard and for a given period.  
    # Percent normalized value:  For the physical hazards, the total VaR values are divided by the corresponding total value for all locations for each hazard.  For non-physical hazards that are linked to physical hazards via the proportions given in the RiskFactor tab of TCSDB_structure_locations, the same division is performed.  For carbonprice, the same division cannot be applied since the result is just the carbon price per ton for the specified year.

    if(input$riskmetric_portfolio=="Relative") {

	# The following produces ct2b and ct2b_nocarbon
	inputTable <- corpTable2_full
    	source("./server_portfolio_analyze_percent_normalized_value.r", local=TRUE)

        ct2c = select(ct2b_nocarbon, RiskFactorName, NormalizedValue)
        ct2c = as.data.table(ct2c)
        ct2c = ct2c[,lapply(.SD,sum),by="RiskFactorName"]
    	values2plot <- 100 * ct2c$NormalizedValue
    	rfnames <- ct2c$RiskFactorName
    	xaxisname <- "Relative Impact (percent)"

    }  # endif on percent normalized value

    df2use = data.frame(values2plot, rfnames) 
    df2use <- df2use %>% group_by(rfnames) %>% summarise(Risk=sum(values2plot)) %>% arrange(desc(Risk)) 
    # 
    # table2use <- table2use %>% group_by(RiskFactorName) %>% summarise(Risk=sum(ValueAtRisk)) %>% arrange(desc(Risk)) %>% head(n=5)
    # table2usedf=data.frame(table2use)
    # table2usedf[2] <- round(table2usedf[2],digits=0)
    
    gvisBarChart(df2use, options = list(
      hAxis=paste("{title:'",xaxisname,"'}"),
      vAxis="{title:'Risk Factor'}",
      legend="none",
      height=500)
    )
      
    # plot_ly(x=df2use$Risk, y=df2use$rfnames, type = 'bar', orientation = 'h') %>% layout(margin = list(l=180, b=100)) %>%
    #   #plot_ly(x=corpTable2$ValueAtRisk, y=rfnames, type = 'bar', orientation = 'h') %>% layout(margin = list(l=180, b=100)) %>%
    #   layout(xaxis = list(title = xaxisname))
    
    # plot_ly(x=values2plot, y=rfnames, type = 'bar', orientation = 'h') %>% layout(margin = list(l=180, b=100)) %>%
    # #plot_ly(x=corpTable2$ValueAtRisk, y=rfnames, type = 'bar', orientation = 'h') %>% layout(margin = list(l=180, b=100)) %>%
    #   layout(xaxis = list(title = xaxisname))
  }) 
  
  # ----------------------------
  # treemapByTicker
  # ----------------------------
  output$treemapByTicker <- renderGvis({
    corpTable2 <- readr::read_csv("./data/scoring_engine/nonphysical/TCSDB_structure.locations.csv.damages.allDFs.withvalues.with.nonphysical.byparentcorp.csv")
    if(input$riskfactor_subset_portfolio=="Chronic physical + Carbon price") corpTable2 <- filter(corpTable2, TCFDSubCatName=="Chronic" | RiskFactorName=="Carbon pricing")
    parentCorp = dbsheet4
    portfolioSheet <- dbsheet15
    ncols <- length(names(portfolioSheet))
    portfolios <- names(portfolioSheet)[5:ncols]
    
    if (input$inputLocationsPort != 'Entire portfolio' & (input$inputLocationsPort %in% portfolios)=="FALSE") {
      #corpTable2 <- corpTable2[which(corpTable2$ParentCorpID == USER$ParentCorpID & corpTable2$Location == input$inputLocationsPort & corpTable2$RiskYear == input$sliderInputYearPort),]
      corpTable2 <- corpTable2[which(corpTable2$Location == input$inputLocationsPort & corpTable2$RiskYear == input$sliderInputYearPort),]
    }
    
    if (input$inputLocationsPort == 'Entire portfolio') {
      #corpTable2 <- corpTable2[which(corpTable2$ParentCorpID == USER$ParentCorpID & corpTable2$RiskYear == input$sliderInputYearPort),]
      corpTable2 <- corpTable2[which(corpTable2$RiskYear == input$sliderInputYearPort),]
    }
    
    if (input$inputLocationsPort != 'Entire portfolio' & (input$inputLocationsPort %in% portfolios)=="TRUE") {
      portfolio <- input$inputLocationsPort
      portfolio_members <- portfolioSheet %>% filter(portfolioSheet[[portfolio]]==1)
      corpTable2 <- subset(corpTable2, Location %in% portfolio_members$TickerSymbol)
      corpTable2 <- corpTable2[which(corpTable2$RiskYear == input$sliderInputYearPort),]
    }
    
    values <- corpTable2$ValueAtRisk
    
    if(input$riskmetric_portfolio=="Absolute") {
      values2plot <- values
      table2use <- corpTable2 %>% select(Location:ValueAtRisk)
      rfnames=corpTable2$RiskFactorName
      axisname <- "Impact ($M)"
    } # endif
    
    if(input$riskmetric_portfolio=="Relative") {
      
      # The following produces ct2b and ct2b_nocarbon
      inputTable <- corpTable2
      source("./server_portfolio_analyze_percent_normalized_value.r", local=TRUE)
      
      values2plot <- 100 * ct2b_nocarbon$NormalizedValue
      table2use <- ct2b_nocarbon %>% select(Location:ValueAtRisk, CompanyAssetsThisRisk, NormalizedValue)
      table2use$NormalizedValue <- round(100*table2use$NormalizedValue,2)
      rfnames <- ct2b_nocarbon$RiskFactorName
      axisname <- "Relative Impact (percent)"
      
    }  # endif on percent normalized value

    #This next section wrangles the data into TreeMap format, in which every row is a node with a unique ID and a parent

    #Append parent to each risk factor node. Needed because TreeMap needs a unique ID (unid)
    table2use <- table2use %>% mutate(unidvar = paste(table2use$RiskFactorName,table2use$Location,sep = "_")) 
    df2use = data.frame(table2use)
 
    #Create header rows to define the portfolio for the TreeMap
    tickersOfMembers <-c(portfolio_members[4])
    names(tickersOfMembers) <- "RiskFactorName" #need to change the name of the vector so it can be merged with the core data frame
    headerrows = data.frame(unidvar=tickersOfMembers,Location=rep(input$inputLocationsPort,nrow(portfolio_members)),ValueAtRisk=rep(0,nrow(portfolio_members)))
    colnames(headerrows)[1] <- "unidvar"
    print(headerrows)
    
    #Add a top global row with the portfolio name
    headerrows <- add_row(headerrows, unidvar=input$inputLocationsPort, Location=c(NA), ValueAtRisk=c(0), .before = 1)

    #Bind the header rows into the core data frame
    # df2use <- data.frame(corpTable2) %>% group_by(RiskFactorName) %>% summarize(sVaR=sum(ValueAtRisk))
    df2use <- bind_rows(headerrows,df2use)
    
    print(df2use)
    
    #Output the TreeMap HTML
    gvisTreeMap(df2use, idvar = "unidvar" , parentvar = "Location", sizevar = "ValueAtRisk", colorvar = "ValueAtRisk", options = list(
      showScale=TRUE, maxColor = '#dd4444', minColor = '#4444dd', maxPostDepth = 2)
    )
  }) #the v and f in idvar are for value (unid) and formatted (display)   paste("{v:'",df2use$unidvar,"', f:'",df2use$RiskFactorName,"'}")
  
  
# ----------------------------
  #barByLocation
# ----------------------------
  output$barByLocationPort <- renderPlotly({

    corpTable2 <- readr::read_csv("./data/scoring_engine/nonphysical/TCSDB_structure.locations.csv.damages.allDFs.withvalues.with.nonphysical.byparentcorp.csv")
    if(input$riskfactor_subset_portfolio=="Chronic physical + Carbon price") corpTable2 <- filter(corpTable2, TCFDSubCatName=="Chronic" | RiskFactorName=="Carbon pricing")

    parentCorp = dbsheet4
    portfolioSheet <- dbsheet15
    ncols <- length(names(portfolioSheet))
    portfolios <- names(portfolioSheet)[5:ncols]

    if (input$inputLocationsPort != 'Entire portfolio' & (input$inputLocationsPort %in% portfolios)=="FALSE") {
      #corpTable2 <- corpTable2[which(corpTable2$ParentCorpID == USER$ParentCorpID & corpTable2$Location == input$inputLocationsPort & corpTable2$RiskYear == input$sliderInputYearPort),]
      corpTable2 <- corpTable2[which(corpTable2$Location == input$inputLocationsPort & corpTable2$RiskYear == input$sliderInputYearPort),]
    }

    if (input$inputLocationsPort == 'Entire portfolio') {
      #corpTable2 <- corpTable2[which(corpTable2$ParentCorpID == USER$ParentCorpID & corpTable2$RiskYear == input$sliderInputYearPort),]
      corpTable2 <- corpTable2[which(corpTable2$RiskYear == input$sliderInputYearPort),]
    }

    if (input$inputLocationsPort != 'Entire portfolio' & (input$inputLocationsPort %in% portfolios)=="TRUE") {
      portfolio <- input$inputLocationsPort
      portfolio_members <- portfolioSheet %>% filter(portfolioSheet[[portfolio]]==1)
      corpTable2 <- subset(corpTable2, Location %in% portfolio_members$TickerSymbol)
      corpTable2 <- corpTable2[which(corpTable2$RiskYear == input$sliderInputYearPort),]
    }

    values <- corpTable2$ValueAtRisk

    if(input$riskmetric_portfolio=="Absolute") {
	values2plot <- values
	table2use <- corpTable2
        rfnames=corpTable2$RiskFactorName
        axisname <- "Impact ($M)"
    } # endif

    if(input$riskmetric_portfolio=="Relative") {

	# The following produces ct2b and ct2b_nocarbon
	inputTable <- corpTable2
    	source("./server_portfolio_analyze_percent_normalized_value.r", local=TRUE)

    	values2plot <- 100 * ct2b_nocarbon$NormalizedValue
	table2use <- ct2b_nocarbon
    	rfnames <- ct2b_nocarbon$RiskFactorName
    	axisname <- "Relative Impact (percent)"

    }  # endif on percent normalized value

    ncorp <- pull(count(corpTable2))

    #plot_ly(corpTable2, x = ~Location, y = ~ValueAtRisk, type='bar', text=corpTable2$RiskFactorName, marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(ncorp))) %>%
    plot_ly(table2use, x = ~Location, y = values2plot, type='bar', text=rfnames, marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(ncorp))) %>%
      layout(yaxis = list(title = axisname), barmode = 'stack', margin = list(l=80,b=100))
  }) 
  
# ----------------------------
  #stacked area by Time
# ----------------------------
  output$areaByTimePort <- renderPlotly({
    corpTable2 <- readr::read_csv("./data/scoring_engine/nonphysical/TCSDB_structure.locations.csv.damages.allDFs.withvalues.with.nonphysical.byparentcorp.csv")
    parentCorp = dbsheet4
    portfolioSheet <- dbsheet15
    ncols <- length(names(portfolioSheet))
    portfolios <- names(portfolioSheet)[5:ncols]

    if (input$inputLocationsPort != 'Entire portfolio' & (input$inputLocationsPort %in% portfolios)=="FALSE") {
      #corpTable2 <- corpTable2[which(corpTable2$ParentCorpID == USER$ParentCorpID & corpTable2$Location == input$inputLocationsPort),]
      corpTable2 <- corpTable2[which(corpTable2$Location == input$inputLocationsPort),]
    }

    if (input$inputLocationsPort != 'Entire portfolio' & (input$inputLocationsPort %in% portfolios)=="TRUE") {
      portfolio <- input$inputLocationsPort
      portfolio_members <- portfolioSheet %>% filter(portfolioSheet[[portfolio]]==1)
      corpTable2 <- subset(corpTable2, Location %in% portfolio_members$TickerSymbol)
    }

    values <- corpTable2$ValueAtRisk

    if(input$riskmetric_portfolio=="Absolute") {
	values2plot <- values
	table2use <- corpTable2
        rfnames=corpTable2$RiskFactorName
        axisname <- "Impact ($M)"
    } # endif

    if(input$riskmetric_portfolio=="Relative") {

	# The following produces ct2b and ct2b_nocarbon
	inputTable <- corpTable2
    	source("./server_portfolio_analyze_percent_normalized_value.r", local=TRUE)

    	values2plot <- 100 * ct2b_nocarbon$NormalizedValue
	table2use <- ct2b_nocarbon
    	rfnames <- ct2b_nocarbon$RiskFactorName
    	axisname <- "Relative Impact (percent)"

    }  # endif on percent normalized value

    # to chart a time series, need to build a new datafame with additive traces. I'm sure there's a better way to do this.
    nriskyears = length(corpTable2 %>% group_by(RiskYear) %>% summarise(svar=sum(ValueAtRisk)) %>% select(RiskYear) %>% pull())
    time_series = as.data.frame( matrix(0, nrow = nriskyears, ncol = 10, dimnames = list(c(1:nriskyears), c("RiskYear", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9"))) )

    if(input$riskmetric_portfolio=="Absolute") {

    time_series$RiskYear <- table2use %>% group_by(RiskYear) %>% summarise(svar=sum(ValueAtRisk)) %>% select(RiskYear) %>% pull()
    time_series$s1 <- table2use %>% filter(TCFDSubCatName=='Policy and Legal') %>% group_by(RiskYear) %>% summarise(s1=sum(ValueAtRisk)) %>% select(s1) %>% pull()
    time_series$s2 <- table2use %>% filter(TCFDSubCatName=='Technology') %>% group_by(RiskYear) %>% summarise(s2=sum(ValueAtRisk)) %>% select(s2) %>% pull()
    time_series$s3 <- table2use %>% filter(TCFDSubCatName=='Market') %>% group_by(RiskYear) %>% summarise(s3=sum(ValueAtRisk)) %>% select(s3) %>% pull()
    time_series$s4 <- table2use %>% filter(TCFDSubCatName=='Reputation') %>% group_by(RiskYear) %>% summarise(s4=sum(ValueAtRisk)) %>% select(s4) %>% pull()
    time_series$s5 <- table2use %>% filter(TCFDSubCatName=='Acute') %>% group_by(RiskYear) %>% summarise(s5=sum(ValueAtRisk)) %>% select(s5) %>% pull()
    time_series$s6 <- table2use %>% filter(TCFDSubCatName=='Chronic') %>% group_by(RiskYear) %>% summarise(s6=sum(ValueAtRisk)) %>% select(s6) %>% pull()
    time_series$s7 <- table2use %>% filter(TCFDSubCatName=='Resource Efficiency') %>% group_by(RiskYear) %>% summarise(s7=sum(ValueAtRisk)) %>% select(s7) %>% pull()
    time_series$s8 <- table2use %>% filter(TCFDSubCatName=='Energy Source') %>% group_by(RiskYear) %>% summarise(s8=sum(ValueAtRisk)) %>% select(s8) %>% pull()
    time_series$s9 <- table2use %>% filter(TCFDSubCatName=='Resilience') %>% group_by(RiskYear) %>% summarise(s9=sum(ValueAtRisk)) %>% select(s9) %>% pull()

    } # endif


    if(input$riskmetric_portfolio=="Relative") {

    time_series$RiskYear <- table2use %>% group_by(RiskYear) %>% summarise(svar=sum(NormalizedValue)) %>% select(RiskYear) %>% pull()
    # Note use of 100* below to convert from decimal to percent.
    time_series$s1 <- table2use %>% filter(TCFDSubCatName=='Policy and Legal') %>% group_by(RiskYear) %>% summarise(s1=sum(100*NormalizedValue)) %>% select(s1) %>% pull()
    time_series$s2 <- table2use %>% filter(TCFDSubCatName=='Technology') %>% group_by(RiskYear) %>% summarise(s2=sum(100*NormalizedValue)) %>% select(s2) %>% pull()
    time_series$s3 <- table2use %>% filter(TCFDSubCatName=='Market') %>% group_by(RiskYear) %>% summarise(s3=sum(100*NormalizedValue)) %>% select(s3) %>% pull()
    time_series$s4 <- table2use %>% filter(TCFDSubCatName=='Reputation') %>% group_by(RiskYear) %>% summarise(s4=sum(100*NormalizedValue)) %>% select(s4) %>% pull()
    time_series$s5 <- table2use %>% filter(TCFDSubCatName=='Acute') %>% group_by(RiskYear) %>% summarise(s5=sum(100*NormalizedValue)) %>% select(s5) %>% pull()
    time_series$s6 <- table2use %>% filter(TCFDSubCatName=='Chronic') %>% group_by(RiskYear) %>% summarise(s6=sum(100*NormalizedValue)) %>% select(s6) %>% pull()
    time_series$s7 <- table2use %>% filter(TCFDSubCatName=='Resource Efficiency') %>% group_by(RiskYear) %>% summarise(s7=sum(100*NormalizedValue)) %>% select(s7) %>% pull()
    time_series$s8 <- table2use %>% filter(TCFDSubCatName=='Energy Source') %>% group_by(RiskYear) %>% summarise(s8=sum(100*NormalizedValue)) %>% select(s8) %>% pull()
    time_series$s9 <- table2use %>% filter(TCFDSubCatName=='Resilience') %>% group_by(RiskYear) %>% summarise(s9=sum(100*NormalizedValue)) %>% select(s9) %>% pull()

    }  # endif on percent normalized value
    
    
    # hack the stacking (plotly doesn't actually do stacking - it's a documented problem.)
    time_series$stack1 <- time_series$s1
    time_series$stack2 <- time_series$stack1 + time_series$s2
    time_series$stack3 <- time_series$stack2 + time_series$s3
    time_series$stack4 <- time_series$stack3 + time_series$s4
    time_series$stack5 <- time_series$stack4 + time_series$s5
    time_series$stack6 <- time_series$stack5 + time_series$s6
    time_series$stack7 <- time_series$stack6 + time_series$s7
    time_series$stack8 <- time_series$stack7 + time_series$s8
    time_series$stack9 <- time_series$stack8 + time_series$s9
    
# Draw the graph

  # Note that the plot is saved as object tplot under each conditional; this is needed to avoid the following:
  #    Error in UseMethod: no applicable method for 'ggplotly' applied to an object of class "NULL".

  if(input$riskfactor_subset_portfolio=="Chronic physical + Carbon price") { 
    time_series$stack1 <- corpTable2 %>% filter(RiskFactorName=='Carbon pricing') %>% group_by(RiskYear) %>% summarise(s1=sum(ValueAtRisk)) %>% select(s1) %>% pull()
    time_series$stack2 <- time_series$stack1 + time_series$s6
    tplot <- plot_ly(time_series, x = ~RiskYear, y = ~stack1, name='Policy & Legal - Carbon Price', type='scatter', mode = 'none', fill = 'tonexty') %>% 
      add_trace(y = ~stack2, name = 'Chronic', fill = 'tonexty') %>%
      layout(yaxis = list(title = 'Impact ($M)', showgrid = TRUE), xaxis = list(showgrid = TRUE), margin = list(l=80,b=100))
    } # endif

  if(input$riskfactor_subset_portfolio=="All") { 
    tplot <- plot_ly(time_series, x = ~RiskYear, y = ~stack1, name='Policy & Legal', type='scatter', mode = 'none', fill = 'tonexty') %>% 
      add_trace(y = ~stack2, name = 'Technology', fill = 'tonexty') %>%
      add_trace(y = ~stack3, name = 'Market', fill = 'tonexty') %>%
      add_trace(y = ~stack4, name = 'Reputation', fill = 'tonexty') %>%
      add_trace(y = ~stack5, name = 'Acute', fill = 'tonexty') %>%
      add_trace(y = ~stack6, name = 'Chronic', fill = 'tonexty') %>%
      add_trace(y = ~stack7, name = 'Resource Efficiency', fill = 'tonexty') %>%
      add_trace(y = ~stack8, name = 'Energy Source', fill = 'tonexty') %>%
      add_trace(y = ~stack9, name = 'Resilience', fill = 'tonexty') %>%
      layout(yaxis = list(title = axisname, showgrid = FALSE), xaxis = list(showgrid = FALSE), margin = list(l=80,b=100))
    } # endif

   tplot

  }) 
  
# ----------------------------
  # TCFD stacked bar chart
# ----------------------------
  output$stackedCorpFinImpactsPlotPort <- renderPlotly({
    corpTable2 <- readr::read_csv("./data/scoring_engine/nonphysical/TCSDB_structure.locations.csv.damages.allDFs.withvalues.with.nonphysical.byparentcorp.csv")
    if(input$riskfactor_subset_portfolio=="Chronic physical + Carbon price") corpTable2 <- filter(corpTable2, TCFDSubCatName=="Chronic" | RiskFactorName=="Carbon pricing")
    parentCorp = dbsheet4
    portfolioSheet <- dbsheet15
    ncols <- length(names(portfolioSheet))
    portfolios <- names(portfolioSheet)[5:ncols]

    if (input$inputLocationsPort != 'Entire portfolio' & (input$inputLocationsPort %in% portfolios)=="FALSE") {
      #corpTable2 <- corpTable2[which(corpTable2$ParentCorpID == USER$ParentCorpID & corpTable2$Location == input$inputLocationsPort & corpTable2$RiskYear == input$sliderInputYearPort),]
      corpTable2 <- corpTable2[which(corpTable2$Location == input$inputLocationsPort & corpTable2$RiskYear == input$sliderInputYearPort),]
    }

    if (input$inputLocationsPort == 'Entire portfolio') {
      #corpTable2 <- corpTable2[which(corpTable2$ParentCorpID == USER$ParentCorpID & corpTable2$RiskYear == input$sliderInputYearPort),]
      corpTable2 <- corpTable2[which(corpTable2$RiskYear == input$sliderInputYearPort),]
    }

    if (input$inputLocationsPort != 'Entire portfolio' & (input$inputLocationsPort %in% portfolios)=="TRUE") {
      portfolio <- input$inputLocationsPort
      portfolio_members <- portfolioSheet %>% filter(portfolioSheet[[portfolio]]==1)
      corpTable2 <- subset(corpTable2, Location %in% portfolio_members$TickerSymbol)
      corpTable2 <- corpTable2[which(corpTable2$RiskYear == input$sliderInputYearPort),]
    }

    values <- corpTable2$ValueAtRisk

    if(input$riskmetric_portfolio=="Absolute") {
	values2plot <- values
	table2use <- corpTable2
        rfnames=corpTable2$RiskFactorName
        axisname <- "Impact ($M)"
    } # endif

    if(input$riskmetric_portfolio=="Relative") {

	# The following produces ct2b and ct2b_nocarbon
	inputTable <- corpTable2
    	source("./server_portfolio_analyze_percent_normalized_value.r", local=TRUE)

    	values2plot <- 100 * ct2b_nocarbon$NormalizedValue
	table2use <- ct2b_nocarbon
    	rfnames <- ct2b_nocarbon$RiskFactorName
    	axisname <- "Relative Impact (percent)"

    }  # endif on percent normalized value

    ncorp <- pull(count(corpTable2))

    #plot_ly(corpTable2, x = ~TCFDCategoryName, y = ~ValueAtRisk, type='bar', text=corpTable2$RiskFactorName, marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(ncorp))) %>%
    plot_ly(table2use, x = ~TCFDCategoryName, y = values2plot, type='bar', text=rfnames, marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(ncorp))) %>%
      layout(yaxis = list(title = axisname), barmode = 'stack', margin = list(l=80,b=100))
  })
  
# ----------------------------
  #Data table
# ----------------------------
  output$corpFinImpactsPort <- DT::renderDataTable({
     #colnames(corpTable2) = c('Location','TCFD Category','Subcategory','Risk Factor','Scenario','Year','Value at Risk ($M)') #someday figure this out
    corpTable2 <- readr::read_csv("./data/scoring_engine/nonphysical/TCSDB_structure.locations.csv.damages.allDFs.withvalues.with.nonphysical.byparentcorp.csv")
    if(input$riskfactor_subset_portfolio=="Chronic physical + Carbon price") corpTable2 <- filter(corpTable2, TCFDSubCatName=="Chronic" | RiskFactorName=="Carbon pricing")
    parentCorp = dbsheet4
    portfolioSheet <- dbsheet15
    ncols <- length(names(portfolioSheet))
    portfolios <- names(portfolioSheet)[5:ncols]

    if (input$inputLocationsPort != 'Entire portfolio' & (input$inputLocationsPort %in% portfolios)=="FALSE") {
        #corpTable2 <- corpTable2[which(corpTable2$ParentCorpID == USER$ParentCorpID & corpTable2$Location == input$inputLocationsPort & corpTable2$RiskYear == input$sliderInputYearPort),]
        corpTable2 <- corpTable2[which(corpTable2$Location == input$inputLocationsPort & corpTable2$RiskYear == input$sliderInputYearPort),]
      }

      if (input$inputLocationsPort == 'Entire portfolio') {
        #corpTable2 <- corpTable2[which(corpTable2$ParentCorpID == USER$ParentCorpID & corpTable2$RiskYear == input$sliderInputYearPort),]
        corpTable2 <- corpTable2[which(corpTable2$RiskYear == input$sliderInputYearPort),]
      }

    if (input$inputLocationsPort != 'Entire portfolio' & (input$inputLocationsPort %in% portfolios)=="TRUE") {
      portfolio <- input$inputLocationsPort
      portfolio_members <- portfolioSheet %>% filter(portfolioSheet[[portfolio]]==1)
      corpTable2 <- subset(corpTable2, Location %in% portfolio_members$TickerSymbol)
      corpTable2 <- corpTable2[which(corpTable2$RiskYear == input$sliderInputYearPort),]
    }

    values <- corpTable2$ValueAtRisk

    if(input$riskmetric_portfolio=="Absolute") {
	values2plot <- values
	table2use <- corpTable2 %>% select(Location:ValueAtRisk)
        rfnames=corpTable2$RiskFactorName
        axisname <- "Impact ($M)"
    } # endif

    if(input$riskmetric_portfolio=="Relative") {

	# The following produces ct2b and ct2b_nocarbon
	inputTable <- corpTable2
    	source("./server_portfolio_analyze_percent_normalized_value.r", local=TRUE)

    	values2plot <- 100 * ct2b_nocarbon$NormalizedValue
	table2use <- ct2b_nocarbon %>% select(Location:ValueAtRisk, CompanyAssetsThisRisk, NormalizedValue)
	table2use$NormalizedValue <- round(100*table2use$NormalizedValue,2)
    	rfnames <- ct2b_nocarbon$RiskFactorName
    	axisname <- "Relative Impact (percent)"

    }  # endif on percent normalized value

      #corpTable2[1:7]
      table2use
  })

