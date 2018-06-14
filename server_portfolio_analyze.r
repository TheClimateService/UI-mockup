
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
    #selectInput('inputLocationsPort',"Locations",c('All locations', unique(subset(corpLocations, ParentCorpID == USER$ParentCorpID, select = LocationName))),selected='All locations',selectize = TRUE)
    #selectInput('inputLocationsPort',"Portfolios",c('Entire portfolio', "HMC", unique(subset(parentCorp, select = TickerSymbol))),selected='All locations',selectize = TRUE)
    # Portfolios are defined in sheet 15 of TCSDB_structure.xlsx
    ncols <- length(names(dbsheet15))
    portfolios <- names(dbsheet15)[5:ncols]
    selectInput('inputLocationsPort',"Portfolios",c('Entire portfolio', portfolios, unique(subset(parentCorp, select = TickerSymbol))),selectize = TRUE)
  })
  
  output$selectInput_scenarioPort <- renderUI({
    selectInput('inputScenariosPort',"Scenario",c(unique(as.character(corpTable2$ScenarioName))),selected='RCP8.5',selectize = TRUE)
  })
  
  #barByRiskFactor
  output$barByRiskFactorPort <- renderPlotly({
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
    }

    plot_ly(x=corpTable2$ValueAtRisk, y=corpTable2$RiskFactorName, type = 'bar', orientation = 'h') %>% layout(margin = list(l=180, b=100)) %>%
      layout(xaxis = list(title = 'Impact ($M)'))
  }) 
  
  #barByLocation
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

    ncorp <- pull(count(corpTable2))
    plot_ly(corpTable2, x = ~Location, y = ~ValueAtRisk, type='bar', text=corpTable2$RiskFactorName, marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(ncorp))) %>%
      layout(yaxis = list(title = 'Impact ($M)'), barmode = 'stack', margin = list(l=80,b=100))
  }) 
  
  #stacked area by Time
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

    # to chart a time series, need to build a new datafame with additive traces. I'm sure there's a better way to do this.
    nriskyears = length(corpTable2 %>% group_by(RiskYear) %>% summarise(svar=sum(ValueAtRisk)) %>% select(RiskYear) %>% pull())
    time_series = as.data.frame( matrix(0, nrow = nriskyears, ncol = 10, dimnames = list(c(1:nriskyears), c("RiskYear", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9"))) )

    time_series$RiskYear <- corpTable2 %>% group_by(RiskYear) %>% summarise(svar=sum(ValueAtRisk)) %>% select(RiskYear) %>% pull()
    time_series$s1 <- corpTable2 %>% filter(TCFDSubCatName=='Policy and Legal') %>% group_by(RiskYear) %>% summarise(s1=sum(ValueAtRisk)) %>% select(s1) %>% pull()
    time_series$s2 <- corpTable2 %>% filter(TCFDSubCatName=='Technology') %>% group_by(RiskYear) %>% summarise(s2=sum(ValueAtRisk)) %>% select(s2) %>% pull()
    time_series$s3 <- corpTable2 %>% filter(TCFDSubCatName=='Market') %>% group_by(RiskYear) %>% summarise(s3=sum(ValueAtRisk)) %>% select(s3) %>% pull()
    time_series$s4 <- corpTable2 %>% filter(TCFDSubCatName=='Reputation') %>% group_by(RiskYear) %>% summarise(s4=sum(ValueAtRisk)) %>% select(s4) %>% pull()
    time_series$s5 <- corpTable2 %>% filter(TCFDSubCatName=='Acute') %>% group_by(RiskYear) %>% summarise(s5=sum(ValueAtRisk)) %>% select(s5) %>% pull()
    time_series$s6 <- corpTable2 %>% filter(TCFDSubCatName=='Chronic') %>% group_by(RiskYear) %>% summarise(s6=sum(ValueAtRisk)) %>% select(s6) %>% pull()
    time_series$s7 <- corpTable2 %>% filter(TCFDSubCatName=='Resource Efficiency') %>% group_by(RiskYear) %>% summarise(s7=sum(ValueAtRisk)) %>% select(s7) %>% pull()
    time_series$s8 <- corpTable2 %>% filter(TCFDSubCatName=='Energy Source') %>% group_by(RiskYear) %>% summarise(s8=sum(ValueAtRisk)) %>% select(s8) %>% pull()
    time_series$s9 <- corpTable2 %>% filter(TCFDSubCatName=='Resilience') %>% group_by(RiskYear) %>% summarise(s9=sum(ValueAtRisk)) %>% select(s9) %>% pull()
    
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
      layout(yaxis = list(title = 'Impact ($M)', showgrid = FALSE), xaxis = list(showgrid = FALSE), margin = list(l=80,b=100))
    } # endif

   tplot

  }) 
  
  # TCFD stacked bar chart
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

    ncorp <- pull(count(corpTable2))
    plot_ly(corpTable2, x = ~TCFDCategoryName, y = ~ValueAtRisk, type='bar', text=corpTable2$RiskFactorName, marker = list(color = colorRampPalette(brewer.pal(11,"Spectral"))(ncorp))) %>%
      layout(yaxis = list(title = 'Impact ($M)'), barmode = 'stack', margin = list(l=80,b=100))
  })
  
  #Data table
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

      corpTable2[1:7]
  })

