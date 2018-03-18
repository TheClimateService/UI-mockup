
  
  #Area by Time for single RiskFactor
  output$areaByTime <- renderPlotly({
      corpTable <- readr::read_csv("./data/scoring_engine/nonphysical/TCSDB_structure.locations.csv.damages.allDFs.withvalues.with.nonphysical.csv")
    if (input$inputLocations != 'All locations') {
      corpTable <- corpTable[which(corpTable$ParentCorpID == USER$ParentCorpID & corpTable$Location == input$inputLocations),]
    }
    if (input$inputLocations == 'All locations') {
      corpTable <- corpTable[which(corpTable$ParentCorpID == USER$ParentCorpID),]
    }
    
    nriskyears = length(corpTable %>% group_by(RiskYear) %>% summarise(svar=sum(ValueAtRisk)) %>% select(RiskYear) %>% pull())
    time_series = as.data.frame( matrix(0, nrow = nriskyears, ncol = 10, dimnames = list(c(1:nriskyears), c("RiskYear", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9"))) )

    time_series$RiskYear <- corpTable %>% group_by(RiskYear) %>% summarise(svar=sum(ValueAtRisk)) %>% select(RiskYear) %>% pull()
    time_series$s1 <- corpTable %>% filter(RiskFactorName=='Temperature extremes') %>% group_by(RiskYear) %>% summarise(s1=sum(ValueAtRisk)) %>% select(s1) %>% pull()
    time_series$s2 <- corpTable %>% filter(RiskFactorName=='Drought') %>% group_by(RiskYear) %>% summarise(s2=sum(ValueAtRisk)) %>% select(s2) %>% pull()
    time_series$s3 <- corpTable %>% filter(RiskFactorName=='Coastal flooding') %>% group_by(RiskYear) %>% summarise(s3=sum(ValueAtRisk)) %>% select(s3) %>% pull()
    time_series$s4 <- corpTable %>% filter(RiskFactorName=='Carbon pricing') %>% group_by(RiskYear) %>% summarise(s4=sum(ValueAtRisk)) %>% select(s4) %>% pull()

  # Note that the plot is saved as object tplot under each conditional; this is needed to avoid the following:
  #    Error in UseMethod: no applicable method for 'ggplotly' applied to an object of class "NULL".

  if(input$selectCausalVariable=="Temperature (daily maximum 90th percentile)") { 
    tplot <- plot_ly(time_series, x = ~RiskYear, y = ~s1, name='Temperature (daily maximum 90th percentile)', type='scatter', mode = 'none', fill = 'tonexty')
    } # endif

  if(input$selectCausalVariable=="Drought Severity (90th percentile)") { 
    tplot <- plot_ly(time_series, x = ~RiskYear, y = ~s2, name='Drought Severity (90th percentile)', type='scatter', mode = 'none', fill = 'tonexty')
    } # endif

  if(input$selectCausalVariable=="Coastal Flooding (return period 100yr level)") { 
    tplot <- plot_ly(time_series, x = ~RiskYear, y = ~s3, name='Coastal Flooding (return period 100yr level)', type='scatter', mode = 'none', fill = 'tonexty')
    } # endif

  if(input$selectCausalVariable=="Carbon Price") { 
    tplot <- plot_ly(time_series, x = ~RiskYear, y = ~s4, name='Carbon Price', type='scatter', mode = 'none', fill = 'tonexty') 
    } # endif

  tplot %>%
       layout(yaxis = list(title = 'Impact ($M)', showgrid = TRUE), xaxis = list(showgrid = TRUE), margin = list(l=80,b=100))

  }) 
  
