## app.R ##
# Reference:  	https://rstudio.github.io/shinydashboard/
# Icons:	http://fontawesome.io/icons/
#		http://getbootstrap.com/components/#glyphicons

# Notes:
#
#


server <- function(input, output, session) {

# -----------
# Log in
# -----------
  
  observeEvent(input$btnLogin, {updateTabItems(session, 'sidebar', 'corporate')})
  
  
   output$login_response <- renderText({
      m = ""
      loggedIn=FALSE
      if(input$username=="" | input$userpass=="") {m="Please enter user name and password"}
#      if(input$username=="terry") {m="Welcome, terry"} else {m="Please enter user name and password"}
#      if(input$username=="terry" && input$userpass=="terry") {m="Welcome, terry"}
      if(input$username=="terry") {m="Welcome, terry"}
      if(input$username=="james" & input$userpass=="james") {
          loggedIn=TRUE
          m="Welcome, James"
          enable('btnLogin')
          }
      paste("",m)
    })



# James start -----------------------------------------------------------

  # traffic light text for corp risk analyzer
  
  txtImpactColor1 <- reactive({
    input$siTimeframe
    name <- switch(input$siTimeframe,'green','green','green','green','yellow','yellow','yellow','yellow','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red')
    return( name=name )
  })
  
  txtImpactColor2 <- reactive({
    input$siTimeframe
    name <- switch(input$siTimeframe,'green','green','green','green','green','green','yellow','yellow','yellow','yellow','yellow','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red')
    return( name=name )
  })
  
  txtImpactColor3 <- reactive({
    input$siTimeframe
    name <- switch(input$siTimeframe,'green','green','yellow','yellow','yellow','yellow','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red','red')
    return( name=name )
  })
  
  EPSVAR <- reactive({
    input$siTimeframe
    epsvarvalue <- switch(input$siTimeframe,'0.04','0.08','0.12','0.16','0.2','0.25','0.30','0.36','0.42','0.5','0.58','0.65','0.76','1.0','1.2','1.4','1.6','1.8','2.0','2.2','2.4','2.5','2.6','2.7','2.8','2.9','3.0','3.1','3.2','3.3')
    return( epsvarvalue=epsvarvalue )
  })
  
  output$infobox1 <- renderInfoBox({
    x <- input$siTimeframe
    color <- 'yellow'
    if(x < 4) color <- 'green'
    if(x > 10) color <- 'red'
    infoBox(value = EPSVAR(), title = 'EPS Value at Risk', color = color, icon = icon("percent"))
  })
  
  output$txtImpact1 <- renderUI({
        div( HTML(sprintf("<text style='background-color:%s'>HVAC systems %s likely to be overloaded</text>", txtImpactColor1(), input$siTimeframe) ) )
  })
  
  output$txtImpact2 <- renderUI({
        div( HTML(sprintf("<text style='background-color:%s'>Drainage systems %s likely to be overloaded</text>", txtImpactColor2(), input$siTimeframe) ) )
  })
  
  output$txtImpact3 <- renderUI({
        div( HTML(sprintf("<text style='background-color:%s'>Energy costs for cooling %s likely to increase significantly</text>", txtImpactColor3(), input$siTimeframe) ) )
  })
  
    observeEvent(input$toProjects, {
    updateTabItems(session, "sidebar", "projects")
  })

  set.seed(122)
  histdata <- rnorm(500)

  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)

  output$mymap <- renderLeaflet({
    leaflet() #%>%
    #   addProviderTiles(providers$Stamen.TonerLite,
    #                    options = providerTileOptions(noWrap = TRUE)
    #   ) %>%
    #   addMarkers(data = points())
  })

# Reading corporate Database
  con = dbConnect(drv=RSQLite::SQLite(), dbname="data/users/db.sqlite3")
  dbSendQuery(conn = db, "CREATE TABLE ValueAtRisk
              (LocationID INTEGER,TCFDCategory TEXT,TCFDSubCat TEXT,ScenarioID INTEGER,)")
  
  
  riskCategories <- c('Transition','Transition','Transition','Transition','Transition','Physical','Physical','Physical','Physical','Opportunity','Opportunity','Opportunity','Opportunity','Opportunity')
  riskSubCat <- c('Policy & Legal','Policy & Legal','Technology','Market','Reputation','Acute','Chronic','Chronic','Chronic','Resource Efficiency','Resource Efficiency','Resource Efficiency','Energy Source','Resilience')
  riskFactors <- c('Increased pricing of GHG emissions','Exposure to litigation','Costs to transition to lower emissions technology',
                   'Increased stakeholder concern or negative stakeholder feedback','Increased cost of raw materials',
                   'Increased severity of extreme weather events such as cyclones and floods',
                   'Changes in precipitation patterns and extreme variability in weather patterns',
                   'Rising mean temperatures','Rising sea levels','Energy efficiency','Water efficiency','Materials efficiency',
                  'Use of lower-emission sources of energy','Renewable energy')
  riskVaR <- c(3.4,2.3,1.2,4.5,2.5,3.4,2.3,1.2,4.5,2.4,2.1,1.8,2.6,1.2)

  corpTable = data.frame(riskCategories,riskSubCat,riskFactors,riskVaR)
    
  output$corpFinImpacts <- DT::renderDataTable({
    colnames(corpTable) = c('TCFD Categories','Subcategory','Risk Factor','Value at Risk ($M)')
    # corpTable %>% DT::formatCurrency('Value at Risk ($M)', currency = '$')  not sure why this doesn't work
    corpTable
  })

  output$stackedCorpFinImpactsPlot <- renderPlotly({
    plot_ly(corpTable, x = ~riskCategories, y = ~riskVaR, type='bar', name='Risk Factors',text=riskFactors,
            #this color list needs to be generated programmatically if we can figure out the function, and if it seems useful
            marker = list(color = c('red', 'yellow','green','red','yellow', 'red','yellow','green','red', 'green','yellow','green','red', 'green'))) %>% 
      layout(yaxis = list(title = 'Impact ($M)'), barmode = 'stack')
  })

  # Old plot for testing - interesting, but probably not a keeper.    
  # output$corpFinImpactsPlot <- renderPlotly({
  #   plot_ly(corpTable, x = ~riskFactors, y = ~riskVaR, type='bar', name='Risk Factors')
  # })

  
  output$map_micron_boise <- renderUI({
    input$Member
    # iframe finds its target source in the www directory.
    thismap <- tags$iframe(src="map.html", height=300, width=300)
    thismap
  })

  output$map_micron_singapore <- renderUI({
    # iframe finds its target source in the www directory.
    thismap <- tags$iframe(src="map_singapore.html", height=300, width=300)
    thismap
  })

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
# James end -----------------------------------------------------------


# Terry -----------------------------------------------------------

  # Functions
    source("./functions/fit_distributions.r")
    source("./functions/sigmoid.r")
    source("./functions/quadratic.r")
    source("./data/sealevel_us/function_annual_probability_withslr.r")

  # Data
    source("./data/transfer_functions/load_database_transfer_functions.r")
    source("./data/users/load_database_users.r")
    #source("./data/users/write_dbsqlite_test.r")
    fl_dept <- extract_hazus_functions()

  # Constants
    range_tempK = seq(270,320,0.01)
    # x-axis tickmarks are not labelled properly if the first period below is longer than 7 characters.
    periods <- c("1976-05", "2016-25", "2026-35", "2036-45", "2046-55", "2056-65", "2066-75", "2076-85", "2086-95")
    shapes <- c(81,82,83,84,85,86,87,88,89)
    scales <- c(292,293,294,295,296,297,298,299,300)
    thresholds <- c(285,290,295,300,305,310)
    initializer <- c(0,0,0,0,0,0,0)
    colors <- brewer.pal(length(thresholds), "Spectral")
    ltypes <- c(1:length(thresholds))
    labels <- c("285K","290K","295K","300K","305K","310K")
    slrScenarios = c("0.3_-_MED","0.5_-_MED","1.0_-_MED","1.5_-_MED","2.0_-_MED","2.5_-_MED")
    slrYears = c(2020,2030,2040,2050,2060,2070,2080,2090,2100)

# -----------
# Portfolio analysis
# -----------

  output$stockselected <- renderText({
#	stock_parameters = filter(stocks_nasdaq,Symbol==input$selected_nasdaq)
	stock_parameters = filter(stocks_nasdaq,Security.Name==input$selected_nasdaq)
	symbol = stock_parameters[1,1]
	name = stock_parameters[1,2]
	paste(symbol, "==", name,"== NASDAQ")
	})

  observeEvent(input$add2portfolio, {session$sendCustomMessage(type = 'testmessage',
      message = "This company will be added to the portfolio.") })

  output$stock_financial_parameters <- renderText({
	stock_parameters = filter(stocks_nasdaq_parameters,Security.Name==input$selected_nasdaq)
	p1 = stock_parameters[1,3]
	p2 = stock_parameters[1,4]
	p3 = stock_parameters[1,5]
	paste(p1,p2,p3)
	# Use the following with htmlOutput in ui.R
	# if(p3 > 10){return(paste("<span style=\"color:red\">This is red text</span>"))} 
       	#        else{return(paste("<span style=\"color:blue\">This is blue text</span>"))}
	})

  output$stock_financial_factors <- renderText({
	stock_parameters = filter(stocks_nasdaq_factors,Security.Name==input$selected_nasdaq)
	p1 = stock_parameters[1,3]
	p2 = stock_parameters[1,4]
	p3 = stock_parameters[1,5]
	paste(p1,p2,p3,sep="\n")
	})

  output$stock_overall_score_gauge <- renderGauge({
	stock_parameters = filter(stocks_nasdaq_parameters,Security.Name==input$selected_nasdaq)
	p = round( sum(stock_parameters[1,3:14])/12 )
	gauge(p, min = 0, max = 1000, symbol = '', 
            gaugeSectors( success = c(800, 1000), warning = c(400, 790), danger = c(0, 390))
            )
	})

  output$stock_financial_gauge1 <- renderGauge({
	stock_parameters = filter(stocks_nasdaq_parameters,Security.Name==input$selected_nasdaq)
	p = stock_parameters[1,3]
	gauge(p, min = 0, max = 1000, symbol = '', 
            gaugeSectors( success = c(800, 1000), warning = c(400, 790), danger = c(0, 390))
            )
	})

  output$stock_financial_gauge2 <- renderGauge({
	stock_parameters = filter(stocks_nasdaq_parameters,Security.Name==input$selected_nasdaq)
	p = stock_parameters[1,4]
	gauge(p, min = 0, max = 1000, symbol = '', 
            gaugeSectors( success = c(800, 1000), warning = c(400, 790), danger = c(0, 390))
            )
	})

  output$stock_financial_gauge3 <- renderGauge({
	stock_parameters = filter(stocks_nasdaq_parameters,Security.Name==input$selected_nasdaq)
	p = stock_parameters[1,5]
	gauge(p, min = 0, max = 1000, symbol = '', 
            gaugeSectors( success = c(800, 1000), warning = c(400, 790), danger = c(0, 390))
            )
	})

  output$stock_transition_parameters <- renderText({
	stock_parameters = filter(stocks_nasdaq_parameters,Security.Name==input$selected_nasdaq)
	p1 = stock_parameters[1,6]
	p2 = stock_parameters[1,7]
	p3 = stock_parameters[1,8]
	paste(p1,p2,p3)
	})

  output$stock_transition_factors <- renderText({
	stock_parameters = filter(stocks_nasdaq_factors,Security.Name==input$selected_nasdaq)
	p1 = stock_parameters[1,6]
	p2 = stock_parameters[1,7]
	p3 = stock_parameters[1,8]
	paste(p1,p2,p3,sep="\n")
	})

  output$stock_transition_gauge1 <- renderGauge({
	stock_parameters = filter(stocks_nasdaq_parameters,Security.Name==input$selected_nasdaq)
	p = stock_parameters[1,6]
	gauge(p, min = 0, max = 1000, symbol = '', 
            gaugeSectors( success = c(800, 1000), warning = c(400, 790), danger = c(0, 390))
            )
	})

  output$stock_transition_gauge2 <- renderGauge({
	stock_parameters = filter(stocks_nasdaq_parameters,Security.Name==input$selected_nasdaq)
	p = stock_parameters[1,7]
	gauge(p, min = 0, max = 1000, symbol = '', 
            gaugeSectors( success = c(800, 1000), warning = c(400, 790), danger = c(0, 390))
            )
	})

  output$stock_transition_gauge3 <- renderGauge({
	stock_parameters = filter(stocks_nasdaq_parameters,Security.Name==input$selected_nasdaq)
	p = stock_parameters[1,8]
	gauge(p, min = 0, max = 1000, symbol = '', 
            gaugeSectors( success = c(800, 1000), warning = c(400, 790), danger = c(0, 390))
            )
	})

  output$stock_physical_parameters <- renderText({
	stock_parameters = filter(stocks_nasdaq_parameters,Security.Name==input$selected_nasdaq)
	p1 = stock_parameters[1,9]
	p2 = stock_parameters[1,10]
	p3 = stock_parameters[1,11]
	paste(p1,p2,p3)
	})

  output$stock_physical_factors <- renderText({
	stock_parameters = filter(stocks_nasdaq_factors,Security.Name==input$selected_nasdaq)
	p1 = stock_parameters[1,9]
	p2 = stock_parameters[1,10]
	p3 = stock_parameters[1,11]
	paste(p1,p2,p3,sep="\n")
	})

  output$stock_physical_gauge_temperature <- renderGauge({
	stock_parameters = filter(stocks_nasdaq_parameters,Security.Name==input$selected_nasdaq)
	p = stock_parameters[1,9]
	# Score below is weighted value of percentage electrical load increases from /functions/damage_impacts_4function_elec_load.r .
	score_input = read.table("./output/score_input_elec_load.csv")
	if(input$selected_nasdaq=="Micron Technology, Inc. - Common Stock") p = 1000 - round(10*as.numeric(score_input))
	gauge(p, min = 0, max = 1000, symbol = '', 
            gaugeSectors( success = c(800, 1000), warning = c(400, 790), danger = c(0, 390))
            )
	})

  output$stock_physical_gauge_slr <- renderGauge({
	stock_parameters = filter(stocks_nasdaq_parameters,Security.Name==input$selected_nasdaq)
	p = stock_parameters[1,10]
	# Score below is weighted value of percentage flood damage from /functions/damage_impacts_4function_hazus_flood_depth_damage.r .
	score_input = read.table("./output/score_input_flood.csv")
	if(input$selected_nasdaq=="Micron Technology, Inc. - Common Stock") p = 1000 - round(10*as.numeric(score_input))
	gauge(p, min = 0, max = 1000, symbol = '', 
            gaugeSectors( success = c(800, 1000), warning = c(400, 790), danger = c(0, 390))
            )
	})

  output$stock_physical_gauge_drought <- renderGauge({
	stock_parameters = filter(stocks_nasdaq_parameters,Security.Name==input$selected_nasdaq)
	p = stock_parameters[1,11]
	# Score below is weighted value of percentage drought damage computed below when input$impact_selected == "Corn Yield (US, drought)" .
	score_input = read.table("./output/score_input_drought.csv")
	if(input$selected_nasdaq=="Micron Technology, Inc. - Common Stock") p = 1000 - round(10*as.numeric(score_input))
	gauge(p, min = 0, max = 1000, symbol = '', 
            gaugeSectors( success = c(800, 1000), warning = c(400, 790), danger = c(0, 390))
            )
	})

  output$stock_opportunity_parameters <- renderText({
	stock_parameters = filter(stocks_nasdaq_parameters,Security.Name==input$selected_nasdaq)
	p1 = stock_parameters[1,12]
	p2 = stock_parameters[1,13]
	p3 = stock_parameters[1,14]
	paste(p1,p2,p3)
	})

  output$stock_opportunity_factors <- renderText({
	stock_parameters = filter(stocks_nasdaq_factors,Security.Name==input$selected_nasdaq)
	p1 = stock_parameters[1,12]
	p2 = stock_parameters[1,13]
	p3 = stock_parameters[1,14]
	paste(p1,p2,p3,sep="\n")
	})

  output$stock_opportunity_gauge1 <- renderGauge({
	stock_parameters = filter(stocks_nasdaq_parameters,Security.Name==input$selected_nasdaq)
	p = stock_parameters[1,12]
	gauge(p, min = 0, max = 1000, symbol = '', 
            gaugeSectors( success = c(800, 1000), warning = c(400, 790), danger = c(0, 390))
            )
	})

  output$stock_opportunity_gauge2 <- renderGauge({
	stock_parameters = filter(stocks_nasdaq_parameters,Security.Name==input$selected_nasdaq)
	p = stock_parameters[1,13]
	gauge(p, min = 0, max = 1000, symbol = '', 
            gaugeSectors( success = c(800, 1000), warning = c(400, 790), danger = c(0, 390))
            )
	})

  output$stock_opportunity_gauge3 <- renderGauge({
	stock_parameters = filter(stocks_nasdaq_parameters,Security.Name==input$selected_nasdaq)
	p = stock_parameters[1,14]
	gauge(p, min = 0, max = 1000, symbol = '', 
            gaugeSectors( success = c(800, 1000), warning = c(400, 790), danger = c(0, 390))
            )
	})

# -----------
# Climate variables
# -----------
  output$climplot1 <- renderPlot({
     #test.hist = read.table("./data/tasmax_day_BCSD_historical_r1i1p1_inmcm4_1950-2005.interpolated.merged.aggregated", header=TRUE)
     # The original data set contains 159650 daily tasmax values:  10 years, 31 days covering June, 5 models, 103 NEX-GDDP grid centers in Western Equatoria region of South Sudan.  This has been downselected to fewer points to reduce loading speed.
     test.hist = read.table("./data/temperature/nex_gddp_western_equatoria_103pts_5models/tasmax.1970-1979.allmodels.westernequatoria.jun.csv.10pts", header=FALSE)
     test.hist[,1] <- NULL
     test.hist[,1] <- NULL
     x = ts(test.hist[1,])
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #hist(x, breaks = bins, col = 'skyblue', border = 'white', main="1950-2005, inmcm4", xlab="Degrees K")
    hist(x, breaks = bins, col = 'skyblue', border = 'white', main="June 1970-1979, 5 models, Western Equatoria", xlab="Degrees K")
  })

  output$climplot2 <- renderPlot({
     #test.hist = read.table("./data/tasmax_day_BCSD_rcp85_r1i1p1_inmcm4_2006-2100.interpolated.merged.aggregated", header=TRUE)
     # The original data set contains 159650 daily tasmax values:  10 years, 31 days covering June, 5 models, 103 NEX-GDDP grid centers in Western Equatoria region of South Sudan.  This has been downselected to fewer points to reduce loading speed.
     test.hist = read.table("./data/temperature/nex_gddp_western_equatoria_103pts_5models/tasmax.2020-2029.allmodels.westernequatoria.jun.csv.10pts", header=FALSE)
     test.hist[,1] <- NULL
     test.hist[,1] <- NULL
     x = ts(test.hist[1,])
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #hist(x, breaks = bins, col = 'yellow', border = 'white', main="2006-2100, RCP8.5, inmcm4", xlab="Degrees K")
    hist(x, breaks = bins, col = 'skyblue', border = 'white', main="June 2020-2029, 5 models, Western Equatoria", xlab="Degrees K")
  })

  output$climplot3 <- renderPlot({
     #test.hist = read.table("./data/tasmax_day_BCSD_historical_r1i1p1_CNRM-CM5_1950-2005.interpolated.merged.aggregated", header=TRUE)
     # The original data set contains 159650 daily tasmax values:  10 years, 31 days covering June, 5 models, 103 NEX-GDDP grid centers in Western Equatoria region of South Sudan.  This has been downselected to fewer points to reduce loading speed.
     test.hist = read.table("./data/temperature/nex_gddp_western_equatoria_103pts_5models/tasmax.2050-2059.allmodels.westernequatoria.jun.csv.10pts", header=FALSE)
     test.hist[,1] <- NULL
     test.hist[,1] <- NULL
     x = ts(test.hist[1,])
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #hist(x, breaks = bins, col = 'skyblue', border = 'white', main="1950-2005, CNRM-CM5", xlab="Degrees K")
    hist(x, breaks = bins, col = 'skyblue', border = 'white', main="June 2050-2059, 5 models, Western Equatoria", xlab="Degrees K")
  })

  output$climplot4 <- renderPlot({
     #test.hist = read.table("./data/tasmax_day_BCSD_rcp85_r1i1p1_CNRM-CM5_2006-2100.interpolated.merged.aggregated", header=TRUE)
     # The original data set contains 159650 daily tasmax values:  10 years, 31 days covering June, 5 models, 103 NEX-GDDP grid centers in Western Equatoria region of South Sudan.  This has been downselected to fewer points to reduce loading speed.
     test.hist = read.table("./data/temperature/nex_gddp_western_equatoria_103pts_5models/tasmax.2090-2099.allmodels.westernequatoria.jun.csv.10pts", header=FALSE)
     test.hist[,1] <- NULL
     test.hist[,1] <- NULL
     x = ts(test.hist[1,])
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #hist(x, breaks = bins, col = 'yellow', border = 'white', main="2006-2100, RCP8.5, CNRM-CM5", xlab="Degrees K")
    hist(x, breaks = bins, col = 'skyblue', border = 'white', main="June 2090-2099, 5 models, Western Equatoria", xlab="Degrees K")
  })

  output$climplot5 <- renderPlot({
    shapes <- c(81.8730, 93.0240, 88.9460, 84.7620, 95.8550, 90.0690, 86.1060, 90.3700, 91.5810)
    scales <- c(292.0320, 293.0880, 293.0820, 293.3870, 293.7670, 293.9150, 294.5310, 295.7390, 295.7960)
    #colors <- brewer.pal(length(shapes), "Spectral")
    colors <- brewer.pal(length(shapes), "Paired")
    ltypes <- c(1:length(shapes))
    labels <- c("1976-2005", "2016-25", "2026-35", "2036-45", "2046-55", "2056-65", "2066-75", "2076-85", "2086-95")
    x <- seq(275,315,0.1)
    plot(x,dweibull(x,shapes[1],scales[1]), type="l", lwd=3, lty=1, col=colors[1], xlim=c(275,315), ylim=c(0,0.12), xlab="Daily Maximum Surface Temperature (degK)", ylab="Probability Density")
    for(i in 2:length(shapes) ) {
      lines( x, dweibull(x,shapes[i],scales[i]), lwd=2, lty=i, col=colors[i] )
    }
    legend("topright", inset=.05, title="Periods", labels, lwd=3, lty=ltypes, col=colors)
  })

  output$sealevel_extremes_plot1 <- renderPlot({
    shapes <- c(81.8730, 93.0240, 88.9460, 84.7620, 95.8550, 90.0690, 86.1060, 90.3700, 91.5810)
    scales <- c(292.0320, 293.0880, 293.0820, 293.3870, 293.7670, 293.9150, 294.5310, 295.7390, 295.7960)
    #colors <- brewer.pal(length(shapes), "Spectral")
    colors <- brewer.pal(length(shapes), "Paired")
    ltypes <- c(1:length(shapes))
    labels <- c("1976-2005", "2016-25", "2026-35", "2036-45", "2046-55", "2056-65", "2066-75", "2076-85", "2086-95")
    x <- seq(275,315,0.1)
	location_parameters = filter(ewl,name==input$extremewaterLocation) %>% select(3:8)
      # z contains the location, scale, and shape parameters in rows 1, 3, and 5 of column 1.
      # z contains the +/-95% confidence interval of these parameters in rows 2, 4, and 6 of column 1.
	z = t(location_parameters)
	loc = z[1,1]
	sc = z[3,1]
	sh = z[5,1]
	return_periods = c(1.01,2,5,10,20,50,100,200,500)
	if(sh > 0) {yt=(-log((1-1/return_periods)))^(-sh);
		    return_levels = ((sc/sh)*(yt-1)) + loc}
	if(sh == 0) {yt=-log((1-1/return_periods));
		    return_levels = loc - sc*log(yt) }
	plot(return_levels, log="x", type="b", xlab="Return Period (years)", ylab="Level (m)", xaxt="n")
	axis(1, at=c(1:length(return_periods)), labels=return_periods)
	#grid(NULL, NULL, lwd=2)
	abline(v=return_periods, h=return_levels, col="gray", lty=3, lwd=2)
#axis(1, at=c(1:6), labels=c("location","location_range","scale","scale_range","shape","shape_param"))
#    plot(x,dweibull(x,shapes[1],scales[1]), type="l", lwd=3, lty=1, col=colors[1], xlim=c(275,315), ylim=c(0,0.12), xlab="Daily Maximum Surface Temperature (degK)", ylab="Probability Density")
#    for(i in 2:length(shapes) ) {
#      lines( x, dweibull(x,shapes[i],scales[i]), lwd=2, lty=i, col=colors[i] )
#    }
#    legend("topright", inset=.05, title="Periods", labels, lwd=3, lty=ltypes, col=colors)
  })

  output$sealevel_projections_plot1 <- renderPlot({
	profile1 = filter(proj,Site==input$sealevelProjectionLocation) %>% filter(Scenario=="0.3_-_MED") %>% select(7:17)
	z = t(profile1)
	profile2 = filter(proj,Site==input$sealevelProjectionLocation) %>% filter(Scenario=="0.5_-_MED") %>% select(7:17)
	z2 = t(profile2)
	profile3 = filter(proj,Site==input$sealevelProjectionLocation) %>% filter(Scenario=="1.0_-_MED") %>% select(7:17)
	z3 = t(profile3)
	profile4 = filter(proj,Site==input$sealevelProjectionLocation) %>% filter(Scenario=="1.5_-_MED") %>% select(7:17)
	z4 = t(profile4)
	profile5 = filter(proj,Site==input$sealevelProjectionLocation) %>% filter(Scenario=="2.0_-_MED") %>% select(7:17)
	z5 = t(profile5)
	profile6 = filter(proj,Site==input$sealevelProjectionLocation) %>% filter(Scenario=="2.5_-_MED") %>% select(7:17)
	z6 = t(profile6)
	plot(z, type="l",lwd=3,col="black", xlab="Year", ylab="Relative Local Sea Level Rise (cm)", xaxt="n")
	lines(z2, lwd=3, col="blue")
	lines(z3, lwd=3, col="green")
	lines(z4, lwd=3, col="yellow")
	lines(z5, lwd=3, col="orange")
	lines(z6, lwd=3, col="red")
	axis(1, at=c(1:11), labels=c("2000","2010","2020","2030","2040","2050","2060","2070","2080","2090","2100"))
     	legend("topleft", inset=.05, title="Scenarios (GMSL 2100)",legend=c("0.3m","0.5m","1.0m","1.5m","2.0m","2.5m"), lwd=3, col=c("black","blue","green","yellow","orange","red"))
  })

  output$returnlevel_probability <- renderText({
	source("./data/sealevel_us/annual_probability_withslr.r", local=TRUE)
	#slrScenarios = c("0.3_-_MED","0.5_-_MED","1.0_-_MED","1.5_-_MED","2.0_-_MED","2.5_-_MED")
	#slrYears = c(2020,2030,2040,2050,2060,2070,2080,2090,2100)
	# slrScenarios and slrYears are defined in and returned as the second and third elements in a list by function_annual_probability_withslr.  The annual_probability_withslr matrix is the first element in the list.
	slrScenarios = function_annual_probability_withslr(input$extremewaterLocation, input$sealevelProjectionLocation, input$returnLevel)[[2]]
	slrYears = function_annual_probability_withslr(input$extremewaterLocation, input$sealevelProjectionLocation, input$returnLevel)[[3]]
	thresholds_flood_m <- c(0,1.5,3,4.5,6,7.5)
	thresholds_flood_m_midpoints = c(0.75, 2.25, 3.75, 5.25, 6.75)
	outputfile = "./output/output_flood_annual_prob.csv"
	annual_probability_withslr_1 = function_annual_probability_withslr(input$extremewaterLocation, input$sealevelProjectionLocation, thresholds_flood_m_midpoints[1])[[1]]
	annual_probability_withslr_2 = function_annual_probability_withslr(input$extremewaterLocation, input$sealevelProjectionLocation, thresholds_flood_m_midpoints[2])[[1]]
	annual_probability_withslr_3 = function_annual_probability_withslr(input$extremewaterLocation, input$sealevelProjectionLocation, thresholds_flood_m_midpoints[3])[[1]]
	annual_probability_withslr_4 = function_annual_probability_withslr(input$extremewaterLocation, input$sealevelProjectionLocation, thresholds_flood_m_midpoints[4])[[1]]
	annual_probability_withslr_5 = function_annual_probability_withslr(input$extremewaterLocation, input$sealevelProjectionLocation, thresholds_flood_m_midpoints[5])[[1]]
	# The table is written explicitly here, rather than in function_annual_probability_     withslr.
	write.table(annual_probability_withslr_1,"./output/output_flood_annual_prob_level1.csv", row.names=slrScenarios, col.names=slrYears)
	write.table(annual_probability_withslr_2,"./output/output_flood_annual_prob_level2.csv", row.names=slrScenarios, col.names=slrYears)
	write.table(annual_probability_withslr_3,"./output/output_flood_annual_prob_level3.csv", row.names=slrScenarios, col.names=slrYears)
	write.table(annual_probability_withslr_4,"./output/output_flood_annual_prob_level4.csv", row.names=slrScenarios, col.names=slrYears)
	write.table(annual_probability_withslr_5,"./output/output_flood_annual_prob_level5.csv", row.names=slrScenarios, col.names=slrYears)

	if(input$slrScenario=="0.3 meter") selectedScenario="0.3_-_MED"
	if(input$slrScenario=="0.5 meter") selectedScenario="0.5_-_MED"
	if(input$slrScenario=="1.0 meter") selectedScenario="1.0_-_MED"
	if(input$slrScenario=="1.5 meters") selectedScenario="1.5_-_MED"
	if(input$slrScenario=="2.0 meters") selectedScenario="2.0_-_MED"
	if(input$slrScenario=="2.5 meters") selectedScenario="2.5_-_MED"
	#selectedScenario = paste(as.character(input$slrScenario),"_-_MED")
	if(input$slrYear=="2020") column=9
	if(input$slrYear=="2030") column=10
	if(input$slrYear=="2040") column=11
	if(input$slrYear=="2050") column=12
	if(input$slrYear=="2060") column=13
	if(input$slrYear=="2070") column=14
	if(input$slrYear=="2080") column=15
	if(input$slrYear=="2090") column=16
	if(input$slrYear=="2100") column=17
	slr_scenario_year = filter(proj,Site==input$sealevelProjectionLocation) %>% filter(Scenario==selectedScenario) %>% select(column)
	location_parameters = filter(ewl,name==input$extremewaterLocation) %>% select(3:8)
      # z contains the location, scale, and shape parameters in rows 1, 3, and 5 of column 1.
      # z contains the +/-95% confidence interval of these parameters in rows 2, 4, and 6 of column 1.
	z = t(location_parameters)
	loc = z[1,1]
	sc = z[3,1]
	sh = z[5,1]
	return_level = as.numeric(input$returnLevel)
	# XXX need to handle sh==0
	if(sh != 0) {return_period = ((sh/sc)*(return_level-loc) + 1)^(1/sh);
		    a = 1/(1+exp(1/return_period)) }
	# Local slr change is in centimeters.
	return_level_withslr = return_level - slr_scenario_year/100
	# XXX need to handle sh==0
	if(sh != 0) {return_period_withslr = ((sh/sc)*(return_level_withslr-loc) + 1)^(1/sh) }
	annual_probability = round(100*1/return_period, digits=2)
	annual_probability_withslr = round(100*1/return_period_withslr, digits=2)
	#paste(input$slrScenario, input$slrYear, slr_scenario_year, "cm", loc,sc,sh,a,return_period,return_period_withslr,annual_probability,"%",annual_probability_withslr,"%")
	#paste("Historical:",annual_probability,"%","Future:",min(annual_probability_withslr,100),"%")
	paste("Historical: ",annual_probability,"% -- ",input$slrYear,":",min(annual_probability_withslr,100),"%")
	})

  output$sealevel_ewl_probabilities <- renderPlot({
	source("./data/sealevel_us/annual_probability_withslr.r", local=TRUE)
	position="topleft"
	if(input$returnLevel==1) position="topright"
    # slrYears are defined in annual_probability_withslr.r.
    # xaxt="n" in plot below turns off xaxis tickmarks.  These are added explicitly with axis.
    plot(annual_probability_withslr[1,], type="l", lwd=3, lty=1, col="black", ylim=c(0,100), xlab="Year", ylab="Annual Probability (%)", xaxt="n")
	lines(annual_probability_withslr[2,], col="blue")
	lines(annual_probability_withslr[3,], col="green")
	lines(annual_probability_withslr[4,], col="yellow")
	lines(annual_probability_withslr[5,], col="orange")
	lines(annual_probability_withslr[6,], col="red")
	axis(1, at=c(1:length(slrYears)), labels=slrYears)
     	legend(position, inset=.05, title="Scenarios (GMSL 2100)",legend=c("0.3m","0.5m","1.0m","1.5m","2.0m","2.5m"), lwd=3, col=c("black","blue","green","yellow","orange","red"))
	})

output$drought_frequencies_lonlat <- renderPlot({
	# Processed drought data is read into dataframe d by load_drought_data.r, which is sourced at the beginning of server.R.
	lon=as.numeric(input$droughtlon)
	lat=as.numeric(input$droughtlat)
	source("./data/drought/process_drought_data.r", local=TRUE)
	#paste(input$droughtlon,input$droughtlat,upperlon,lowerlon,upperlat,lowerlat)
	#paste(as.numeric(d4[3,]), as.numeric(d4[4,]) )
	#plot(as.numeric(d4[3,]), as.numeric(d4[4,]) )
	
	# Fields in d3 used below are lon, lat, and 9 periods defined in load_drought_data.r.
	values = select(d3, V3:V11)
	tvalues = 100*as.numeric( t(values) )
	plot(tvalues, type="l", lwd=3, lty=1, col="black", ylim=c(0,100), xlab="Period", ylab="Annual Probability (%)", xaxt="n")
	axis(1, at=c(1:length(droughtPeriods)), labels=droughtPeriods)
     	legend("topleft", inset=.05, title="Scenarios",legend=c("RCP4.5","RCP8.5"), lwd=3, col=c("black","blue","green","yellow","orange","red"))
     	if(d4[3,1]=="No_data") legend("center", title="NO DATA AVAILABLE AT SPECIFIED LOCATION", legend=" ", bg="red", text.col="white", text.font=2)
	})

output$drought_frequencies_facility <- renderPlot({
	# Processed drought data is read into dataframe d by load_drought_data.r, which is sourced at the beginning of server.R.
	fac_selected = facility_locations %>% filter(facility==input$drought_facility)
	lon=as.numeric(fac_selected[1,2])
	lat=as.numeric(fac_selected[1,3])
	source("./data/drought/process_drought_data.r", local=TRUE)
	#paste(input$droughtlon,input$droughtlat,upperlon,lowerlon,upperlat,lowerlat)
	#paste(as.numeric(d4[3,]), as.numeric(d4[4,]) )
	#plot(as.numeric(d4[3,]), as.numeric(d4[4,]) )
	values = select(d3, V3:V11)
	tvalues = 100*as.numeric( t(values) )
	plot(tvalues, type="l", lwd=3, lty=1, col="black", ylim=c(0,100), xlab="Period", ylab="Annual Probability (%)", xaxt="n")
	axis(1, at=c(1:length(droughtPeriods)), labels=droughtPeriods)
     	legend("topleft", inset=.05, title="Scenarios",legend=c("RCP4.5","RCP8.5"), lwd=3, col=c("black","blue","green","yellow","orange","red"))
     	if(d4[3,1]=="No_data") legend("center", title="NO DATA AVAILABLE AT THIS LOCATION", legend=" ", bg="red", text.col="white", text.font=2)
	})

# -----------
# Impact functions (damage functions)
# -----------
  output$impactplot1 <- renderPlot({
    x = range_tempK
    plot(x,sigmoid(x,input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint), type="l", lwd=3, lty=1, col="red", xlim=c(270,320), ylim=c(-100,100), xlab="Daily Maximum Surface Temperature (degK)", ylab="Relative Impact")
  })

  output$impactplot2 <- renderPlot({
    x = range_tempK
    plot(x,quadratic(x,input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint), type="l", lwd=3, lty=1, col="red", xlim=c(270,320), ylim=c(-100,100), xlab="Daily Maximum Surface Temperature (degK)", ylab="Relative Impact")
  })

  output$impactplot3 <- renderPlot({
    x = range_tempK
    wt1 = input$impactfunctionweight
    wt2 = 1 - wt1
    plot(x,wt1*sigmoid(x,input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(x,input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint), type="l", lwd=3, lty=1, col="red", xlim=c(270,320), ylim=c(-100,100), xlab="Daily Maximum Surface Temperature (degK)", ylab="Relative Impact")
  })

  output$impactplot_elecload <- renderPlot({
    source("./functions/fit_elec_load_v1.r", local=TRUE)
  })

  output$impactplot_building_flood <- renderPlot({
    damage_function_name = as.character(input$hazus_damage_function_id)
	s = unlist( strsplit(damage_function_name, "_") )
        # Given a list structure x, unlist simplifies it to produce a vector which contains all the atomic components which occur in x.
	#s2 = paste(s[1])
    damage_function_id = as.numeric(s[1])
    #fl_dept <- extract_hazus_functions()  # done at start of server.R in data section
    source("./data/hazus/function_extract_hazus_flood_depth_damage.r", local=TRUE)
    get_hazus_damage_function(damage_function_id)
  })

  output$impactplot_corn_drought_return_period <- renderPlot({
    source("./functions/fit_corn_yield_us_drought.r", local=TRUE)
  })

  output$impactplot_agriculture_brazil <- renderPlot({
    source("./functions/fit_agriculture_brazil_v1.r", local=TRUE)
  })

  output$impactplot_maize_us <- renderPlot({
    source("./functions/fit_maize_yield_us_hourly_temp.r", local=TRUE)
  })

  output$comingsoon <- renderImage({
    list(src = "./images/coming_soon.png",width=300,height=300,alt = paste("lobell_crop_yields_2017_fig3_precip"))
  }, deleteFile = FALSE)

  output$impactplot4 <- renderImage({
    #filename <- normalizePath(file.path('./images/lobell_crop_yields_2017_fig1.png'))
    list(src = "./images/lobell_crop_yields_2017_fig1.png",width=300,height=300,alt = paste("lobell_crop_yields_2017_fig1_temperature"))
  }, deleteFile = FALSE)

  output$impactplot5 <- renderImage({
    list(src = "./images/lobell_crop_yields_2017_fig3.png",width=300,height=300,alt = paste("lobell_crop_yields_2017_fig3_precip"))
  }, deleteFile = FALSE)

  output$impactplot6 <- renderImage({
    list(src = "./images/troy_climate_indices_crop_yields_2015_fig2.png",width=300,height=300,alt = paste("troy_climate_indices_crop_yields_2015_fig2_multiplevariables"))
  }, deleteFile = FALSE)

  output$impactplot7 <- renderImage({
    list(src = "./images/carleton_hsiang_climate_dose_response_2016_fig3a.png",width=1000,height=500,alt = paste("carleton_hsiang_climate_dose_response_2016_fig3a_multiplesectors"))
  }, deleteFile = FALSE)

  output$impactplot8 <- renderImage({
    list(src = "./images/carleton_hsiang_climate_dose_response_2016_fig3b.png",width=1000,height=500,alt = paste("carleton_hsiang_climate_dose_response_2016_fig3b_multiplesectors"))
  }, deleteFile = FALSE)

  output$impactplot9 <- renderImage({
    list(src = "./images/thompson_cooling_water_Teffects_v1b_power_airtemp.png",width=600,height=450,alt = paste("thompson_power_generation_air_and_water_temperature"))
  }, deleteFile = FALSE)

  output$impactplot10 <- renderImage({
    list(src = "./images/thompson_cooling_water_Teffects_v1b_water.png",width=500,height=300,alt = paste("thompson_water_needed_water_temperature"))
  }, deleteFile = FALSE)

  output$impactplot_crops_wang <- renderImage({
    list(src = "./images/wang_crop_productivity_climate_midwestUS_2016_fig13.png",width=500,height=300,alt = paste("wang_crop_productivity_climate_midwestUS_2016_fig13"))
  }, deleteFile = FALSE)

  output$impactplot_crops_mishra <- renderImage({
    list(src = "./images/mishra_corn_soy_midwestUS_drought_2010_fig5.png",width=500,height=300,alt = paste("mishra_corn_soy_midwestUS_drought_2010_fig5"))
  }, deleteFile = FALSE)

  output$impactplot_crops_mhakbela <- renderImage({
    list(src = "./images/mhakbela_drought_wheat_canada_2010_fig2.png",width=500,height=300,alt = paste("mhakbela_drought_wheat_canada_2010_fig2"))
  }, deleteFile = FALSE)

# -----------
# Probability of Exceeding Thresholds
# -----------
  # Note that the shapes and scales values are for a pre-determined Weibull distribution for South Sudan.

  output$impactestimateplot1 <- renderPlot({
    #shapes <- c(81.8730, 93.0240, 88.9460, 84.7620, 95.8550, 90.0690, 86.1060, 90.3700, 91.5810)
    #scales <- c(292.0320, 293.0880, 293.0820, 293.3870, 293.7670, 293.9150, 294.5310, 295.7390, 295.7960)
    x <- seq(270,320,0.1)
    # pweibull is the CDF for dweibull.
    probexceed = matrix(0,length(thresholds),length(shapes))
    for(i in 1:length(shapes)) {probexceed[1,i] <- (1.0-pweibull(input$threshold,shapes[i],scales[i]) ) }
    #colnames(probexceed) <- periods
    # xaxt="n" in plot below turns off xaxis tickmarks.  These are added explicitly with axis.
    plot(probexceed[1,], type="l", lwd=3, lty=1, col=colors[1], ylim=c(0,1.0), xlab="Time Periods", ylab="Probability of Exceeding Threshold", xaxt="n")
	axis(1, at=c(1:length(periods)), labels=periods)
#   for(j in 1:length(thresholds)) { for(i in 1:length(shapes)) {probexceed[j,i] <- (1.0-pweibull(thresholds[j],shapes[i],scales[i]) ) } }
#    plot(probexceed[1,], type="l", lwd=3, lty=1, col=colors[1], ylim=c(0,1.0), xlab="Periods", ylab="Probability of Exceeding Threshold")
#    for(i in 2:length(thresholds) ) {
#      lines( probexceed[i,], lwd=3, lty=i, col=colors[i] )
#    }
#    legend("topright", inset=.01, title="Thresholds", labels, lwd=3, lty=ltypes, col=colors)
  })

# -----------
# Probabilistic Impact Estimate
# -----------
  output$impactestimateplot2 <- renderPlot({

  if (input$impact_selected == "Custom-built") {
    x <- seq(270,320,0.1)

# The following fails because the threshold inputs to damagej1/2 are functions of j.
#    damage = damage %>% fdamage(thresholds, shapes, sigmoidlimit, sigmoidsteepness, sigmoidmidpoint, quadraticlimit, quadraticshape, quadraticmidpoint, wt1, wt2)

    source("./functions/damage_impacts.r", local=TRUE)

    write.table(damage, file="./output/damage_custom.csv", row.names = FALSE, col.names = FALSE, sep=" ")
    write.table(impacts, file="./output/impacts_custom.csv", row.names = FALSE, col.names = FALSE, sep=" ")
    write.table(impactbyperiod, file="./output/impactbyperiod_custom.csv", row.names = FALSE, col.names = FALSE, sep=" ")
    write.table(c("impactbyperiod impactbyperiod_relative2baseperiod",paste(impactbyperiod,impactbyperiod_relative2baseperiod)), file="./output/impactbyperiod_base_and_relative2baseperiod_custom.csv", row.names = FALSE, col.names = FALSE, sep=" ")

    plot(impactbyperiod_relative2baseperiod, type="l", lwd=3, lty=1, col=colors[1], xlab="Periods", ylab="Probabilistic Impact(%)", xaxt="n")
	axis(1, at=c(1:length(periods)), labels=periods)
        current_risk = round(impactbyperiod_relative2baseperiod[2], digits=2)
	if(current_risk>=0) {thiscol="blue"} else {thiscol="red"}
        lines(c(2,2),c(0,current_risk), col=thiscol, lwd=2 )
        lines(c(1,length(periods)), c(0,0), col="green" )
        text(3.2,0,"CURRENT IMPACT (%) = ", font=4, col=thiscol)
        text(5.2,0,current_risk, font=4, col=thiscol)
    } #endif

  if (input$impact_selected == "Electricity Load (US; temperature)") {
    source("./functions/fit_elec_load_v1.r", local=TRUE)
    source("./functions/damage_impacts_4function_elec_load.r", local=TRUE)

    write.table(damage, file="./output/damage_elecload.csv", row.names = FALSE, col.names = FALSE, sep=" ")
    write.table(impacts, file="./output/impacts_elecload.csv", row.names = FALSE, col.names = FALSE, sep=" ")
    write.table(impactbyperiod, file="./output/impactbyperiod_elecload.csv", row.names = FALSE, col.names = FALSE, sep=" ")
    write.table(c("impactbyperiod impactbyperiod_relative2baseperiod",paste(impactbyperiod,impactbyperiod_relative2baseperiod)), file="./output/impactbyperiod_base_and_relative2baseperiod_elecload.csv", row.names = FALSE, col.names = FALSE, sep=" ")

    plot(impactbyperiod_relative2baseperiod, type="l", lwd=3, lty=1, col=colors[1], xlab="Periods", ylab="Change in Peak Load (Mw)", xaxt="n")
	axis(1, at=c(1:length(periods)), labels=periods)
    } #endif

  if (input$impact_selected == "Building Damage (flood depth)") {
    #fl_dept <- extract_hazus_functions()  # done at start of server.R in data section
    #source("./data/hazus/extract_hazus_flood_depth_damage.r", local=TRUE)
    #source("./data/hazus/function_extract_hazus_flood_depth_damage.r", local=TRUE)
    source("./data/hazus/function_extract_hazus_flood_depth_damage_return_damage_at_depth.r", local=TRUE)

    #damage_function_id = input$hazus_damage_function_id
    damage_function_name = as.character(input$hazus_damage_function_id)
	s = unlist( strsplit(damage_function_name, "_") )
        # Given a list structure x, unlist simplifies it to produce a vector which contains all the atomic components which occur in x.
    damage_function_id = as.numeric(s[1])
    #get_hazus_damage_function(damage_function_id)

    # The following reads the table of annual probabilities created by ./data/sealevel_us/annual_probability_withslr.r. Note that this is a dynamic table created by the selection of location and return level from the SLR section of the localized climate probabilities tab.  The table has rows for each GMSL scenario and colums for years 2020-2100 in 10-year increments.
    annual_prob_given_return_level = read.table("./output/output_flood_annual_prob.csv", header=TRUE)
    annual_prob_given_return_level1 = read.table("./output/output_flood_annual_prob_level1.csv", header=TRUE)
    source("./functions/damage_impacts_4function_hazus_flood_depth_damage.r", local=TRUE)

    write.table(damage, file="./output/damage_buildingflood.csv", row.names = FALSE, col.names = FALSE, sep=" ")
    write.table(impacts, file="./output/impacts_buildingflood.csv", row.names = FALSE, col.names = FALSE, sep=" ")
    write.table(impactbyperiod, file="./output/impactbyperiod_buildingflood.csv", row.names = FALSE, col.names = FALSE, sep=" ")
    write.table(c("impactbyperiod impactbyperiod_relative2baseperiod",paste(impactbyperiod,impactbyperiod_relative2baseperiod)), file="./output/impactbyperiod_base_and_relative2baseperiod_buildingflood.csv", row.names = FALSE, col.names = FALSE, sep=" ")

    plot(impactbyperiod_relative2baseperiod, type="l", lwd=3, lty=1, col=colors[1], xlab="Periods", ylab="Expected Building Damage for Selected RL (%)", xaxt="n")
	axis(1, at=c(1:length(periods)), labels=periods)
    } #endif

  if (input$impact_selected == "Corn Yield (US, drought)") {
	# Processed drought data is read into dataframe d by load_drought_data.r, which is sourced at the beginning of server.R.
	location_name = "Selected Location"
	lon=as.numeric(input$droughtlon)
	lat=as.numeric(input$droughtlat)
	fac_selected = facility_locations %>% filter(facility==input$drought_facility)
	if(input$use_facility_for_drought=="TRUE") {
		lon=as.numeric(fac_selected[1,2])
		lat=as.numeric(fac_selected[1,3]) 
		location_name = fac_selected[1,1]}
	source("./data/drought/process_drought_data.r", local=TRUE)
	# process_drought_data.r produces the data vector d3.  It also writes this data to /output/output_drought_annual_prob.csv .
	# Fields in d3 used below are lon, lat, and fractional annual probabilities for 9 periods defined in load_drought_data.r.
	# For example, droughtPeriods = c("1950-99","2016-25","2026-35","2036-45","2046-55","2056-65","2066-75","2076-85","2086-95")
	values = select(d3, V3:V11)
	tvalues = 100*as.numeric( t(values) )
        source("./functions/fit_corn_yield_us_drought.r", local=TRUE)
        historical_reduction_10yr_return_period = yield_reduction_pct[4]
        projected_return_period_same_reduction = (10/tvalues) * 10
	annual_expected_reduction = (-1.0*historical_reduction_10yr_return_period) * tvalues/100

    	impactbyperiod = annual_expected_reduction
    	impactbyperiod_relative2baseperiod = impactbyperiod - impactbyperiod[1]
    	write.table(impactbyperiod, file="./output/impactbyperiod_corn_yield_us_drought.csv", row.names = FALSE, col.names = FALSE, sep=" ")
    	write.table(c("impactbyperiod impactbyperiod_relative2baseperiod",paste(impactbyperiod,impactbyperiod_relative2baseperiod)), file="./output/impactbyperiod_base_and_relative2baseperiod_corn_yield_us_drought.csv", row.names = FALSE, col.names = FALSE, sep=" ")

	# Calculate weighted vector of impact by period for climate score.
	weightbyperiod = c(0,80,10,5,1,1,1,1,1)
	#weightbyperiod = c(0,50,50,0,0,0,0,0,0)
	#percent_change = 100*(impactbyperiod/impactbyperiod[1] - 1.0)
	# Impacts on corn yields due to drought are annual percent yield reduction and are negative.
	percent_change = -1.0*(impactbyperiod - impactbyperiod[1])
	score_input_drought = sum( weightbyperiod*percent_change)/100
	#score_input_drought = sum( weightbyperiod*impactbyperiod_relative2baseperiod )/100
	write.table(score_input_drought, "./output/score_input_drought.csv", row.names=FALSE, col.names=FALSE)

	plot(annual_expected_reduction, type="l", lwd=3, lty=1, col="black", main=paste("Yield Change at",location_name,"(",lon,",",lat,")"), xlab="Periods", ylab="Expected Annual Yield Loss (%)", xaxt="n", ylim=c(-1.0*historical_reduction_10yr_return_period, 0))
	axis(1, at=c(1:length(droughtPeriods)), labels=droughtPeriods)
     	legend("topright", inset=.05, title="Scenarios",legend=c("RCP4.5","RCP8.5"), lwd=3, col=c("black","blue","green","yellow","orange","red"))
     	if(d4[3,1]=="No_data") legend("center", title="NO DATA AVAILABLE AT SPECIFIED LOCATION", legend=" ", bg="red", text.col="white", text.font=2)
    } #endif

  if (input$impact_selected == "Agricultural Income (Brazil)") {
    return()
    } #endif

  if (input$impact_selected == "Maize Yield (US)") {
    return()
    } #endif

  })

  # Impact Function for impact estimate (controlled from impact-function tab)
  output$impactestimateplot3 <- renderPlot({
  
  if (input$impact_selected == "Custom-built") {
    x = range_tempK
    wt1 = input$impactfunctionweight
    wt2 = 1 - wt1
    plot(x,wt1*sigmoid(x,input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(x,input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint), type="l", lwd=3, lty=1, col="red", xlim=c(270,320), ylim=c(-100,100), xlab="Daily Maximum Surface Temperature (degK)", ylab="Relative Impact (%)")
    } #endif

  if (input$impact_selected == "Electricity Load (US; temperature)") {
    source("./functions/fit_elec_load_v1.r", local=TRUE)
    } #endif

  if (input$impact_selected == "Building Damage (flood depth)") {
    #fl_dept <- extract_hazus_functions()  # done at start of server.R in data section
    #source("./data/hazus/extract_hazus_flood_depth_damage.r", local=TRUE)
    #damage_function_id = input$hazus_damage_function_id
    damage_function_name = as.character(input$hazus_damage_function_id)
	s = unlist( strsplit(damage_function_name, "_") )
        # Given a list structure x, unlist simplifies it to produce a vector which contains all the atomic components which occur in x.
    damage_function_id = as.numeric(s[1])
    source("./data/hazus/function_extract_hazus_flood_depth_damage.r", local=TRUE)
    get_hazus_damage_function(damage_function_id)
    } #endif

  if (input$impact_selected == "Corn Yield (US, drought)") {
    source("./functions/fit_corn_yield_us_drought.r", local=TRUE)
    } #endif

  if (input$impact_selected == "Agricultural Income (Brazil)") {
    source("./functions/fit_agriculture_brazil_v1.r", local=TRUE)
    } #endif

  if (input$impact_selected == "Maize Yield (US)") {
    source("./functions/fit_maize_yield_us_v1.r", local=TRUE)
    } #endif

  })

  # Beta Multiplier By Period
  output$financialplot1 <- renderPlot({
    x <- seq(275,315,0.1)
    # pweibull is the CDF for dweibull.
    source("./functions/damage_impacts.r", local=TRUE)
    betamultiplier = impactbyperiod
    for(i in 1:length(betamultiplier)) {betamultiplier[i]= 1 / (1 + impactbyperiod_relative2baseperiod[i]/100 )  }
    plot(betamultiplier, type="l", lwd=3, lty=1, col=colors[1], xlab="Periods", ylab="Beta Multiplier", xaxt="n")
	axis(1, at=c(1:length(periods)), labels=periods)
  })

  # NPV impact on project
  output$financialplot2 <- renderPlot({
    x <- seq(275,315,0.1)
    
    source("./functions/damage_impacts.r", local=TRUE)

    betamultiplier = impactbyperiod
    for(i in 1:length(betamultiplier)) {betamultiplier[i]= 1 / (1 + impactbyperiod_relative2baseperiod[i]/100 )  }

    # NPV calculation
    # Assume project starts in period 3 and that each period is a decade.
    cashflowinputs <- c(input$cashflow1,input$cashflow2,input$cashflow3,input$cashflow4,input$cashflow5,input$cashflow6,input$cashflow7)
    cashflow <- initializer
    for(i in 1:length(cashflow)) {cashflow[i] = cashflowinputs[i] }
    capitalcost <- input$capitalcost
    discount <- input$discount
    discountbydecade <- initializer
    discountbydecade[1] = 1 / ((1+discount)^10)
    for(i in 2:length(discountbydecade)) {discountbydecade[i] = discountbydecade[1]^i}
    discountedcashflow <- initializer
    for(i in 1:length(discountbydecade)) {discountedcashflow[i] = discountbydecade[i] * cashflow[i] }
    npv = sum(discountedcashflow) - capitalcost
    npvmultiplier <- initializer
    offset=2
    for(i in 1:length(npvmultiplier)) {npvmultiplier[i] = betamultiplier[i+offset]}
    for(i in 1:length(npvmultiplier)) {npvmultiplier[i] = npvmultiplier[i] - npvmultiplier[1] +1}
    discountedcashflow_modified <- discountedcashflow
    for(i in 1:length(npvmultiplier)) {discountedcashflow_modified[i] = discountedcashflow[i] / npvmultiplier[i]}
    npv_modified = sum(discountedcashflow_modified) - capitalcost

    write.table(c("betamultiplier",betamultiplier), file="./output/betamultiplier.csv", row.names = FALSE, col.names = FALSE, sep=" ")
    write.table(c("npvmultiplier",npvmultiplier), file="./output/npvmultiplier.csv", row.names = FALSE, col.names = FALSE, sep=" ")
    write.table(c("npv npv_modified",paste(npv,npv_modified)), file="./output/npv_base_and_modified.csv", row.names = FALSE, col.names = FALSE, sep=" ")
    write.table(c("discountbydecade",discountbydecade), file="./output/discountbydecade.csv", row.names = FALSE, col.names = FALSE, sep=" ")
    write.table(c("discountedcashflow,discountedcashflow_modified",paste(discountedcashflow,discountedcashflow_modified)), file="./output/discountedcashflow_base_and_modified.csv", row.names = FALSE, col.names = FALSE, sep=" ")

    barplot(c(npv,npv_modified), col=c("green","red"), names.arg=c("Without Climate Impacts", "With Climate Impacts"),xlab="Unmodified and Modified NPV", ylab="NPV", ylim=c(-20,50) )
  })

# -----------
# Climate score
# -----------

  # Climate score by period

  output$scoreplot1 <- renderPlot({
    labels <- c("Without Adaptation","With Adaptation")
    x <- seq(275,315,0.1)
    
    source("./functions/damage_impacts.r", local=TRUE)

    betamultiplier = impactbyperiod
    for(i in 1:length(betamultiplier)) {betamultiplier[i]= 1 / (1 + impactbyperiod_relative2baseperiod[i]/100 )  }

    # Climate score calculation
    score <- betamultiplier
    # for(i in 1:length(betamultiplier)) {score[i] = 100 - 10*betamultiplier[i]^i}
    for(i in 1:length(score)) {score[i] = 100 + impactbyperiod_relative2baseperiod[i]}
    # for(i in 1:length(score)) {score[i] = 100 + impactbyperiod[i]}
    adaptation_adjustment <- c(0,0,0,0,5,10,15,20,20)
      if(input$adaptationplan=="None") {adaptation_adjustment = c(0,0,0,0,0,0,0,0,0)}
      if(input$adaptationplan=="Minimal") {adaptation_adjustment = c(0,0,1,5,5,5,5,5,5)}
      if(input$adaptationplan=="Moderate") {adaptation_adjustment = c(0,0,2,10,10,15,15,20,20)}
      if(input$adaptationplan=="Maximal") {adaptation_adjustment = c(0,0,3,10,15,20,20,30,30)}
    score2 = score + adaptation_adjustment
    plot(score, type="l", lwd=3, lty=1, col=colors[1], xlab="Periods", ylab="Climate Score", xaxt="n", ylim=c(0,max(score)) )
	axis(1, at=c(1:length(periods)), labels=periods)
    lines(score2, lwd=3, lty=2, col=colors[2] )
    legend("topright", inset=.01, title="Scenarios", labels, lwd=3, lty=c(1,2), col=c(colors[1],colors[2]))
  })

   output$climatescores <- renderText({
    x <- seq(275,315,0.1)
    source("./functions/damage_impacts.r", local=TRUE)
    score = impactbyperiod
    for(i in 1:length(score)) {score[i] = 100 + impactbyperiod_relative2baseperiod[i]}
    # for(i in 1:length(score)) {score[i] = 100 + impactbyperiod[i]}
    paste("Near-term (2016-25):", round(score[2],digits=0),"Mid-term (2026-35):", round(score[3],digits=0), "Long-term (2036-45):", round(score[4],digits=0), sep="\n") 
    })

   output$adaptationplan <- renderText({
      if(input$adaptationplan=="None") {adaptation_adjustment = "0  0  0  0  0  0  0  0  0"}
      if(input$adaptationplan=="Minimal") {adaptation_adjustment = "0  0  1  5  5  5  5  5  5"}
      if(input$adaptationplan=="Moderate") {adaptation_adjustment = "0  0  2  10  10  15  15  20  20"}
      if(input$adaptationplan=="Maximal") {adaptation_adjustment = "0  0  3  10  15  20  20  30  30"}
      paste("CREDITS TO CLIMATE SCORE BY PERIOD:   ", adaptation_adjustment)
    })

  # Impact function from impact-function tab.
  output$scoreplot2 <- renderPlot({
    x <- seq(270,320,0.01)
    wt1 = input$impactfunctionweight
    wt2 = 1 - wt1
    plot(x,wt1*sigmoid(x,input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(x,input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint), type="l", lwd=3, lty=1, col="red", xlim=c(270,320), ylim=c(-100,100), xlab="Daily Maximum Surface Temperature (degK)", ylab="Relative Impact")
  })

# -----------
# Adaptation planning
# -----------

  # Impact function from impact-function tab.
  output$adaptationplot1 <- renderPlot({
    x <- seq(270,320,0.01)
    wt1 = input$impactfunctionweight
    wt2 = 1 - wt1
    plot(x,wt1*sigmoid(x,input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(x,input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint), type="l", lwd=3, lty=1, col="red", xlim=c(270,320), ylim=c(-100,100), xlab="Daily Maximum Surface Temperature (degK)", ylab="Relative Impact")
  })

  # Asset-value damage
  output$adaptationplot2 <- renderPlot({
    x <- seq(270,320,0.1)
    nadaptplans = 2
   
  # Asset risk below is sum of values by asset, weighted by sensitivity to given impact function.  Sensitivity varies with adaptation plan.
  #   Impact functions give percent impact, so we allow different assets to have different sensitivities to a given impact function.
  #   This is a simplified way to get different impacts by scaling by asset type; ideally, they would have different impact functions. 
  #   This is also a simplified (linear) way to connect asset value with loss of capacity or condition in the impact function.

    source("./functions/damage_impacts_adaptation.r", local=TRUE)

    labels <- c("No Adaptation","Adaptation Plan 1","Adaptation Plan 2")
    plot(impactbyperiod_relative2baseperiod, type="l", lwd=3, lty=1, col=colors[1], xlab="Periods", ylab="Impact on Asset Value ($M)", xaxt="n")
	axis(1, at=c(1:length(periods)), labels=periods)
    lines(impactbyperiod_relative2baseperiod_plan1, type="l", lwd=3, lty=1, col=colors[3])
    lines(impactbyperiod_relative2baseperiod_plan2, type="l", lwd=3, lty=1, col=colors[5])
    legend("topright", inset=.01, title="Scenarios", labels, lwd=3, lty=c(1,1,1), col=c(colors[1],colors[3],colors[5]))
  })

  # Adaptation-plan benefits.
  # Benefit for plan i = Sum(over periods j) [ (impact_no_adapt(period j) - impact_plan_i(period j)) / (1+discount_rate)^j ]
  output$adaptationbenefit1 <- renderText({
    x <- seq(275,315,0.1)
    source("./functions/damage_impacts_adaptation.r", local=TRUE)

    discount <- input$discount2
    discountbydecade <- initializer
    discountbydecade[1] = 1 / ((1+discount)^10)
    for(i in 2:length(discountbydecade)) {discountbydecade[i] = discountbydecade[1]^i}

    benefitbydecade_plan1 <- initializer
    for(i in 1:length(benefitbydecade_plan1)) {benefitbydecade_plan1[i] = impactbyperiod_relative2noadaptation_plan1[i] / ((1+discountbydecade[i])^i) }
    benefit_plan1 = sum(benefitbydecade_plan1)

    totalcost = (input$cost2implement_plan1 + input$cost2maintain_relative2base_plan1)
    benefit2cost = benefit_plan1 / totalcost

    paste("Adaptation Plan 1 - Benefit, Cost, and B/C Ratio:  ", round(benefit_plan1, digits=2), "$M ,", round(totalcost, digits=2), "$M ,",round(benefit2cost, digits=2) )
  })

  output$adaptationbenefit2 <- renderText({
    x <- seq(275,315,0.1)
    source("./functions/damage_impacts_adaptation.r", local=TRUE)

    discount <- input$discount3
    discountbydecade <- initializer
    discountbydecade[1] = 1 / ((1+discount)^10)
    for(i in 2:length(discountbydecade)) {discountbydecade[i] = discountbydecade[1]^i}

    benefitbydecade_plan2 <- initializer
    for(i in 1:length(benefitbydecade_plan2)) {benefitbydecade_plan2[i] = impactbyperiod_relative2noadaptation_plan2[i] / ((1+discountbydecade[i])^i) }
    benefit_plan2 = sum(benefitbydecade_plan2)

    totalcost = (input$cost2implement_plan2 + input$cost2maintain_relative2base_plan2)
    benefit2cost = benefit_plan2 / totalcost

    paste("Adaptation Plan 2 - Benefit, Cost, and B/C Ratio:  ", round(benefit_plan2, digits=2), "$M ,", round(totalcost, digits=2), "$M ,",round(benefit2cost, digits=2) )
  })

  output$database1_name <- renderText({
    source("./data/users/load_database_users.r", local=TRUE)
    db = dbname
    paste("Database 1:  ", db)
  })

  output$database2_name <- renderText({
    source("./data/users/load_dbsqlite_test.r", local=TRUE)
    db = dbtitle
    paste("Database 2:  ", db)
  })

  output$database2_table12_contents <- renderText({
    source("./data/users/load_dbsqlite_test.r", local=TRUE)
    paste("Contents of table",table12," :  ", qlist)
  })

  output$database2_write <- renderText({
    # XXX next make the sourcing of the write script dependent on an input variable.
    if(input$write_new_table=="Yes") {source("./data/users/write_dbsqlite_test.r", local=TRUE)}
    paste("New table created.")
  })


# -----------
# Links
# -----------
    #googleurl <- a("Google Homepage", href="https://www.google.com/")
    #output$googlelink <- renderUI({ tagList("URL link:", googleurl) })
    output$googlelink <- renderUI({ tags$a("Google Search", href="https:www.google.com", target="_blank") })
    output$ndgain_countries <- renderUI({ tags$a("ND-GAIN Country Index", href="https://gain.nd.edu/our-work/country-index/rankings/", target="_blank") })
    output$actuaries_climate_index <- renderUI({ tags$a("Actuaries Climate Index", href="http://actuariesclimateindex.org/explore/regional-graphs/", target="_blank") })
    output$worldbank_development_indicators <- renderUI({ tags$a("World Bank Development Indicators", href="http://databank.worldbank.org/data/reports.aspx?source=world-development-indicators", target="_blank") })

# Terry -----------------------------------------------------------


} # end server


