## app.R ##
# Reference:  	https://rstudio.github.io/shinydashboard/
# Icons:	http://fontawesome.io/icons/
#		http://getbootstrap.com/components/#glyphicons

# Notes:
#
#


server <- function(input, output, sessionn) {

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

  output$corpFinImpacts = renderDataTable({
    iris
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))


  output$frame <- renderUI({
    input$Member
    # iframe finds its target source in the www directory.
    my_test <- tags$iframe(src="map.html", height=300, width=300)
    print(my_test)
    my_test
  })

  output$frame_singapore <- renderUI({
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

  # Data
    source("./data/transfer_functions/load_database_transfer_functions.r")
    source("./data/users/load_database_users.r")

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

  output$climplot1 <- renderPlot({
     #test.hist = read.table("./data/tasmax_day_BCSD_historical_r1i1p1_inmcm4_1950-2005.interpolated.merged.aggregated", header=TRUE)
     # The original data set contains 159650 daily tasmax values:  10 years, 31 days covering June, 5 models, 103 NEX-GDDP grid centers in Western Equatoria region of South Sudan.  This has been downselected to fewer points to reduce loading speed.
     test.hist = read.table("./data/nex_gddp_western_equatoria_103pts_5models/tasmax.1970-1979.allmodels.westernequatoria.jun.csv.10pts", header=FALSE)
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
     test.hist = read.table("./data/nex_gddp_western_equatoria_103pts_5models/tasmax.2020-2029.allmodels.westernequatoria.jun.csv.10pts", header=FALSE)
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
     test.hist = read.table("./data/nex_gddp_western_equatoria_103pts_5models/tasmax.2050-2059.allmodels.westernequatoria.jun.csv.10pts", header=FALSE)
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
     test.hist = read.table("./data/nex_gddp_western_equatoria_103pts_5models/tasmax.2090-2099.allmodels.westernequatoria.jun.csv.10pts", header=FALSE)
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

  output$impactplot_agriculture_brazil <- renderPlot({
    source("./functions/fit_agriculture_brazil_v1.r", local=TRUE)
  })

  output$impactplot_maize_us <- renderPlot({
    source("./functions/fit_maize_yield_us_v1.r", local=TRUE)
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

  # Probability of Exceeding Thresholds
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

  # Probabilistic Impact Estimate
  output$impactestimateplot2 <- renderPlot({

  if (input$impact_selected == "General") {
    x <- seq(270,320,0.1)

# The following fails because the threshold inputs to damagej1/2 are functions of j.
#    damage = damage %>% fdamage(thresholds, shapes, sigmoidlimit, sigmoidsteepness, sigmoidmidpoint, quadraticlimit, quadraticshape, quadraticmidpoint, wt1, wt2)

    source("./functions/damage_impacts.r", local=TRUE)

    write.table(damage, file="./output/damage.csv", row.names = FALSE, col.names = FALSE, sep=" ")
    write.table(impacts, file="./output/impacts.csv", row.names = FALSE, col.names = FALSE, sep=" ")
    write.table(impactbyperiod, file="./output/impactbyperiod.csv", row.names = FALSE, col.names = FALSE, sep=" ")
    write.table(c("impactbyperiod impactbyperiod_relative2baseperiod",paste(impactbyperiod,impactbyperiod_relative2baseperiod)), file="./output/impactbyperiod_base_and_relative2baseperiod.csv", row.names = FALSE, col.names = FALSE, sep=" ")

    plot(impactbyperiod_relative2baseperiod, type="l", lwd=3, lty=1, col=colors[1], xlab="Periods", ylab="Probabilistic Impact(%)", xaxt="n")
	axis(1, at=c(1:length(periods)), labels=periods)
        current_risk = round(impactbyperiod_relative2baseperiod[2], digits=2)
	if(current_risk>=0) {thiscol="blue"} else {thiscol="red"}
        lines(c(2,2),c(0,current_risk), col=thiscol, lwd=2 )
        lines(c(1,length(periods)), c(0,0), col="green" )
        text(3.2,0,"CURRENT IMPACT (%) = ", font=4, col=thiscol)
        text(5.2,0,current_risk, font=4, col=thiscol)
    } #endif

  if (input$impact_selected == "Electricity Load") {
    source("./functions/fit_elec_load_v1.r", local=TRUE)
    source("./functions/damage_impacts_4function_v1.r", local=TRUE)
    plot(impactbyperiod_relative2baseperiod, type="l", lwd=3, lty=1, col=colors[1], xlab="Periods", ylab="Probabilistic Impact", xaxt="n")
	axis(1, at=c(1:length(periods)), labels=periods)
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
  
  if (input$impact_selected == "General") {
    x = range_tempK
    wt1 = input$impactfunctionweight
    wt2 = 1 - wt1
    plot(x,wt1*sigmoid(x,input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(x,input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint), type="l", lwd=3, lty=1, col="red", xlim=c(270,320), ylim=c(-100,100), xlab="Daily Maximum Surface Temperature (degK)", ylab="Relative Impact (%)")
    } #endif

  if (input$impact_selected == "Electricity Load") {
    #return()
    source("./functions/fit_elec_load_v1.r", local=TRUE)
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

  # Climate score by period
  output$scoreplot1 <- renderPlot({
    labels <- c("Without Adaptation","With Adaptation")
    x <- seq(275,315,0.1)
    
    source("./functions/damage_impacts.r", local=TRUE)

    betamultiplier = impactbyperiod
    for(i in 1:length(betamultiplier)) {betamultiplier[i]= 1 / (1 + impactbyperiod_relative2baseperiod[i]/100 )  }

    # Climate score calculation
    score <- betamultiplier
    for(i in 1:length(betamultiplier)) {score[i] = 100 - 10*betamultiplier[i]^i}
    adaptation_adjustment <- c(0,0,0,0,5,10,15,20,20)
      if(input$adaptationplan=="None") {adaptation_adjustment = c(0,0,0,0,0,0,0,0,0)}
      if(input$adaptationplan=="Minimal") {adaptation_adjustment = c(0,0,1,5,5,5,5,5,5)}
      if(input$adaptationplan=="Moderate") {adaptation_adjustment = c(0,0,2,10,10,15,15,20,20)}
      if(input$adaptationplan=="Maximal") {adaptation_adjustment = c(0,0,3,10,15,20,20,30,30)}
    score2 = score + adaptation_adjustment
    plot(score, type="l", lwd=3, lty=1, col=colors[1], xlab="Periods", ylab="Climate Score", xaxt="n", ylim=c(80,120) )
	axis(1, at=c(1:length(periods)), labels=periods)
    lines(score2, lwd=3, lty=2, col=colors[2] )
    legend("topleft", inset=.01, title="Scenarios", labels, lwd=3, lty=c(1,2), col=c(colors[1],colors[2]))
  })

   output$adaptationplan <- renderText({
      if(input$adaptationplan=="None") {adaptation_adjustment = "0  0  0  0  0  0  0"}
      if(input$adaptationplan=="Minimal") {adaptation_adjustment = "1  5  5  5  5  5  5"}
      if(input$adaptationplan=="Moderate") {adaptation_adjustment = "2  10  10  15  15  20  20"}
      if(input$adaptationplan=="Maximal") {adaptation_adjustment = "3  10  15  20  20  30  30"}
      paste("CREDITS TO CLIMATE SCORE BY PERIOD:   ", adaptation_adjustment)
    })

  # Impact function from impact-function tab.
  output$scoreplot2 <- renderPlot({
    x <- seq(270,320,0.01)
    wt1 = input$impactfunctionweight
    wt2 = 1 - wt1
    plot(x,wt1*sigmoid(x,input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(x,input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint), type="l", lwd=3, lty=1, col="red", xlim=c(270,320), ylim=c(-100,100), xlab="Daily Maximum Surface Temperature (degK)", ylab="Relative Impact")
  })

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

# Terry -----------------------------------------------------------


} # end server


