## app.R ##
# Reference:  	https://rstudio.github.io/shinydashboard/
# Icons:	http://fontawesome.io/icons/
#		http://getbootstrap.com/components/#glyphicons

library(shinydashboard)
library(leaflet)
library(MASS)
library(survival)
library(RColorBrewer)
display.brewer.all()

ui <- dashboardPage(
	skin="red",

  dashboardHeader(
        #title = "****** TRADE SECRET ******  THE CLIMATE SERVICE FINANCIAL IMPACTS AND CLIMATE SCORE PLATFORM  ****** TRADE SECRET ******", titleWidth=1600,
	title = "The Climate Service",
	dropdownMenu(type = "messages",
                     messageItem(
                       from = "Sarah P. - Operations Manager",
                       message = "Finished loading data you requested.",
                       icon = icon("exclamation"),
                       time = "15 minutes ago"
                     ),
                     messageItem(
                       from = "Robin H. - ESG Manager",
                       message = "Did you include CSR effects?",
                       icon = icon("question"),
                       time = "yesterday"
                     ),
                     messageItem(
                       from = "Craig T. - Marketing",
                       message = "New products brainstorm",
                       icon = icon("calendar"),
                       time = "yesterday"
                     )
        )
  ),

  ## Sidebar content
  dashboardSidebar(
	width=300,
	sidebarMenu(id = "sidebar",
	menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      	menuItem("Settings", tabName = "settings", icon = icon("cog")),
      	menuItem("Corporate Risk Analyzer", tabName = "corporate", icon = icon("building-o")),
      	menuItem("Portfolio Analyzer", tabName = "portfolios", icon = icon("briefcase")),
      	menuItem("Project Analyzer", tabName = "projects", icon = icon("bullseye")),
      	menuItem("Compliance Reporter", tabName = "plans", icon = icon("line-chart"),
          menuSubItem("Compliance - TCFD", tabName = "TCFD"),
          menuSubItem("Climate Action Plan", tabName = "CAP"),
          menuSubItem("Project Plans", tabName = "ProjectPlans")
      ),
      	menuItem("Technical Details", tabName = "overview", icon = icon("podcast"),
      	  menuSubItem("LOCALIZED CLIMATE PROBABILITIES", tabName = "localclimate", icon = icon("cubes")),
      	  menuSubItem("SECTOR IMPACT FUNCTIONS", tabName = "impactfunctions", icon = icon("cloud-download", lib = "glyphicon")),
      	  menuSubItem("PROBABLISTIC IMPACT ESTIMATE", tabName = "impactestimate", icon = icon("cog", lib = "glyphicon")),
      	  menuSubItem("FINANCIAL EFFECTS", tabName = "financialeffects", icon = icon("usd", lib = "glyphicon")),

	# Added in Build7
      	  menuSubItem("INTERNATIONAL PROJECT PLANNING", tabName = "internationalplanning", icon = icon("tree-conifer", lib = "glyphicon")),
      	  menuSubItem("ADAPTATION BENEFIT/COST PLANNING", tabName = "adaptationplanning", icon = icon("tree-deciduous", lib = "glyphicon")),

      	  menuSubItem("OVERALL CLIMATE SCORE", tabName = "climatescore", icon = icon("certificate", lib = "glyphicon"))
	  )
    	)
  ),

  ## Body content
  dashboardBody(
    tabItems(
    
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                  valueBox(437, "TCS Climate Score", icon = icon("thermometer-3"), color = "red")
              ),
              fluidRow(
                  valueBox(14, "Locations", icon = icon("map-marker"), color = "teal"),
                  valueBox(3, "Users", icon = icon("user"), color = "aqua"),
                  valueBox(2, "Plans", icon = icon("line-chart"), color = "blue")
              )
      ),

      # Second tab content
      tabItem(tabName = "settings",
              fluidRow(
                tabBox(
                  tabPanel(title = "Basics",
                    textInput("companyName","Company Name",value="Micron"),
                    selectInput("cbGroupBizType","Industry Sector",
                      c("Beverage","Agriculture","Packaged Foods/Meats","Paper & forest","Manufacturing","Metals & mining", "Chemicals", "Real Estate Management/Development","Transportation","Oil & gas","Electric Utilities"),
                      selected = c("Manufacturing")
                    ),
                    selectInput("cbClimateScenario","Climate Scenario",
                                c("Most Likely","Best Case - RCP 2.6","Business As Usual - RCP 8.5","Paris Accord"),
                                selected = c("Most Likely")
                    )
                  ),
                  tabPanel(title = "Locations",
                    textInput("location1","Corporate Headquarters",value = "8000 S Federal Way, Boise, ID 83716"),
                    htmlOutput("frame"),
                    hr(),
                    actionButton("addLocation","Add a location")
                  ),
                  tabPanel(title = "Users",
                    valueBox(1, "Julia Grant", icon = icon("user"), color = "teal"),
                    valueBox(1, "Joe Robinson", icon = icon("user"), color = "teal"),
                    valueBox(1, "Norm Armour", icon = icon("user"), color = "teal"),
                    actionButton("addUser","Add a user")
                  )
                )#tabBox
              )#fluidRow
            ),

      # Third tab content
      tabItem(tabName = "corporate",
              fluidRow(
                tabBox(width = "500",
                  tabPanel(title="Screening level",
                    selectInput("cbLocation","Location",
                               c("Boise","Singapore","Malaysia","Scotland"),
                               selected = c("Boise")
                    ),
                    sliderInput("siTimeframe", "Timeframe", 1, 30, 5, step=1, animate=TRUE),
                    infoBox(title="EPS Value at Risk",value="0.187%",color="aqua",icon = icon("percent")),
                    infoBox(title="Revenue at Risk",value="$2,618,782",color="aqua",icon = icon("usd")),
                    infoBox(title="Expenses at Risk",value="$34,729,133",color="aqua",icon = icon("usd")),
                    infoBox(title="Assets at Risk",value="$215,362,765",color="aqua",icon = icon("usd")),
                    infoBox(title="Liabilities at Risk",value="$1,824,773",color="aqua",icon = icon("usd"))
                  )
                )#tabBox
              )#fluidRow
      ),#tabItem corporate

      # Fourth tab content
      tabItem(tabName = "portfolios",
              fluidRow(
                tabBox(title = "Investment Portfolio Analyzer",
                tabPanel(
                  title = "Set up",
                  actionButton("addStock","Add a Listed Equity")
                ),
                tabPanel(title="Analyze",
                    "Listing of equities with relative risk",br(),"Ability to drill down")
              )
              )
      ),

      # Fifth tab content
      tabItem(tabName = "projects",
              fluidRow(
                box(
                  title = "Projects","This should have tabs for the investment lifecycle that Chiara shared."
                )
              )
      ),

      # Sixth tab content
      tabItem(tabName = "plans",
              fluidRow(
                box(
                  title = "Plans"
                )
              )
      ),
       #Plans tabSubItems
      tabItem("TCFD",
              fluidRow(
                tabBox(title = "TCFD Reporting",
                       tabPanel(title = "Governance",
                                textAreaInput("TCFD-Gov-a","Board Oversight", width = 500, value="Describe the board's oversight of climate-related risks and opportunities."),
                                textAreaInput("TCFD-Gov-b","Management's Role", width = 500, value="Describe management's role in assessing and managing climate-related risks and opportunities.")
                       ),
                       tabPanel(title = "Strategy",
                                textAreaInput("TCFD-Strat-a","Climate Risks and Opportunities", width = 500, value = "Describe the climate-related risks and opportunities the organization has identified over the short, medium, and long term."),
                                checkboxGroupInput("chkbxRisks","Risks & Opportunities", width = 500, c("Policy & Legal Risk","Technology Risk","Market Risk","Reputation Risk","Acute Physical Risk","Chronic Physical Risk","Resource Efficiency","Energy Source","Products/Services","Markets","Resilience"),selected = c("Energy Source","Acute Physical Risk","Chronic Physical Risk")),
                                textAreaInput("TCFD-Strat-b","Impact of Risks", width = 500, value = "Describe the impact of climate-related risks and opportunities on the organization's businesses, strategy, and financial planning.")
                       ),
                       tabPanel(title = "Risk Management",
                                textAreaInput("TCFD-Gov-a","Processes for Identifying Risks", width = 500, value="Describe the processes for identifying & assessing climate-related risks and opportunities."),
                                textAreaInput("TCFD-Gov-b","Processes for Managing", width = 500, value="Describe the processes managing climate-related risks."),
                                textAreaInput("TCFD-Gov-a","Process Integration", width = 500, value="Describe how processes for identifying, assessing, and managing climate-related risks are intgrated into the organization's overall management.")
                       ),
                       tabPanel(title = "Metrics and Targets"),
                       tabPanel(title="Financial Impacts",
                                dataTableOutput("corpFinImpacts")
                       )#tabPanel
              ))
      ),
      tabItem("CAP",
              box(
                title = "Climate Action Plan"
              )
              ),
      tabItem("ProjectPlans",
              box(
                title = "Project Plans"
              )
              ),

      tabItem(tabName = "overview", 
        h1("PROTOTYPE PRODUCTS:", col="red"),
        h2("1.  Project Analyzer"),
        h2("2.  Portfolio Analyzer"),
        h2("3.  Corporate Risk Monitoring"),
        h2("4.  TCFD Reporting")
      ),

      tabItem(tabName = "localclimate",
        h2("Localized probability distributions from historical and projected daily data"),
        fluidRow(
	# More information on cascading style sheets at: http://shiny.rstudio.com/articles/css.html.
	#includeCSS("./www/darkly.css"),
          box(plotOutput("climplot1", height = 200)),
          box(plotOutput("climplot2", height = 200)),
          box(plotOutput("climplot3", height = 200)),
          box(plotOutput("climplot4", height = 200)),
          box(title="Ensemble Distributions Evolve Through Time", background = "red", solidHeader = TRUE, plotOutput("climplot5", height = 300)),
          box(
            title = "Climate Data Controls",
            sliderInput("bins", "Number of bins:", 10, 100, 50, step=10, animate=TRUE)
          )
        )
      ),

      tabItem(tabName = "impactfunctions",
        h2("Sector-specific impact functions translate changes into impacts on infrastructure, workforce, revenue..."),
       
	fluidRow(
          box(title="Impact Functions - Combined", background = "red", solidHeader = TRUE, plotOutput("impactplot3", height = 200)),
          box(
            title = "Relative Weight of Impact Functions",
            sliderInput("impactfunctionweight", "Sigmoid weight (quadratic weight is the remainder):", 0, 1, 0.5, step=0.1, animate=TRUE)
          )
	),
        
	fluidRow(
          box(title="Impact Functions - Sigmoidal", background = "blue", solidHeader = TRUE, plotOutput("impactplot1", height = 200)),
          box(title="Impact Functions - Quadratic", background = "blue", solidHeader = TRUE, plotOutput("impactplot2", height = 200)),
          box(
            title = "Impact Function Controls - Sigmoidal",
            sliderInput("sigmoidlimit", "Sigmoid limit:", -100, 100, -75, step=10, animate=TRUE),
            sliderInput("sigmoidsteepness", "Sigmoid steepness:", 0.1, 10, 2, step=0.5, animate=TRUE),
            sliderInput("sigmoidmidpoint", "Sigmoid midpoint:", 270, 320, 295, step=1, animate=TRUE)
          ),
          box(
            title = "Impact Function Controls - Quadratic",
            sliderInput("quadraticlimit", "Quadratic limit:", 10, 100, 50, step=10, animate=TRUE),
            sliderInput("quadraticshape", "Quadratic shape:", -20, 20, -4, step=2, animate=TRUE),
            sliderInput("quadraticmidpoint", "Quadratic midpoint:", 270, 320, 295, step=1, animate=TRUE)
          )
        ),

	fluidRow(
	  h2("=== IMPACT FUNCTION LIBRARY ==="),
	  column(3,
          #box(title="CROP YIELD - TEMPERATURE", background = "yellow", solidHeader = TRUE, imageOutput("impactplot4", height = 300)),
          #box(title="CROP YIELD - PRECIPITATION", background = "yellow", solidHeader = TRUE, imageOutput("impactplot5", height = 300)),
          #box(title="CROP YIELD - MULTIPLE VARIABLES", background = "yellow", solidHeader = TRUE, imageOutput("impactplot6", height = 300)),
          #box(title="VARIOUS SECTORS", background = "yellow", solidHeader = TRUE, imageOutput("impactplot7", height = 300)),
          #box(title="VARIOUS SECTORS", background = "yellow", solidHeader = TRUE, imageOutput("impactplot8", height = 300))
	  h4("Power Consump., GDP, Income, Productivity, etc."),
          imageOutput("impactplot8", height = 300)
	  ),

	  column(3, offset=1,
	  h4("Mortality, Damage, etc."),
          imageOutput("impactplot7", height = 300)
	  )
	),

	fluidRow(
	  column(3, 
	  h4("Crops - Temperature"),
          imageOutput("impactplot4", height = 300)
	  ),

	  column(3, offset=1,
	  h4("Crops - Precipitation"),
          imageOutput("impactplot5", height = 300)
	  ),

	  column(3, offset=1,
	  h4("Crops - Derived Variables"),
          imageOutput("impactplot6", height = 300)
	  )
	),
        
	fluidRow(
	  column(3, 
	  h4("Power Generation - Air/Water Temperature"),
          imageOutput("impactplot9", height = 300)
	  ),

	  column(3, offset=1,
	  h4("Water Requirements for Power - Water Temperature"),
          imageOutput("impactplot10", height = 300)
	  )

	)
        
      ),

      tabItem(tabName = "impactestimate",
        h2("Combine climate probabilities and impact functions to estimate probability-weighted net impact"),
        fluidRow(
          box(title="Impact Function (controlled from impact-function tab)", background = "blue", solidHeader = TRUE, plotOutput("impactestimateplot3", height = 300)),
          box(title="Probabilistic Impact Estimate", background = "red", solidHeader = TRUE, plotOutput("impactestimateplot2", height = 300))
#          box(
#            title = "Impact Estimate Controls",
#            sliderInput("sigmoidlimit2", "Sigmoid limit:", -100, 100, -75, step=10, animate=TRUE),
#            sliderInput("sigmoidsteepness2", "Sigmoid steepness:", 0.1, 10, 2, step=0.5, animate=TRUE),
#            sliderInput("sigmoidmidpoint2", "Sigmoid midpoint:", 270, 320, 295, step=1, animate=TRUE)
#          )
	),
        fluidRow(
          box(title="Probability of Exceeding Thresholds (based on localized climate projections", background = "yellow", solidHeader = TRUE, plotOutput("impactestimateplot1", height = 300)),
          box(
            title = "Threshold Controls",
            sliderInput("threshold", "Threshold:", 270, 320, 295)
          )
        )
      ),

      tabItem(tabName = "financialeffects",
        h2("Calculate effects of net impact on beta and NPV"),
        fluidRow(
          box(title="Beta Multiplier By Period: 1/(1-D), where D is probabilistic impact", background = "blue", solidHeader = TRUE, plotOutput("financialplot1", height = 300)),
          box(title="NPV Impact on Project", background = "green", solidHeader = TRUE, plotOutput("financialplot2", height = 300)),
          # Modified in Build7 
          #box(
          #  title = "Multiplier Controls (sigmoid impact function)",
          #  sliderInput("sigmoidlimit3", "Sigmoid limit:", -100, 100, -75, step=10, animate=TRUE),
          #  sliderInput("sigmoidsteepness3", "Sigmoid steepness:", 0.1, 10, 2, step=0.5, animate=TRUE),
          #  sliderInput("sigmoidmidpoint3", "Sigmoid midpoint:", 270, 320, 295, step=1, animate=TRUE)
          #),
          box(
            title = "Capital Cost and Discount Rate",
	    width=4,
            sliderInput("capitalcost", "Total Capital Investment:", 0, 50, 20, step=5, animate=TRUE),
            sliderInput("discount", "Discount Rate:", 0.01, 0.05, 0.03, step=0.005)
          ),
          box(
            title = "Cash Flows for Periods 1-4",
	    width=4,
	     #numericInput("cashflow", "Cash Flow:", 10, min = 1, max = 100),
            sliderInput("cashflow1", "Period 1 Cash Flow:", -10, 100, 1),
            sliderInput("cashflow2", "Period 2 Cash Flow:", -10, 100, 5),
            sliderInput("cashflow3", "Period 3 Cash Flow:", -10, 100, 10),
            sliderInput("cashflow4", "Period 4 Cash Flow:", -10, 100, 20)
          ),
          box(
            title = "Cash Flows for Periods 5-7",
	    width=4,
            sliderInput("cashflow5", "Period 5 Cash Flow:", -10, 100, 30),
            sliderInput("cashflow6", "Period 6 Cash Flow:", -10, 100, 30),
            sliderInput("cashflow7", "Period 7 Cash Flow:", -10, 100, 30)
          )
        )
      ),

# Added in Build7
      tabItem(tabName = "internationalplanning",
        h2("Climate risk assessment and adaptation planning throughout the transaction cycle"),
        fluidRow(
          box(
            title = "Economic Sector",
  	    selectInput("econsector", "Choose economic sector:", list("Power","Agriculture","Water") ),
	    width=3,
    	    textOutput("econsector")
          ),
          box(
            title = "ESG Assessment",
  	    selectInput("esgassessment", "Choose ESG assessment:", list("Low","Moderate","High") ),
	    width=3,
    	    textOutput("esgassessment")
          )
        )
      ),

# Added in Build7
      tabItem(tabName = "adaptationplanning",
        h2("Calculate adaptation benefit/cost ratio"),
        fluidRow(
	  box(title="Impact Function (controlled from Sector Impact tab)", background = "blue", solidHeader = TRUE, plotOutput("adaptationplot1", height = 300)),
	  box(title="Asset Value Through Time", background = "red", solidHeader = TRUE, plotOutput("adaptationplot2", height = 300))
        ),
        fluidRow(
          box(
            title = "Asset Values ($M)",
            sliderInput("assetvalue1", "Value of Asset 1:", 0, 100, 1, animate=TRUE),
            sliderInput("assetvalue2", "Value of Asset 2:", 0, 100, 5, animate=TRUE),
            sliderInput("assetvalue3", "Value of Asset 3:", 0, 100, 10, animate=TRUE),
	    width=3
          ),
          box(
            title = "Asset Sensitivities to Impact Function (no adaptation)",
            sliderInput("assetsensitivity1", "Impact Sensitivity of Asset 1:", 0, 5, 1, step=0.25, animate=TRUE),
            sliderInput("assetsensitivity2", "Impact Sensitivity of Asset 2:", 0, 5, 1, step=0.25, animate=TRUE),
            sliderInput("assetsensitivity3", "Impact Sensitivity of Asset 3:", 0, 5, 1, step=0.25, animate=TRUE),
	    width=3
          ),
          box(
            title = "Adaptation Plan 1",
            sliderInput("assetsensitivity1_plan1", "Plan 1 Impact Sensitivity of Asset 1:", 0, 5, 0.5, step=0.25, animate=TRUE),
            sliderInput("assetsensitivity2_plan1", "Plan 1 Impact Sensitivity of Asset 2:", 0, 5, 0.5, step=0.25, animate=TRUE),
            sliderInput("assetsensitivity3_plan1", "Plan 1 Impact Sensitivity of Asset 3:", 0, 5, 0.5, step=0.25, animate=TRUE),
    		textOutput("adaptationbenefit1"),
            sliderInput("cost_plan1", "Plan 1 Cost:", 0, 20, 5, step=1, animate=TRUE),
	    width=3
          ),
          box(
            title = "Adaptation plan 2",
            sliderInput("assetsensitivity1_plan2", "Plan 2 Impact Sensitivity of Asset 1:", 0, 5, 0.25, step=0.25, animate=TRUE),
            sliderInput("assetsensitivity2_plan2", "Plan 2 Impact Sensitivity of Asset 2:", 0, 5, 0.25, step=0.25, animate=TRUE),
            sliderInput("assetsensitivity3_plan2", "Plan 2 Impact Sensitivity of Asset 3:", 0, 5, 0.25, step=0.25, animate=TRUE),
    		textOutput("adaptationbenefit2"),
            sliderInput("cost_plan2", "Plan 2 Cost:", 0, 20, 5, step=1, animate=TRUE),
	    width=3
          )
        )
      ),

      tabItem(tabName = "climatescore",
        h2("Derive climate score from impacts, financial effects, and adaptation actions"),
        fluidRow(
          box(title="Impact Function (controlled from impact-function tab)", background = "blue", solidHeader = TRUE, plotOutput("scoreplot2", height = 300)),
          box(title="Climate Score By Period", background = "green", solidHeader = TRUE, plotOutput("scoreplot1", height = 300)),

      # Modified in Build7
          #box(
          #  title = "Impact Function Controls (sigmoid impact function)",
          #  sliderInput("sigmoidlimit4", "Sigmoid limit:", -100, 100, -75, step=10, animate=TRUE),
          #  sliderInput("sigmoidsteepness4", "Sigmoid steepness:", 0.1, 10, 2, step=0.5, animate=TRUE),
          #  sliderInput("sigmoidmidpoint4", "Sigmoid midpoint:", 270, 320, 295, step=1, animate=TRUE)
          #),

          box(
            title = "Adaptation Credits",
  	    selectInput("adaptationplan", "Choose an adaptation plan:", list("None","Minimal","Moderate","Maximal") ),
    		textOutput("adaptationplan")
          )
        )
      )

    )
  )
)

server <- function(input, output, sessionn) {

# James -----------------------------------------------------------
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
    my_test <- tags$iframe(src="map.html", height=300, width=300)
    print(my_test)
    my_test
  })

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
# James -----------------------------------------------------------


# Terry -----------------------------------------------------------
  # Get functions.
  source("./functions/function_fit_distributions.r")

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
    x <- seq(270,320,0.01)
    sigmoid = function(x,L,a,x0) {L / (1 + exp(-a*(x-x0)) ) }
	# L is upper/lower limit; a is steepness; x0 is x value at sigmoid midpoint
    plot(x,sigmoid(x,input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint), type="l", lwd=3, lty=1, col="red", xlim=c(270,320), ylim=c(-100,100), xlab="Daily Maximum Surface Temperature (degK)", ylab="Relative Impact")
  })

  output$impactplot2 <- renderPlot({
    x <- seq(270,320,0.01)
    quadratic = function(x,L,a,x0) {a*(x-x0)^2 + L }
	# L is value at vertex; a +/- gives parabola opening up/down; x0 is x value at vertex
    plot(x,quadratic(x,input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint), type="l", lwd=3, lty=1, col="red", xlim=c(270,320), ylim=c(-100,100), xlab="Daily Maximum Surface Temperature (degK)", ylab="Relative Impact")
  })

  output$impactplot3 <- renderPlot({
    x <- seq(270,320,0.01)
    sigmoid = function(x,L,a,x0) {L / (1 + exp(-a*(x-x0)) ) }
    quadratic = function(x,L,a,x0) {a*(x-x0)^2 + L }
    wt1 = input$impactfunctionweight
    wt2 = 1 - wt1
    plot(x,wt1*sigmoid(x,input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(x,input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint), type="l", lwd=3, lty=1, col="red", xlim=c(270,320), ylim=c(-100,100), xlab="Daily Maximum Surface Temperature (degK)", ylab="Relative Impact")
  })

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
    list(src = "./images/carleton_hsiang_climate_dose_response_2016_fig3a.png",width=450,height=300,alt = paste("carleton_hsiang_climate_dose_response_2016_fig3a_multiplesectors"))
  }, deleteFile = FALSE)

  output$impactplot8 <- renderImage({
    list(src = "./images/carleton_hsiang_climate_dose_response_2016_fig3b.png",width=450,height=300,alt = paste("carleton_hsiang_climate_dose_response_2016_fig3b_multiplesectors"))
  }, deleteFile = FALSE)

  output$impactplot9 <- renderImage({
    list(src = "./images/thompson_cooling_water_Teffects_v1b_power_airtemp.png",width=450,height=300,alt = paste("thompson_power_generation_air_and_water_temperature"))
  }, deleteFile = FALSE)

  output$impactplot10 <- renderImage({
    list(src = "./images/thompson_cooling_water_Teffects_v1b_water.png",width=450,height=300,alt = paste("thompson_water_needed_water_temperature"))
  }, deleteFile = FALSE)

  # Probability of Exceeding Thresholds
  # Modified in Build7 - Just note that the shapes and scales values are for a pre-determined Weibull distribution for South Sudan.
  output$impactestimateplot1 <- renderPlot({
    #shapes <- c(81.8730, 93.0240, 88.9460, 84.7620, 95.8550, 90.0690, 86.1060, 90.3700, 91.5810)
    #scales <- c(292.0320, 293.0880, 293.0820, 293.3870, 293.7670, 293.9150, 294.5310, 295.7390, 295.7960)
    # x-axis tickmarks are not labelled properly if the first period below is longer than 7 characters.
    periods <- c("1976-05", "2016-25", "2026-35", "2036-45", "2046-55", "2056-65", "2066-75", "2076-85", "2086-95")
    shapes <- c(81,82,83,84,85,86,87,88,89)
    scales <- c(292,293,294,295,296,297,298,299,300)
    thresholds <- c(285,290,295,300,305,310)
    colors <- brewer.pal(length(thresholds), "Spectral")
    ltypes <- c(1:length(thresholds))
    labels <- c("285K","290K","295K","300K","305K","310K")
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
    periods <- c("1976-05", "2016-25", "2026-35", "2036-45", "2046-55", "2056-65", "2066-75", "2076-85", "2086-95")
    shapes <- c(81,82,83,84,85,86,87,88,89)
    scales <- c(292,293,294,295,296,297,298,299,300)
    thresholds <- c(285,290,295,300,305,310)
    colors <- brewer.pal(length(thresholds), "Spectral")
    ltypes <- c(1:length(thresholds))
    labels <- c("285K","290K","295K","300K","305K","310K")
    x <- seq(270,320,0.1)
    # pweibull is the CDF for dweibull.
    # Rows in the impacts and damage matrices correspond to the climate-variable values, while columns correspond to the periods covered.  
    # thresholds has length corresponding to the variable values, while shapes and scales have lengths corresponding to periods.
    impacts = matrix(0,length(thresholds),length(shapes))
    # Impacts are the product of the probability that the climate variable will lie in a certain interval and the damage corresponding to the midpoint of the climate variable in that interval.
# XXX make thresholds 1-degree wide and incorporate real damage function
    damage <- 1
    damage = matrix(1,length(thresholds),length(shapes))
    sigmoid = function(x,L,a,x0) {L / (1 + exp(-a*(x-x0)) ) }
	# L is upper/lower limit; a is steepness; x0 is x value at sigmoid midpoint
    # Note that the sigmoid inputs used below are those labelled with "2" and found on the panel for this plot.
    # Remove the "2" if the plot on this panel should be controlled from the impact-function panel, and remove the controls referring to inputs labelled "2".
  # Modified in Build7
        quadratic = function(x,L,a,x0) {a*(x-x0)^2 + L }
      # wt1 is sigmoid weight; wt2 is quadratic weight. 
        wt1 = input$impactfunctionweight
        wt2 = 1 - wt1
    for(j in 1:length(thresholds)-1) { for(i in 1:length(shapes)) {
#	damage[j,i] <- 0.5*(sigmoid(thresholds[j+1],input$sigmoidlimit2,input$sigmoidsteepness2,input$sigmoidmidpoint2) + sigmoid(thresholds[j],input$sigmoidlimit2,input$sigmoidsteepness2,input$sigmoidmidpoint2) )
        damagej1 = wt1*sigmoid(thresholds[j],input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(thresholds[j],input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint)
        damagej2 = wt1*sigmoid(thresholds[j+1],input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(thresholds[j+1],input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint)
	damage[j,i] <- 0.5*(damagej2 + damagej1)
       } }
    for(j in 1:length(thresholds)-1) { for(i in 1:length(shapes)) {
	impacts[j,i] <- damage[j,i]*( pweibull(thresholds[j+1],shapes[i],scales[i]) - pweibull(thresholds[j],shapes[i],scales[i])  ) } }
    impactbyperiod <- c(0,0,0,0,0,0,0,0,0)
    for(i in 1:length(impactbyperiod)) {impactbyperiod[i]=sum(impacts[,i])  }
    impactbyperiod_relative2baseperiod = impactbyperiod - impactbyperiod[1]
    plot(impactbyperiod_relative2baseperiod, type="l", lwd=3, lty=1, col=colors[1], xlab="Periods", ylab="Probabilistic Impact", xaxt="n")
	axis(1, at=c(1:length(periods)), labels=periods)
  })

  # Impact Function for impact estimate (controlled from impact-function tab)
  # Modified in Build7 
  output$impactestimateplot3 <- renderPlot({
    x <- seq(270,320,0.01)
    sigmoid = function(x,L,a,x0) {L / (1 + exp(-a*(x-x0)) ) }
    #plot(x,sigmoid(x,input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint), type="l", lwd=3, lty=1, col="red", xlim=c(270,320), ylim=c(-100,100), xlab="Daily Maximum Surface Temperature (degK)", ylab="Relative Impact")
    quadratic = function(x,L,a,x0) {a*(x-x0)^2 + L }
    wt1 = input$impactfunctionweight
    wt2 = 1 - wt1
    plot(x,wt1*sigmoid(x,input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(x,input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint), type="l", lwd=3, lty=1, col="red", xlim=c(270,320), ylim=c(-100,100), xlab="Daily Maximum Surface Temperature (degK)", ylab="Relative Impact")
  })

# Modified in Build7
  output$financialplot1 <- renderPlot({
    periods <- c("1976-05", "2016-25", "2026-35", "2036-45", "2046-55", "2056-65", "2066-75", "2076-85", "2086-95")
    shapes <- c(81,82,83,84,85,86,87,88,89)
    scales <- c(292,293,294,295,296,297,298,299,300)
    thresholds <- c(285,290,295,300,305,310)
    colors <- brewer.pal(length(thresholds), "Spectral")
    ltypes <- c(1:length(thresholds))
    labels <- c("285K","290K","295K","300K","305K","310K")
    x <- seq(275,315,0.1)
    # pweibull is the CDF for dweibull.
    # Rows in the impacts and damage matrices correspond to the climate-variable values, while columns correspond to the periods covered.  
    # thresholds has length corresponding to the variable values, while shapes and scales have lengths corresponding to periods.
    impacts = matrix(0,length(thresholds),length(shapes))
    # Impacts are the product of the probability that the climate variable will lie in a certain interval and the damage corresponding to the midpoint of the climate variable in that interval.
# XXX make thresholds 1-degree wide and incorporate real damage function
    damage = matrix(1,length(thresholds),length(shapes))
    sigmoid = function(x,L,a,x0) {L / (1 + exp(-a*(x-x0)) ) }
	# L is upper/lower limit; a is steepness; x0 is x value at sigmoid midpoint
    quadratic = function(x,L,a,x0) {a*(x-x0)^2 + L }
      # wt1 is sigmoid weight; wt2 is quadratic weight. 
        wt1 = input$impactfunctionweight
        wt2 = 1 - wt1
    # Remove the "3" if the plot on this panel should be controlled from the impact-function panel, and remove the controls referring to inputs labelled "3".
    # Modified in Build7
    for(j in 1:length(thresholds)-1) { for(i in 1:length(shapes)) {
	#damage[j,i] <- 0.5*(sigmoid(thresholds[j+1],input$sigmoidlimit3,input$sigmoidsteepness3,input$sigmoidmidpoint3) + sigmoid(thresholds[j],input$sigmoidlimit3,input$sigmoidsteepness3,input$sigmoidmidpoint3) )
        damagej1 = wt1*sigmoid(thresholds[j],input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(thresholds[j],input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint)
        damagej2 = wt1*sigmoid(thresholds[j+1],input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(thresholds[j+1],input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint)
        damage[j,i] <- 0.5*(damagej2 + damagej1)
       } }
    for(j in 1:length(thresholds)-1) { for(i in 1:length(shapes)) {
	impacts[j,i] <- damage[j,i]*( pweibull(thresholds[j+1],shapes[i],scales[i]) - pweibull(thresholds[j],shapes[i],scales[i])  ) } }
    impactbyperiod <- c(0,0,0,0,0,0,0,0,0)
    for(i in 1:length(impactbyperiod)) {impactbyperiod[i]=sum(impacts[,i])  }
    impactbyperiod_relative2baseperiod = impactbyperiod - impactbyperiod[1]
    betamultiplier = impactbyperiod
    for(i in 1:length(betamultiplier)) {betamultiplier[i]= 1 / (1 + impactbyperiod_relative2baseperiod[i]/100 )  }
    plot(betamultiplier, type="l", lwd=3, lty=1, col=colors[1], xlab="Periods", ylab="Beta Multiplier", xaxt="n")
	axis(1, at=c(1:length(periods)), labels=periods)
  })

  output$financialplot2 <- renderPlot({
    periods <- c("1976-05", "2016-25", "2026-35", "2036-45", "2046-55", "2056-65", "2066-75", "2076-85", "2086-95")
    shapes <- c(81,82,83,84,85,86,87,88,89)
    scales <- c(292,293,294,295,296,297,298,299,300)
    thresholds <- c(285,290,295,300,305,310)
    colors <- brewer.pal(length(thresholds), "Spectral")
    ltypes <- c(1:length(thresholds))
    labels <- c("285K","290K","295K","300K","305K","310K")
    x <- seq(275,315,0.1)
    # pweibull is the CDF for dweibull.
    # Rows in the impacts and damage matrices correspond to the climate-variable values, while columns correspond to the periods covered.  
    # thresholds has length corresponding to the variable values, while shapes and scales have lengths corresponding to periods.
    impacts = matrix(0,length(thresholds),length(shapes))
    # Impacts are the product of the probability that the climate variable will lie in a certain interval and the damage corresponding to the midpoint of the climate variable in that interval.
# XXX make thresholds 1-degree wide and incorporate real damage function
    damage <- 1
    damage = matrix(1,length(thresholds),length(shapes))
    sigmoid = function(x,L,a,x0) {L / (1 + exp(-a*(x-x0)) ) }
	# L is upper/lower limit; a is steepness; x0 is x value at sigmoid midpoint
    # Note that the sigmoid inputs used below are those labelled with "2" and found on the panel for this plot.
    # Remove the "3" if the plot on this panel should be controlled from the impact-function panel, and remove the controls referring to inputs labelled "3".
    # Modified in Build7
    for(j in 1:length(thresholds)-1) { for(i in 1:length(shapes)) {
	#damage[j,i] <- 0.5*(sigmoid(thresholds[j+1],input$sigmoidlimit3,input$sigmoidsteepness3,input$sigmoidmidpoint3) + sigmoid(thresholds[j],input$sigmoidlimit3,input$sigmoidsteepness3,input$sigmoidmidpoint3) )
	damage[j,i] <- 0.5*(sigmoid(thresholds[j+1],input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + sigmoid(thresholds[j],input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) )
       } }
    for(j in 1:length(thresholds)-1) { for(i in 1:length(shapes)) {
	impacts[j,i] <- damage[j,i]*( pweibull(thresholds[j+1],shapes[i],scales[i]) - pweibull(thresholds[j],shapes[i],scales[i])  ) } }
    impactbyperiod <- c(0,0,0,0,0,0,0,0,0)
    for(i in 1:length(impactbyperiod)) {impactbyperiod[i]=sum(impacts[,i])  }
    impactbyperiod_relative2baseperiod = impactbyperiod - impactbyperiod[1]
    betamultiplier = impactbyperiod
    for(i in 1:length(betamultiplier)) {betamultiplier[i]= 1 / (1 + impactbyperiod_relative2baseperiod[i]/100 )  }
    # NPV calculation
    # Assume project starts in period 3 and that each period is a decade.
    initializer <- c(0,0,0,0,0,0,0)
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
    barplot(c(npv,npv_modified), col=c("green","red"), names.arg=c("Without Climate Impacts", "With Climate Impacts"),xlab="Unmodified and Modified NPV", ylab="NPV", ylim=c(-20,50) )
  })

  # Modified in Build7
  output$scoreplot1 <- renderPlot({
    periods <- c("1976-05", "2016-25", "2026-35", "2036-45", "2046-55", "2056-65", "2066-75", "2076-85", "2086-95")
    shapes <- c(81,82,83,84,85,86,87,88,89)
    scales <- c(292,293,294,295,296,297,298,299,300)
    thresholds <- c(285,290,295,300,305,310)
    colors <- brewer.pal(length(thresholds), "Spectral")
    ltypes <- c(1:length(thresholds))
    labels <- c("Without Adaptation","With Adaptation")
    x <- seq(275,315,0.1)
    # Rows in the impacts and damage matrices correspond to the climate-variable values, while columns correspond to the periods covered.  
    # thresholds has length corresponding to the variable values, while shapes and scales have lengths corresponding to periods.
    impacts = matrix(0,length(thresholds),length(shapes))
    # Impacts are the product of the probability that the climate variable will lie in a certain interval and the damage corresponding to the midpoint of the climate variable in that interval.
# XXX make thresholds 1-degree wide and incorporate real damage function
    damage = matrix(1,length(thresholds),length(shapes))
    sigmoid = function(x,L,a,x0) {L / (1 + exp(-a*(x-x0)) ) }
    quadratic = function(x,L,a,x0) {a*(x-x0)^2 + L }
    wt1 = input$impactfunctionweight
    wt2 = 1 - wt1
    #plot(x,wt1*sigmoid(x,input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(x,input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint), type="l", lwd=3, lty=1, col="red", xlim=c(270,320), ylim=c(-100,100), xlab="Daily Maximum Surface Temperature (degK)", ylab="Relative Impact")

    for(j in 1:length(thresholds)-1) { for(i in 1:length(shapes)) {
    # Modified in Build7 
#	damage[j,i] <- 0.5*(sigmoid(thresholds[j+1],input$sigmoidlimit4,input$sigmoidsteepness4,input$sigmoidmidpoint4) + sigmoid(thresholds[j],input$sigmoidlimit4,input$sigmoidsteepness4,input$sigmoidmidpoint4) )
        damagej1 = wt1*sigmoid(thresholds[j],input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(thresholds[j],input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint)
        damagej2 = wt1*sigmoid(thresholds[j+1],input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(thresholds[j+1],input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint)
        damage[j,i] <- 0.5*(damagej2 + damagej1)
       } }

    for(j in 1:length(thresholds)-1) { for(i in 1:length(shapes)) {
	impacts[j,i] <- damage[j,i]*( pweibull(thresholds[j+1],shapes[i],scales[i]) - pweibull(thresholds[j],shapes[i],scales[i])  ) } }
    impactbyperiod <- c(0,0,0,0,0,0,0,0,0)
    for(i in 1:length(impactbyperiod)) {impactbyperiod[i]=sum(impacts[,i])  }
    impactbyperiod_relative2baseperiod = impactbyperiod - impactbyperiod[1]
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

  # Modified in Build7
  # Impact function from impact-function tab.
  output$scoreplot2 <- renderPlot({
    x <- seq(270,320,0.01)
    sigmoid = function(x,L,a,x0) {L / (1 + exp(-a*(x-x0)) ) }
    quadratic = function(x,L,a,x0) {a*(x-x0)^2 + L }
    wt1 = input$impactfunctionweight
    wt2 = 1 - wt1
    # plot(x,sigmoid(x,input$sigmoidlimit4,input$sigmoidsteepness4,input$sigmoidmidpoint4), type="l", lwd=3, lty=1, col="red", xlim=c(270,320), ylim=c(-100,100), xlab="Daily Maximum Surface Temperature (degK)", ylab="Relative Impact")
    plot(x,wt1*sigmoid(x,input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(x,input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint), type="l", lwd=3, lty=1, col="red", xlim=c(270,320), ylim=c(-100,100), xlab="Daily Maximum Surface Temperature (degK)", ylab="Relative Impact")
  })

  # Added in Build7
  # Impact function from impact-function tab.
  output$adaptationplot1 <- renderPlot({
    x <- seq(270,320,0.01)
    sigmoid = function(x,L,a,x0) {L / (1 + exp(-a*(x-x0)) ) }
    quadratic = function(x,L,a,x0) {a*(x-x0)^2 + L }
    wt1 = input$impactfunctionweight
    wt2 = 1 - wt1
    plot(x,wt1*sigmoid(x,input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(x,input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint), type="l", lwd=3, lty=1, col="red", xlim=c(270,320), ylim=c(-100,100), xlab="Daily Maximum Surface Temperature (degK)", ylab="Relative Impact")
  })

  # Added in Build7
  # Asset-value damage
  output$adaptationplot2 <- renderPlot({
    periods <- c("1976-05", "2016-25", "2026-35", "2036-45", "2046-55", "2056-65", "2066-75", "2076-85", "2086-95")
    shapes <- c(81,82,83,84,85,86,87,88,89)
    scales <- c(292,293,294,295,296,297,298,299,300)
    thresholds <- c(285,290,295,300,305,310)
    colors <- brewer.pal(length(thresholds), "Spectral")
    ltypes <- c(1:length(thresholds))
    labels <- c("285K","290K","295K","300K","305K","310K")
    x <- seq(270,320,0.1)

    # Rows in the impacts and damage matrices correspond to the climate-variable values, while columns correspond to the periods covered.  The third dimension is the adaptation plan.
    # The code below does not yet use an array or tensor to store damages and impacts by adaptation plan.
    # thresholds has length corresponding to the variable values, while shapes and scales have lengths corresponding to periods.
    nadaptplans = 2
    # Impacts are the product of the probability that the climate variable will lie in a certain interval and the damage corresponding to the midpoint of the climate variable in that interval.
    impacts = matrix(0,length(thresholds),length(shapes))
    impacts_plan1 = impacts
    impacts_plan2 = impacts
    damage = matrix(1,length(thresholds),length(shapes))
    damage_plan1 = damage
    damage_plan2 = damage

    sigmoid = function(x,L,a,x0) {L / (1 + exp(-a*(x-x0)) ) }
    quadratic = function(x,L,a,x0) {a*(x-x0)^2 + L }
      # wt1 is sigmoid weight; wt2 is quadratic weight. 
        wt1 = input$impactfunctionweight
        wt2 = 1 - wt1

  # Asset risk below is sum of values by asset, weighted by sensitivity to given impact function.  Sensitivity varies with adaptation plan.
  #   Impact functions give percent impact, so we allow different assets to have different sensitivities to a given impact function.
  #   This is a simplified way to get different impacts by scaling by asset type; ideally, they would have different impact functions. 
  #   This is also a simplified (linear) way to connect asset value with loss of capacity or condition in the impact function.
  #    assetrisk = (input$assetvalue1*input$assetsensitivity1) + (input$assetvalue2*input$assetsensitivity2) + (input$assetvalue3*input$assetsensitivity3)
    assetrisk1 = (input$assetvalue1*input$assetsensitivity1)
    assetrisk2 = (input$assetvalue2*input$assetsensitivity2)
    assetrisk3 = (input$assetvalue3*input$assetsensitivity3)
    assetrisk1_plan1 = (input$assetvalue1*input$assetsensitivity1_plan1)
    assetrisk2_plan1 = (input$assetvalue2*input$assetsensitivity2_plan1)
    assetrisk3_plan1 = (input$assetvalue3*input$assetsensitivity3_plan1)
    assetrisk1_plan2 = (input$assetvalue1*input$assetsensitivity1_plan2)
    assetrisk2_plan2 = (input$assetvalue2*input$assetsensitivity2_plan2)
    assetrisk3_plan2 = (input$assetvalue3*input$assetsensitivity3_plan2)

    for(j in 1:length(thresholds)-1) { for(i in 1:length(shapes)) { 
        damagej1 = wt1*sigmoid(thresholds[j],input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(thresholds[j],input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint)
        damagej2 = wt1*sigmoid(thresholds[j+1],input$sigmoidlimit,input$sigmoidsteepness,input$sigmoidmidpoint) + wt2*quadratic(thresholds[j+1],input$quadraticlimit,input$quadraticshape,input$quadraticmidpoint)

	damageji1 <- max( -input$assetvalue1, 0.01 * assetrisk1 * 0.5*(damagej2 + damagej1) )
	damageji2 <- max( -input$assetvalue2, 0.01 * assetrisk2 * 0.5*(damagej2 + damagej1) )
	damageji3 <- max( -input$assetvalue3, 0.01 * assetrisk3 * 0.5*(damagej2 + damagej1) )
        damage[j,i] <- damageji1 + damageji2 + damageji3

	damageji1_plan1 <- max( -input$assetvalue1, 0.01 * assetrisk1_plan1 * 0.5*(damagej2 + damagej1) )
	damageji2_plan1 <- max( -input$assetvalue2, 0.01 * assetrisk2_plan1 * 0.5*(damagej2 + damagej1) )
	damageji3_plan1 <- max( -input$assetvalue3, 0.01 * assetrisk3_plan1 * 0.5*(damagej2 + damagej1) )
        damage_plan1[j,i] <- damageji1_plan1 + damageji2_plan1 + damageji3_plan1

	damageji1_plan2 <- max( -input$assetvalue1, 0.01 * assetrisk1_plan2 * 0.5*(damagej2 + damagej1) )
	damageji2_plan2 <- max( -input$assetvalue2, 0.01 * assetrisk2_plan2 * 0.5*(damagej2 + damagej1) )
	damageji3_plan2 <- max( -input$assetvalue3, 0.01 * assetrisk3_plan2 * 0.5*(damagej2 + damagej1) )
        damage_plan2[j,i] <- damageji1_plan2 + damageji2_plan2 + damageji3_plan2

       } }

    for(j in 1:length(thresholds)-1) { for(i in 1:length(shapes)) {
	impacts[j,i] <- damage[j,i]*( pweibull(thresholds[j+1],shapes[i],scales[i]) - pweibull(thresholds[j],shapes[i],scales[i]) ) 
	impacts_plan1[j,i] <- damage_plan1[j,i]*( pweibull(thresholds[j+1],shapes[i],scales[i]) - pweibull(thresholds[j],shapes[i],scales[i]) ) 
	impacts_plan2[j,i] <- damage_plan2[j,i]*( pweibull(thresholds[j+1],shapes[i],scales[i]) - pweibull(thresholds[j],shapes[i],scales[i]) ) 
       } } 
    
    impactbyperiod <- c(0,0,0,0,0,0,0,0,0)
    impactbyperiod_plan1 = impactbyperiod
    impactbyperiod_plan2 = impactbyperiod

    for(i in 1:length(impactbyperiod)) {
        impactbyperiod[i]=sum(impacts[,i])  
        impactbyperiod_plan1[i]=sum(impacts_plan1[,i])  
        impactbyperiod_plan2[i]=sum(impacts_plan2[,i])  
       }

    impactbyperiod_relative2baseperiod = impactbyperiod - impactbyperiod[1]
    impactbyperiod_relative2baseperiod_plan1 = impactbyperiod_plan1 - impactbyperiod_plan1[1]
    impactbyperiod_relative2baseperiod_plan2 = impactbyperiod_plan2 - impactbyperiod_plan2[1]

    labels <- c("No Adaptation","Adaptation Plan 1","Adaptation Plan 2")
    plot(impactbyperiod_relative2baseperiod, type="l", lwd=3, lty=1, col=colors[1], xlab="Periods", ylab="Impact on Asset Value ($M)", xaxt="n")
	axis(1, at=c(1:length(periods)), labels=periods)
    lines(impactbyperiod_relative2baseperiod_plan1, type="l", lwd=3, lty=1, col=colors[3])
    lines(impactbyperiod_relative2baseperiod_plan2, type="l", lwd=3, lty=1, col=colors[5])
    legend("topright", inset=.01, title="Scenarios", labels, lwd=3, lty=c(1,1,1), col=c(colors[1],colors[3],colors[5]))
  })

# Added in Build7
  # Adaptation-plan benefits.
  output$adaptationbenefit1 <- renderText({
	benefit_plan1 = "WORKING ON THIS"
        paste("Plan 1 Benefit:   ", benefit_plan1)
  })
  output$adaptationbenefit2 <- renderText({
	benefit_plan2 = "WORKING ON THIS"
        paste("Plan 2 Benefit:   ", benefit_plan2)
  })

# Terry -----------------------------------------------------------


} # end server

shinyApp(ui, server, enableBookmarking = "url")

