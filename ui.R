## app.R ##
# References:  	https://rstudio.github.io/shinydashboard/
#		http://rmarkdown.rstudio.com/flexdashboard/
#		https://shiny.rstudio.com/articles/dynamic-ui.html

# Icons:	http://fontawesome.io/icons/
#		http://getbootstrap.com/components/#glyphicons

# Notes:
#
#

library(shinydashboard)
library(flexdashboard)
library(leaflet)
library(MASS)
library(survival)
library(RColorBrewer)
display.brewer.all()
library(tidyverse)
library(RSQLite)
library(quantmod)
library(hazus)
library(reshape2)
library(ggplot2)
library(Lmoments)
library(MatrixModels)
library(RcppEigen)
library(SparseM)
library(car)
library(distillery)
library(evd)
library(extRemes)
library(shinyjs)

# Data
# US SLR projections and historical extreme water levels.  Variables created are "proj" and "ewl".
source("./data/sealevel_us/load_sealevel_data_us.r")
source("./data/sealevel_us/function_annual_probability_withslr.r")

# Drought data.  Variable created is "d".
source("./data/drought/load_drought_data.r")

source("./data/financial/load_financial_data.r")

# Load hazus flood-depth damage functions and list of hazus building flood damage functions.
fl_dept <- extract_hazus_functions()
hazus_building_flood_damage_function_names = read.table("./data/hazus/hazus_flood_depth_damage.csv.bldg.list", header=TRUE)

ui <- dashboardPage(
	skin="black",
  #includeScript("www/message-handler.js"),

  dashboardHeader(
	title = img(src="logo-TCS-small.png", height = 50, align = "left")
  ),

  ## Sidebar content
  dashboardSidebar(
	width=280,
	useShinyjs(),
	sidebarMenu(id = "sidebar",
    	menuItem("Log In", tabName = "login", icon = icon("lock")),
#	    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
#     menuItem("Settings", tabName = "settings", icon = icon("cog")),
      menuItem("Corporate Risk Analyzer", tabName = "corporate", icon = icon("building-o")),
      menuItem("Portfolio Analyzer", tabName = "portfolios", icon = icon("briefcase")),
#     menuItem("Compliance Reporter", tabName = "plans", icon = icon("line-chart"),
#         menuSubItem("Compliance - TCFD", tabName = "TCFD")
#     ), #menuItem
      menuItem("Technical Details", tabName = "overview", icon = icon("podcast"),
      	  menuSubItem("LOCALIZED CLIMATE PROBABILITIES", tabName = "localclimate", icon = icon("cubes")),
      	  menuSubItem("SECTOR IMPACT FUNCTIONS", tabName = "impactfunctions", icon = icon("cloud-download", lib = "glyphicon")),
      	  menuSubItem("PROBABLISTIC IMPACT ESTIMATE", tabName = "impactestimate", icon = icon("cog", lib = "glyphicon")),
      	  menuSubItem("FINANCIAL EFFECTS", tabName = "financialeffects", icon = icon("usd", lib = "glyphicon")),
      	  menuSubItem("SUSTAINABLE INFRASTRUCTURE", tabName = "sustainable_infrastructure", icon = icon("tree-conifer", lib = "glyphicon")),
      	  menuSubItem("ADAPTATION BENEFIT/COST PLANNING", tabName = "adaptationplanning", icon = icon("tree-deciduous", lib = "glyphicon")),
      	  menuSubItem("OVERALL CLIMATE SCORE", tabName = "climatescore", icon = icon("certificate", lib = "glyphicon")),
      	  menuSubItem("DATABASE", tabName = "database", icon = icon("database")),
      	  menuSubItem("Links", tabName = "links", icon = icon("external-link"))
      ) #menuItem
    	) #sidebarMenu
  ), #dashBoardSidebar

  ## Body content
  dashboardBody(
    tabItems(
    
      # Login tab content
      tabItem(tabName = "login",
              fluidRow(
                tabBox(
                  tabPanel(title = "Log In",
                    textInput("username","User Name",value=""),
                    passwordInput("userpass","Password",value=""),
                    textOutput("login_response"),
                    disabled(actionButton("btnLogin","Log in"))
                    ) #tabPanel
                  ) #tabBox
              )#fluidRow
        ), #tabItem

      # First tab content
      # tabItem(tabName = "dashboard",
      #         fluidRow(
      #             infoBox("TCS Climate Score", 437, icon = icon("thermometer-3"), color = "red")
      #         ),
      #         fluidRow(
      #             infoBox("Locations", 14, icon = icon("map-marker"), color = "teal"),
      #             infoBox("Users", 3, icon = icon("user"), color = "aqua"),
      #             infoBox("Plans", 2, icon = icon("line-chart"), color = "blue")
      #         )
      # ),

# SETTINGS tab content
      tabItem(tabName = "settings",
              fluidRow(
                tabBox(
                  tabPanel(title = "Basics",
                    textInput("companyName","Company Name",value="Micron"),
                    selectInput("cbGroupBizType","Industry Sector",
                      c("Beverage","Agriculture","Packaged Foods/Meats","Paper & forest","Manufacturing","Metals & mining", "Chemicals", "Real Estate Management/Development","Transportation","Oil & gas","Electric Utilities"),
                      selected = c("Manufacturing")
                    )
                  ),
		tabPanel(title = "Location 1",
                    textInput("location1","Corporate Headquarters",value = "8000 S Federal Way, Boise, ID 83716"),
                    htmlOutput("map_micron_boise"),
                    hr(),
                    checkboxGroupInput("cbBusinessFunctions","Business functions performed at this location",
                                c("Clean Room Manufacturing","Shipping","Inventory Management","R&D","HR","Legal","Marketing/Sales","Corporate Governance"),
                                selected = c("Clean Room Manufacturing","R&D")
                    ),
                    hr(),
                    actionButton("addLocation","Add a location")
                  ),
                  tabPanel(title = "Users",
                    valueBox(1, "Julia Grant", icon = icon("user"), color = "teal"),
                    valueBox(1, "Joe Robinson", icon = icon("user"), color = "teal"),
                    valueBox(1, "Norm Armour", icon = icon("user"), color = "teal"),
                    actionButton("addUser","Add a user")
                  ),
		tabPanel(title = "Location 2",
                    textInput("location2","Singapore Facilities",value = "4 facilities in Singapore"),
                    htmlOutput("map_micron_singapore"),
                    hr(),
                    checkboxGroupInput("cbBusinessFunctions","Business functions performed at this location",
                                c("Clean Room Manufacturing","Shipping","Inventory Management","R&D","HR","Legal","Marketing/Sales","Corporate Governance"),
                                selected = c("Clean Room Manufacturing","R&D")
                    ),
                    hr(),
                    actionButton("addLocation","Add a location")
                  ),
                  tabPanel(title = "Users",
                    valueBox(1, "TBD", icon = icon("user"), color = "teal"),
                    valueBox(1, "TBD", icon = icon("user"), color = "teal"),
                    valueBox(1, "TBD", icon = icon("user"), color = "teal"),
                    actionButton("addUser","Add a user")
                  )
                )#tabBox
              )#fluidRow
            ),

# CORPORATE tab content
      tabItem(tabName = "corporate",
        h2("Corporate Risk Analyzer"),
        fluidRow(
          tabBox(width=12,
            tabPanel(title="Settings",
               selectizeInput(
                 "selected_nasdaq","Search Companies",
                 choices = names,  # [from source("./data/financial/load_financial_data.r") ]
                 options = list(placeholder='Type company name', onInitialize = I('function() { this.setValue(""); }'))
               ),#selectizeInput
               "Some other config things"
            ),#tabPanel
            tabPanel(title="Financials",
              fluidRow(
                column(4,
                  selectInput('inputLocations',"Locations",c('All locations','Boise','Ireland','Singapore'),selectize = TRUE)
                ),
                column(4,
                  selectInput('inputYear',"Year",c('2018','2019','2020','2021','2022','2023','2024','2025','2026','2027','2028'),selectize = TRUE)
                ),
                column(4,
                  selectInput('inputScenario',"Scenario",c('RCP8.5','RCP2.6'),selectize = TRUE)
                )
              ),
              dataTableOutput("corpFinImpacts")
            ),#tabPanel
            tabPanel(title="TCFD",width="100%",
               fluidRow(
                 tabBox(width="100%",
                        tabPanel(title = "Governance",
                                 textAreaInput("TCFD-Gov-a","Board Oversight", value="Describe the board's oversight of climate-related risks and opportunities."),
                                 textAreaInput("TCFD-Gov-b","Management's Role", value="Describe management's role in assessing and managing climate-related risks and opportunities.")
                        ),
                        tabPanel(title = "Strategy",
                                 textAreaInput("TCFD-Strat-a","Climate Risks and Opportunities", value = "Describe the climate-related risks and opportunities the organization has identified over the short, medium, and long term."),
                                 checkboxGroupInput("chkbxRisks","Risks & Opportunities", c("Policy & Legal Risk","Technology Risk","Market Risk","Reputation Risk","Acute Physical Risk","Chronic Physical Risk","Resource Efficiency","Energy Source","Products/Services","Markets","Resilience"),selected = c("Energy Source","Acute Physical Risk","Chronic Physical Risk")),
                                 textAreaInput("TCFD-Strat-b","Impact of Risks", value = "Describe the impact of climate-related risks and opportunities on the organization's businesses, strategy, and financial planning.")
                        ),
                        tabPanel(title = "Risk Management",
                                 textAreaInput("TCFD-Gov-a","Processes for Identifying Risks", value="Describe the processes for identifying & assessing climate-related risks and opportunities."),
                                 textAreaInput("TCFD-Gov-b","Processes for Managing", value="Describe the processes managing climate-related risks."),
                                 textAreaInput("TCFD-Gov-a","Process Integration", value="Describe how processes for identifying, assessing, and managing climate-related risks are intgrated into the organization's overall management.")
                        ),
                        tabPanel(title = "Metrics and Targets"),
                        tabPanel(title = "Report",
                                 actionButton("TCFD-Report", "Export Report")
                        )
                 )#tabBox
               )#fluidRow
            )#tabPanel
          ) #tabBox
        ) #fluidrow
              
# This is James's old code to animate impacts - probably remove
#               fluidRow(
#                 tabBox(width = "500",
# 		              tabPanel(title="Screening level",
#                     selectInput("cbLocation","Location",
#                                c("Boise","Singapore","Malaysia","Scotland"),
#                                selected = c("Boise")
#                     ),
#                     sliderInput("siTimeframe", "Timeframe", 1, 30, 5, step=1, animate=TRUE),
#                     htmlOutput('txtImpact1'),
#                     htmlOutput('txtImpact2'),
#                     htmlOutput('txtImpact3'),
#                     hr(),
#                     infoBoxOutput('infobox1')
#                   )#tabPanel
#                 )#tabBox
#               )#fluidRow
      ),#tabItem corporate

      # Fourth tab content
      tabItem(tabName = "portfolios",
        h2("Investment Portfolio Analyzer"),
        fluidRow(
          tabBox(width=500,
            tabPanel(
              title = "Set up",
              actionButton("addStock","Add a Listed Equity")
            ),
          tabPanel(title="Analyze",
          	fluidRow(
        	  	column(6,
            		selectInput("selected_nasdaq","Search Companies", width=400,
        	     		names,  # [from source("./data/financial/load_financial_data.r") ]
        	     		selected = "Apple Inc. - Common Stock"
                )#selectInput
        		  ),#column
          		column(3, offset=1,
          			tags$head(tags$script(src = "message-handler.js")),
          			actionButton("add2portfolio", "ADD TO PORTFOLIO")
          		) #column
          	), #fluidrow

          fluidRow(
  	  	    column(12, offset=0,
            		box(title="Symbol, Name, and Exchange", background = "aqua", solidHeader = TRUE, textOutput("stockselected"))
          	), #fluidrow
            fluidRow(
    	  	    column(6, offset=2,h2("Cx SCORE"),gaugeOutput("stock_overall_score_gauge")),
    		      column(2, offset=0, checkboxInput("showmore_overall_score", "SHOW MORE") )
        		) #fluidRow
        	), #fluidrow
	  
	 	conditionalPanel(condition = "input.showmore_overall_score == true",
        	fluidRow(
	  #	column(4, offset=1,
          #		box(title="Financials", background = "aqua", solidHeader = TRUE, textOutput("stock_financial_parameters"))
	#	      ),
	  	column(3,h2("Financials")
                       # , textOutput("stock_financial_parameters")
                      ),
	  	column(2,h4("Revenue Risk"),gaugeOutput("stock_financial_gauge1")),
	  	column(2,h4("Expense Risk"),gaugeOutput("stock_financial_gauge2")),
	  	column(2,h4("Equity/Share VaR"),gaugeOutput("stock_financial_gauge3")),
		column(2, offset=0, checkboxInput("showmore_financial", "SHOW MORE"))
        	), #fluidrow

        	fluidRow(
	 	conditionalPanel(condition = "input.showmore_financial == true",verbatimTextOutput("stock_financial_factors")) 
        	), #fluidrow

        	fluidRow(
	  	#column(4, offset=1,
          	#	box(title="Transition Risk", background = "aqua", solidHeader = TRUE, textOutput("stock_transition_parameters"))
		#      ),
	  	column(3,h2("Transition Risk")
			#,textOutput("stock_transition_parameters")
		),
	  	column(2,h4("Legal & Policy"),gaugeOutput("stock_transition_gauge1")),
	  	column(2,h4("Technology"),gaugeOutput("stock_transition_gauge2")),
	  	column(2,h4("Reputation"),gaugeOutput("stock_transition_gauge3")),
		column(2, offset=0, checkboxInput("showmore_transition", "SHOW MORE"))
        	), #fluidrow

        	fluidRow(
	 	conditionalPanel(condition = "input.showmore_transition == true",verbatimTextOutput("stock_transition_factors")) 
        	), #fluidrow

        	fluidRow(
	  	#column(4, offset=1,
          	#box(title="Physical Risk", background = "aqua", solidHeader = TRUE, textOutput("stock_physical_parameters"))
		#      ),
	  	column(3,h2("Physical Risk")
			#,textOutput("stock_physical_parameters")
		),
	  	column(2,h4("Temperature"),gaugeOutput("stock_physical_gauge_temperature")),
	  	column(2,h4("Sea Level Rise"),gaugeOutput("stock_physical_gauge_slr")),
	  	column(2,h4("Drought"),gaugeOutput("stock_physical_gauge_drought")),
		column(2, offset=0, checkboxInput("showmore_physical", "SHOW MORE"))
        	), #fluidrow

        	fluidRow(
	 	conditionalPanel(condition = "input.showmore_physical == true",verbatimTextOutput("stock_physical_factors")) 
        	), #fluidrow

        	fluidRow(
	  	#column(4, offset=1,
          	#box(title="Opportunities", background = "aqua", solidHeader = TRUE, textOutput("stock_opportunity_parameters"))
		#      ),
	  	column(3,h2("Opportunities")
			#,textOutput("stock_opportunity_parameters")
		),
	  	column(2,h4("Resource Efficiency"),gaugeOutput("stock_opportunity_gauge1")),
	  	column(2,h4("Energy Mix"),gaugeOutput("stock_opportunity_gauge2")),
	  	column(2,h4("New Products"),gaugeOutput("stock_opportunity_gauge3")),
		column(2, offset=0, checkboxInput("showmore_opportunity", "SHOW MORE"))
        	), #fluidrow

        	fluidRow(
	 	conditionalPanel(condition = "input.showmore_opportunity == true",verbatimTextOutput("stock_opportunity_factors")) 
        	) #fluidrow

		) #conditionalPanel

		) #tabPanel
              )
              )
      ), #tabItem

 
      tabItem(tabName = "links", 
	#uiOutput("googlelink"),
	h2( uiOutput("ndgain_countries") ),
	h2( uiOutput("actuaries_climate_index") ),
	h2( uiOutput("worldbank_development_indicators") )
      ), #tabItem

      tabItem(tabName = "localclimate",
        h2("Localized probability distributions from historical and projected daily data"),
        
	tabBox(width=12,

        tabPanel(title = "TEMPERATURE",
	icon = icon("thermometer-three-quarters"),
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
        ) #fluidrow
	),  #tabPanel

        tabPanel(title = "SEA LEVEL",
	icon = icon("bath"),

        fluidRow(
	  column(6,
          selectInput("sealevelProjectionLocation","Select Location for Sea Level Projections",
	     noaa_slr_locations,  # [from source("./data/sealevel_us/load_sealevel_data_us.r") ]
             selected = "This is a dummy value that gets the list to load the first data value since there are multiple rows for each location covering the different GMSL scenarios"
                    )
		),
	  
	  column(6,
          selectInput("extremewaterLocation","Select Location for Extreme Water Levels",
             # list(`East Coast` = c("NY", "NJ", "CT"),
             #      `West Coast` = c("WA", "OR", "CA"),
             #      `Midwest` = c("MN", "WI", "IA"))
             # c("Boise","Singapore","Malaysia","Scotland"),
	     noaa_extremes_locations, # [ from source("./data/sealevel_us/load_sealevel_data_us.r") ]
             selected = "KingsPoint/WilletsPoint"
                    )
		)
        ), #fluidrow

        fluidRow(
	  column(4,
          selectInput("slrScenario","Select Sea Level Rise Scenario (GMSL 2100)",
	     c("0.3 meter","0.5 meter","1.0 meter","1.5 meters","2.0 meters", "2.5 meters"),
             selected = "0.5 meter"
                    )
		),
	  
	  column(3,
          selectInput("slrYear","Select Year of Concern",
	     c("2020","2030","2040","2050","2060","2070","2080","2090","2100"),
             selected = "2020"
                    )
		),
	  
	  column(3,
          selectInput("returnLevel","Select Return Level of Concern (m)",
	     c(0.5,1.0,1.5,2.0,2.5,3.0,4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0,8.5,9.0,9.5,10.0),
	     #c("1","2","3","4", "5", "6", "7", "8", "9", "10"),
	     #c(1:10),
             selected = "1"
                    )
		)

	  #column(3,
          #box(title="Probability of Exceeding Selected Level", background = "red", solidHeader = TRUE, textOutput("returnlevel_probability"))
	#	)
        ), #fluidrow

        fluidRow(
          box(title="Probability of Exceeding Selected Level - Historical and Selected Period", background = "red", solidHeader = TRUE, textOutput("returnlevel_probability")),
          box(title="Probability of Exceeding Selected Level - All Periods", background = "red", solidHeader = TRUE, plotOutput("sealevel_ewl_probabilities", height=300))
        ), #fluidrow

        fluidRow(
          box(title="Local Sea-level Rise Projections at Selected Location", background = "red", solidHeader = TRUE, plotOutput("sealevel_projections_plot1", height = 300)),
          box(title="Historical Extreme Water Levels at Selected Location", background = "red", solidHeader = TRUE, plotOutput("sealevel_extremes_plot1", height = 300))
        ) #fluidrow

	),  #tabPanel

        tabPanel(title = "DROUGHT",
	icon = icon("sun-o"),
        fluidRow(
	  h3("Annual Probability of Exceeding the Historical (1950-99) 90th-percentile Value of the Palmer Drought Severity Index"),

         column(6,
          selectInput("drought_facility","Search Company Facilities", width=400,
                      facility_locations),  # [from source("./data/financial/load_financial_data.r") ]
	  checkboxInput("use_facility_for_drought", "USE THIS FACILITY FOR DROUGHT IMPACTS")
          ),
         column(6,
	  textInput("droughtlon","Specify Other Longitude (E+/W-)",value="19.9"),
          textInput("droughtlat","Specify Other Latitude (N+/S-)",value="-16.3")
          )
         ), #fluidrow

        fluidRow(
	  box(title="Annual Probabilty at Selected Facility", background = "blue", solidHeader = TRUE, plotOutput("drought_frequencies_facility", height=300)),
	  box(title="Annual Probability at Selected Location", background = "blue", solidHeader = TRUE, plotOutput("drought_frequencies_lonlat", height=300))
          ) #fluidrow
	)  #tabPanel

	)  #tabBox
      ), #tabItem

      tabItem(tabName = "impactfunctions",
        h2("Sector-specific impact functions quantify impacts on infrastructure, workforce, revenue..."),
        tabBox(width=12,
 
        tabPanel(title = "FUNCTION BUILDER",
		 icon = icon("wrench"), 
	fluidRow(
          box(title="Impact Function - Tailored", background = "red", solidHeader = TRUE, plotOutput("impactplot3", height = 200)),
          box(
            title = "Weight of Sigmoidal and Quadratic Contributions",
            sliderInput("impactfunctionweight", "Sigmoid weight (quadratic weight is the remainder):", 0, 1, 0.5, step=0.1, animate=TRUE)
          )
	),
        
	fluidRow(
          box(title="Sigmoidal Contribution", background = "blue", solidHeader = TRUE, plotOutput("impactplot1", height = 200)),
          box(title="Quadratic Contribution", background = "blue", solidHeader = TRUE, plotOutput("impactplot2", height = 200)),
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
        )
	), #tabPanel

	tabPanel(title = "FITTED", 
		 icon = icon("line-chart"), 
                 #helpText("Fitted functions linked to climate variables"),
	fluidRow(
	  column(3,
	  h4("Electricity Load Vs. Daily Average Temperature"),
          imageOutput("impactplot_elecload", height = 500, width=500)
	  ),

	  column(6, offset=3,
	  h4("Building Damage Vs. Flood Depth"),
          #selectInput("hazus_damage_function_id","Damage Function Number",c(1:657)),
          selectInput("hazus_damage_function_id","Select Damage Function", hazus_building_flood_damage_function_names),
          imageOutput("impactplot_building_flood", height = 500, width=500)
          #verbatimTextOutput("impactplot_building_flood")
	  )
	), #fluidrow

	fluidRow(
	  column(3, offset=0,
	  h4("Corn Yield Vs. Drought Return Period"),
          imageOutput("impactplot_corn_drought_return_period", height = 500, width=500)
	  ),

	  column(3, offset=3,
	  h4("Agricultural Income in Brazil Vs. Rainfall"),
          imageOutput("impactplot_agriculture_brazil", height = 500, width=500)
	  )
	), #fluidrow

	fluidRow(
	  column(3, offset=0,
	  h4("Maize Yield in U.S. Vs. Temperature"),
          imageOutput("impactplot_maize_us", height = 500, width=300)
	  )
	) #fluidrow

	), #tabPanel

        tabPanel(title = "FUNCTION LIBRARY 1",
		 icon = icon("book"), 
	fluidRow(
	  #h2("=== IMPACT FUNCTION LIBRARY ==="),
	  column(12,
	  h4("Power Consump., GDP, Income, Productivity, etc."),
          imageOutput("impactplot8", height = 500)
	  )
	)
	), #tabPanel

        tabPanel(title = "FUNCTION LIBRARY 2",
		 icon = icon("book"), 
	fluidRow(
	  column(12, offset=0,
	  h4("Mortality, Damage, etc."),
          imageOutput("impactplot7", height = 500)
	  )
	)
	), #tabPanel

        tabPanel(title = "FUNCTION LIBRARY 3",
		 icon = icon("book"), 
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
	), #fluidRow

	fluidRow(
	  column(5, 
	  h4("Corn Yield - Drought (US midwest)"),
          imageOutput("impactplot_crops_wang", height = 300)
	  ),

	  column(4, offset=1,
	  h4("Corn and Soy - Drought, Soil Moisture (US midwest)"),
          imageOutput("impactplot_crops_mishra", height = 300)
	  )
	), #fluidRow

	fluidRow(
	  column(5,
	  h4("Wheat - Evapotranspiration (Canada)"),
          imageOutput("impactplot_crops_mhakbela", height = 300)
	  )
	) #fluidRow
	), #tabPanel

        tabPanel(title = "FUNCTION LIBRARY 4",
		 icon = icon("book"), 
	fluidRow(
	  column(3, 
	  h4("Power Generation - Air/Water Temperature"),
          imageOutput("impactplot9", height = 500)
	  ),

	  column(3, offset=3,
	  h4("Water Requirements for Power - Water Temperature"),
          imageOutput("impactplot10", height = 500)
	  )

	)
      ) #tabPanel  
      ) #tabBox
      ),

      tabItem(tabName = "impactestimate",
        h2("Combine climate probabilities and impact functions to estimate probability-weighted net impact"),
        fluidRow(
                    selectInput("impact_selected","IMPACT FUNCTION SOURCE",
                               c("Custom-built","Electricity Load (US; temperature)","Building Damage (flood depth)", "Corn Yield (US, drought)", "Agricultural Income (Brazil)", "Maize Yield (US)"),
                               selected = c("Custom-built")
                    ),
          box(title="Impact Function (controlled from impact-function tab)", background = "blue", solidHeader = TRUE, plotOutput("impactestimateplot3", height = 300)),

          box(title="Probabilistic Impact Estimate", background = "red", solidHeader = TRUE, plotOutput("impactestimateplot2", height = 300))

#          box(
#            title = "Impact Estimate Controls",
#            sliderInput("sigmoidlimit2", "Sigmoid limit:", -100, 100, -75, step=10, animate=TRUE),
#            sliderInput("sigmoidsteepness2", "Sigmoid steepness:", 0.1, 10, 2, step=0.5, animate=TRUE),
#            sliderInput("sigmoidmidpoint2", "Sigmoid midpoint:", 270, 320, 295, step=1, animate=TRUE)
#          )
	), #fluidRow

        fluidRow(
          box(title="Probability of Exceeding Thresholds (based on localized climate projections", background = "yellow", solidHeader = TRUE, plotOutput("impactestimateplot1", height = 300)),
          box(
            title = "Threshold Controls",
            sliderInput("threshold", "Threshold:", 270, 320, 295, step=2, animate=TRUE)
          )
        )
      ),

      tabItem(tabName = "financialeffects",
        h2("Calculate effects of net impact on beta and NPV"),
        fluidRow(
          box(title="Beta Multiplier By Period: 1/(1-D), where D is probabilistic impact", background = "blue", solidHeader = TRUE, plotOutput("financialplot1", height = 300)),
          box(title="NPV Impact on Project", background = "green", solidHeader = TRUE, plotOutput("financialplot2", height = 300)),
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

      tabItem(tabName = "sustainable_infrastructure",
        #h2("Climate risk assessment and adaptation planning throughout the transaction cycle"),
        h2("Align financial flows with country pathways to low-carbon and climate-resilient development"),
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

      tabItem(tabName = "adaptationplanning",
        h2("Calculate adaptation benefit/cost ratio"),
        fluidRow(
	  box(title="Impact Function (controlled from Sector Impact tab)", background = "blue", solidHeader = TRUE, plotOutput("adaptationplot1", height=300)),
	  box(title="Asset Value Through Time", background = "red", solidHeader = TRUE, plotOutput("adaptationplot2", height=300))
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
            title = "Relative Asset Sensitivities to Impact Function (no adaptation)",
            sliderInput("assetsensitivity1", "Impact Sensitivity of Asset 1:", 0, 5, 1, step=0.25, animate=TRUE),
            sliderInput("assetsensitivity2", "Impact Sensitivity of Asset 2:", 0, 5, 1, step=0.25, animate=TRUE),
            sliderInput("assetsensitivity3", "Impact Sensitivity of Asset 3:", 0, 5, 1, step=0.25, animate=TRUE),
	    width=3
          ),
          box(
            #title = "Adaptation Plan 1",
    	    title = textOutput("adaptationbenefit1"),
            sliderInput("assetsensitivity1_plan1", "Plan 1 Impact Sensitivity of Asset 1:", 0, 5, 0.5, step=0.25, animate=TRUE),
            sliderInput("assetsensitivity2_plan1", "Plan 1 Impact Sensitivity of Asset 2:", 0, 5, 0.5, step=0.25, animate=TRUE),
            sliderInput("assetsensitivity3_plan1", "Plan 1 Impact Sensitivity of Asset 3:", 0, 5, 0.5, step=0.25, animate=TRUE),
            sliderInput("discount2", "Discount Rate:", 0.01, 0.05, 0.03, step=0.005),
            sliderInput("cost2implement_plan1", "Plan 1 Implementation Cost ($M):", 0, 20, 5, step=1, animate=TRUE),
            sliderInput("cost2maintain_relative2base_plan1", "Plan 1 Discounted Maintenance & Repair Costs Relative to No-adaptation Case ($M):", -20, 20, 2, step=1, animate=TRUE),
	    width=3
          ),
          box(
            #title = "Adaptation plan 2",
    	    title = textOutput("adaptationbenefit2"),
            sliderInput("assetsensitivity1_plan2", "Plan 2 Impact Sensitivity of Asset 1:", 0, 5, 0.25, step=0.25, animate=TRUE),
            sliderInput("assetsensitivity2_plan2", "Plan 2 Impact Sensitivity of Asset 2:", 0, 5, 0.25, step=0.25, animate=TRUE),
            sliderInput("assetsensitivity3_plan2", "Plan 2 Impact Sensitivity of Asset 3:", 0, 5, 0.25, step=0.25, animate=TRUE),
            sliderInput("discount3", "Discount Rate:", 0.01, 0.05, 0.03, step=0.005),
            sliderInput("cost2implement_plan2", "Plan 2 Implementation Cost ($M):", 0, 20, 5, step=1, animate=TRUE),
            sliderInput("cost2maintain_relative2base_plan2", "Plan 2 Discounted Maintenance & Repair Costs Relative to No-adaptation Case ($M):", -20, 20, 5, step=1, animate=TRUE),
	    width=3
          )
        )
      ),

      tabItem(tabName = "climatescore",
        h2("Derive climate score from impacts, financial effects, and adaptation actions"),

              fluidRow(
                box(
	            title = h2("Climate Scores"), status = "danger", solidHeader=TRUE,
		    h2(verbatimTextOutput("climatescores")),
  		    # h3("- Higher is better"), br(), h3("- Historical = 100")
  		    h3("Higher is better; historical = 100")
		   )
              ),

        fluidRow(
          box(title="Climate Score By Period (Higher is better; historical=100)", background = "green", solidHeader = TRUE, plotOutput("scoreplot1", height = 300)),
          box(title="Impact Function (controlled from impact-function tab)", background = "blue", solidHeader = TRUE, plotOutput("scoreplot2", height = 300)),

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
      ),

      tabItem(tabName = "database",
        h2("Databases link detailed calculations with user interface"),
        fluidRow(
          box(
    	    title = textOutput("database1_name"),
	    width=3
          ),

          box(
    	    title = textOutput("database2_name"),
	    width=3
          ),

          box(
    	    title = textOutput("database2_table12_contents"),
	    width=6
          ),

          textInput("addquestion","Add Question",value="enter question"),
          textInput("write_new_table","Write New Table",value="No"),
	  textOutput("database2_write")

        ) #end fluidRow
      ) #end database tabItem

    )#tabItems
  )
) #end ui

