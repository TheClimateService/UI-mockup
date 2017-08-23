## app.R ##
library(shinydashboard)

ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "The Climate Service",
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
    sidebarMenu(id = "sidebar",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Settings", tabName = "settings", icon = icon("cog")),
      menuItem("Corporate", tabName = "corporate", icon = icon("building-o")),
      menuItem("Portfolios", tabName = "portfolios", icon = icon("briefcase")),
      menuItem("Projects", tabName = "projects", icon = icon("bullseye")),
      menuItem("Plans", tabName = "plans", icon = icon("line-chart"),
        menuSubItem("Compliance - TCFD", tabName = "TCFD"),
        menuSubItem("Climate Action Plan", tabName = "CAP"),
        menuSubItem("Project Plans", tabName = "ProjectPlans")
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
                    )
                  ),
                  tabPanel(title = "Locations",
                    textInput("location1","Corporate Headquarters",value = "8000 S Federal Way, Boise, ID 83716"),
                    htmlOutput("frame"),
                    hr(),
                    actionButton("addLocation","Add a location")
                  ),
                  tabPanel(title = "Users",
                    valueBox(1, "Maria", icon = icon("user"), color = "teal"),
                    valueBox(1, "Chen", icon = icon("user"), color = "teal"),
                    valueBox(1, "Vijay", icon = icon("user"), color = "teal")
                  )
                )#tabBox
              )#fluidRow
      ),
      
      # Third tab content
      tabItem(tabName = "corporate",
              fluidRow(
                tabBox(
                  tabPanel(title = "Governance",
                    textAreaInput("TCFD-Gov-a","Board Oversight", width = 500, value="Describe the board's oversight of climate-related risks and opportunities."),
                    textAreaInput("TCFD-Gov-b","Management's Role", width = 500, value="Describe management's role in assessing and managing climate-related risks and opportunities.")
                  ),
                  tabPanel(title = "Strategy",
                    textAreaInput("TCFD-Strat-a","Climate Risks and Opportunities", width = 500, value = "Describe the climate-related risks and opportunities the organization has identified over the short, medium, and long term."),
                    checkboxGroupInput("chkbxRisks","Risks & Opportunities", width = 500, c("Policy & Legal Risk","Technology Risk","Market Risk","Reputation Risk","Acute Physical Risk","Chronic Physical Risk","Resource Efficiency","Energy Source","Products/Services","Markets","Resilience"),selected = c("Energy Source","Acute Physical Risk","Chronic Physical Risk")),
                    textAreaInput("TCFD-Strat-b","Impact of Risks", width = 500, value = "Describe the impact of climate-related risks and opportunities on the organization's businesses, strategy, and financial planning.")
                  ),
                  tabPanel(title = "Risk Management")
                ),
                box(
                  title = "Corporate",
                  "Overall Score",
                  "EPS Value at Risk",
                  "Breakdown of risks with quantified impacts to Revenue, Expenses, Assets, Liabilities"
                )
              )
      ),
      
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
                  title = "Projects"
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
                       tabPanel(
                         title = "Governance"
                         ),
                       tabPanel(title="Strategy"),
                       tabPanel(title="Metrics and Targets")
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
              )
    )
  )
)

server <- function(input, output, session) {
  
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
  
  output$frame <- renderUI({
    input$Member
    my_test <- tags$iframe(src="map.html", height=600, width=535)
    print(my_test)
    my_test
  })
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}



shinyApp(ui, server, enableBookmarking = "url")