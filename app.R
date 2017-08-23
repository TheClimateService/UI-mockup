## app.R ##
library(shinydashboard)
library(leaflet)

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
                  valueBox(1, "Portfolios", icon = icon("briefcase"), color = "teal"),
                  valueBox(3, "Projects", icon = icon("bullseye"), color = "aqua"),
                  valueBox(2, "Plans", icon = icon("line-chart"), color = "blue")
              )
      ),
      
      # Second tab content
      tabItem(tabName = "settings",
              fluidRow(
                box(
                  title = "Basics",
                  textInput("companyName","Company Name",value="Micron")
                ),
                box(
                  title = "Locations",
                  textInput("location1","Corporate Headquarters",value = "8000 S Federal Way, Boise, ID 83716"),
                  # leafletOutput("mymap")
                  img(src="grab-micron-map.png", width=400),
                  hr(),
                  actionButton("addLocation","Add a location")
                )
              )
      ),
      
      # Third tab content
      tabItem(tabName = "corporate",
              fluidRow(
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
              box(
                  title = "Compliance Reporting - Task Force on Climate-Related Financial Disclosures (TCFD)"
              )
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
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server, enableBookmarking = "url")