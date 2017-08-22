## app.R ##
library(shinydashboard)

ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Basic dashboard"),

  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Settings", tabName = "settings", icon = icon("cog")),
      menuItem("Corporate", tabName = "corporate", icon = icon("building-o")),
      menuItem("Portfolios", tabName = "portfolios", icon = icon("briefcase")),
      menuItem("Projects", tabName = "projects", icon = icon("bullseye")),
      menuItem("Plans", tabName = "plans", icon = icon("line-chart"))
    )
  ),
  
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "settings",
              fluidRow(
                box(
                  title = "Setup",
                  includeHTML("map.html")
                )
              )
      ),
      
      # Third tab content
      tabItem(tabName = "corporate",
              fluidRow(
                box(
                  title = "Corporate"
                )
              )
      ),
      
      # Fourth tab content
      tabItem(tabName = "portfolios",
              fluidRow(
                box(
                  title = "Portfolios"
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
      )
           
      
    )
  )
)
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)