library(shiny)
library(shinydashboard)
library(leaflet)

#ui 
ui <- dashboardPage(
  dashboardHeader(title="Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Homeless Analysis", tabName = "dashboard", icon = icon("bar-chart-o"),
               menuSubItem(
                           selectInput(inputId = "recos",
                                       label="Analysis Based on",
                                       choices = list ( 
                                                "Homeless Density and Shelter Optimization",
                                                "Mental Health Institutes by Location",
                                                "Crime Analysis"
                                        ), 
                                       selected = "Homeless Density and Shelter Optimization",
                                       width = '100%')
                           )
              )
      
    )
    ),
  dashboardBody(
    tags$head(tags$style(HTML('
    /* Change padding of sub-menu items */
                              .sidebar .sidebar-menu .treeview-menu>li>a {
                              padding: 1px 1px 1px 1px;
                              }
                              
                              /* Hide icons in sub-menu items */
                              .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                              display: none;
                              }
                              '))),
    tags$style(type = "text/css", "#myMap1 {height: calc(100vh - 80px) !important;}"),
    fluidRow(
      box(
        leafletOutput("myMap1"),
        width="100%"
      )
    )
    )
  )


#starting server
server <- function(input, output){
  source("Maps_LA_City_Project.R")
  map <- reactive({
    
  switch(input$recos,
           "Homeless Density and Shelter Optimization"= map_density_optimization,
           "Mental Health Institutes by Location" = map_mental_health,
           "Crime Analysis" = map_crime
           )
  })
  output$myMap1 <- renderLeaflet({map()})
}
shinyApp(ui, server)