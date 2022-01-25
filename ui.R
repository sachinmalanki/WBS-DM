#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(readr)
library(leaflet)
library(leaflet.extras)

#Read food hygiene rating data
final_Scot <-
  read_csv("final_Scot.csv")

final_Scot$LocalAuthorityName <-
  as.factor(final_Scot$LocalAuthorityName)
level_scot_local <- levels(final_Scot$LocalAuthorityName)
final_Scot$BusinessType <- as.factor(final_Scot$BusinessType)
level_scot_type <- levels(final_Scot$BusinessType)
final_others <-
  read_csv("final_others.csv")

final_others$LocalAuthorityName <-
  as.factor(final_others$LocalAuthorityName)
final_others$RatingValue <- as.factor(final_others$RatingValue)
level_others_local <- levels(final_others$LocalAuthorityName)
final_others$BusinessType <- as.factor(final_others$BusinessType)
level_others_type <- levels(final_others$BusinessType)

ui <- dashboardPage(
  # set ui headtitle
  dashboardHeader(title = "FOOD RATING IN UK"),
  
  # set sidebar ui
  dashboardSidebar(sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("chart-bar")),
    menuItem(
      "Scotland Map",
      tabName = "mapScot",
      icon = icon("globe-europe")
    ),
    menuItem(
      "Rest of UK Map",
      tabName = "mapOthers",
      icon = icon("globe-europe")
    ),
    
    menuItem("Data Overview",
             tabName = "data",
             icon = icon("table"))
  )),
  
  # set main body output
  dashboardBody(tabItems(
    # First tab content：to establish all data we have for scotish and other regions
    tabItem(tabName = "overview",
            fluidRow(h1("Overview")),
            fluidRow(
              column(
                width = 6,
                box(
                  title = "Overview - Scotland",
                  width = 12,
                  selectInput(
                    "input_overview_scot",
                    label = h5("LocalAuthorityName"),
                    
                    choices = list("ALL" = "ALL", level_scot_local = level_scot_local),
                    
                    selected = "ALL"
                  ),
                  fluidRow(style = "margin: 5%"),
                  
                  selectInput(
                    "input_overview_scot_2",
                    label = h5("BusinessType"),
                    choices = list("ALL" = "ALL", level_scot_type = level_scot_type),
                    
                    selected = "ALL"
                  ),
                  fluidRow(style = "margin: 5%"),
                  tableOutput("scot_overview_table"),
                  plotOutput("overview_scot"),
                  tableOutput("scot_overview_percentage"),
                  plotOutput("overview_scot_pie")
                )
              ),
              
              column(
                width = 6,
                box(
                  title = "Overview - Rest of UK",
                  width = 12,
                  selectInput(
                    "input_overview_others",
                    label = h5("LocalAuthorityName"),
                    
                    choices = list("ALL" = "ALL", level_others_local = level_others_local),
                    
                    selected = "ALL"
                  ),
                  fluidRow(style = "margin: 5%"),
                  selectInput(
                    "input_overview_others_2",
                    label = h5("BusinessType"),
                    
                    choices = list("ALL" = "ALL", level_others_type = level_others_type),
                    
                    selected = "ALL"
                  ),
                  fluidRow(style = "margin: 5%"),
                  tableOutput("others_overview_table"),
                  plotOutput("overview_others"),
                  tableOutput("others_overview_percentage"),
                  plotOutput("overview_others_pie")
                )
              )
              
            )),
    
    #-----------------------------
    
    #Second tab for datalab
    tabItem(tabName = "data",
            navbarPage(
              title = "DataLab",
              
              # create a panel for scotish data
              tabPanel(
                "Scotland",
                
                # create two select box for business types and rating value
                fluidRow(column(
                  6,
                  selectInput("bustype",
                              "BusinessType:",
                              c("All",
                                unique(
                                  as.character(final_Scot$BusinessType)
                                )))
                ),
                column(
                  6,
                  selectInput("rate",
                              "RatingValue:",
                              c("All",
                                unique(
                                  as.character(final_Scot$RatingValue)
                                )))
                )),
                
                # import Scottish data
                DT::dataTableOutput("Scot_table"),
                style = "overflow-y: scroll"
              ),
              
              # create a panel for others regions data
              tabPanel(
                "England / Northern Ireland / Wales",
                
                # create two select box for business types and rating value
                fluidRow(column(
                  6,
                  selectInput("Bustype",
                              "BusinessType:",
                              c("All",
                                unique(
                                  as.character(final_others$BusinessType)
                                )))
                ),
                column(
                  6,
                  selectInput("Rate",
                              "RatingValue:",
                              c("All",
                                unique(
                                  as.character(final_others$RatingValue)
                                )))
                )),
                DT::dataTableOutput("Others_table"),
                style = "overflow-y: scroll"
              )
            )),
    #-------------------------------------------
    #Tab content for Scotland
    tabItem(
      tabName = "mapScot",
      fillPage(
        tags$head(
          tags$style(
            ".control-label {margin-left: 1%; width: 400px; font-family: Lato; color:black; }",
            ".item {font-family: Lato; color:grey;}",
            ".row {height: 100%; width: 80vw; background: #ffffff;}",
            "#mymap{ background:#ffffff ; outline: 0; margin-top:20px}",
            "div.form-group.shiny-input-container {height: 40px; }",
            "div.info.legend.leaflet-control {font-weight:100; font-family: Lato; color: gray; font-size:10px }",
            "#desc {margin-left: 2%; color: gray; font-family: Lato, width: 300px;}",
            "#helptext {margin-left: 2%; color: gray; font-family: Lato, width: 300px;}",
            "html, body {width:100%;height:100%;overflow:visible;}"
          )
        ),
        
        h3(id = "big-heading", "How safe is the food here?"),
        tags$style(
          HTML(
            "#big-heading{margin-left: 2%; color: gray; font-family: Lato, width: 400px; margin-bottom: 5px}"
          )
        ),
        p(id = "helptext", helpText("Business food hygiene in Scotland")),
        tags$style(
          HTML(
            "#helptext{margin-left: 2%; color: gray; font-family: Lato, width: 400px; margin-bottom: 5px}"
          )
        ),
        
        
        mainPanel(
          tags$head(tags$style(
            HTML('.container-fluid {width: 100vw; padding: 0, margin 0 auto;}')
          )),
          fluidRow(
            column(
              4,
              selectInput(
                'authorityScot',
                'Select Local Authority',
                unique(final_Scot$LocalAuthorityName),
                selectize = TRUE
              )
            ),
            
            column(
              4,
              selectInput(
                'bustypeScot',
                'Select Business Type',
                unique(final_Scot$BusinessType),
                selectize = TRUE
              )
            ),
            
            column(
              4,
              selectInput(
                'valueScot',
                'Select Rating Value',
                unique(final_Scot$RatingValue),
                selectize = TRUE
              )
            )
          ),
          
          fluidRow(column(
            width = 12,
            leafletOutput("scot_map", "100%", "400")
          ))
        )
        
      )
    ),
    
    #--------------------------------------------------------------
    # Next tab content for Rest of UK
    tabItem(
      tabName = "mapOthers",
      fillPage(
        tags$head(
          tags$style(
            ".control-label {margin-left: 1%; width: 400px; font-family: Lato; color:black; }",
            ".item {font-family: Lato; color:grey;}",
            ".row {height: 100%; width: 80vw; background: #ffffff;}",
            "#mymap{ background:#ffffff ; outline: 0; margin-top:20px}",
            "div.form-group.shiny-input-container {height: 40px; }",
            "div.info.legend.leaflet-control {font-weight:100; font-family: Lato; color: gray; font-size:10px }",
            "#desc {margin-left: 2%; color: gray; font-family: Lato, width: 300px;}",
            "#helptext {margin-left: 2%; color: gray; font-family: Lato, width: 300px;}",
            "html, body {width:100%;height:100%;overflow:visible;}"
          )
        ),
        
        h3(id = "big-heading", "Does food safe here?"),
        tags$style(
          HTML(
            "#big-heading{margin-left: 2%; color: gray; font-family: Lato, width: 400px; margin-bottom: 5px}"
          )
        ),
        p(id = "helptext", helpText("Business food hygiene in Scotish")),
        tags$style(
          HTML(
            "#helptext{margin-left: 2%; color: gray; font-family: Lato, width: 400px; margin-bottom: 5px}"
          )
        ),
        
        
        mainPanel(
          tags$head(tags$style(
            HTML('.container-fluid {width: 100vw; padding: 0, margin 0 auto;}')
          )),
          fluidRow(
            column(
              4,
              selectInput(
                'authorityOthers',
                'Select Local Authority',
                unique(final_others$LocalAuthorityName),
                selectize = TRUE
              )
            ),
            
            column(
              4,
              selectInput(
                'bustypeOthers',
                'Select Business Type',
                unique(final_others$BusinessType),
                selectize = TRUE
              )
            ),
            
            column(
              4,
              selectInput(
                'valueOthers',
                'Select Rating Value',
                unique(final_others$RatingValue),
                selectize = TRUE
              )
            )
          ),
          
          fluidRow(column(
            width = 12,
            leafletOutput("other_map", "100%", "400")
          ))
        )
        
      )
    )
    
    
  ))
)