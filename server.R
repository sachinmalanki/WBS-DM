#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
knitr::opts_chunk$set(echo = TRUE,
                      comment = NA,
                      attr.source = '.numberLines')
library(readr)
library(RSQLite)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(DT)
library(leaflet)
library(leaflet.extras)
library(readr)
library(scales)

final_Scot <-
  read_csv("final_Scot.csv")
final_others <-
  read_csv("final_others.csv")

server <- function(input, output, session) {
  #Tab 1 for Overview of data
  output$overview_scot <- renderPlot({
    to_filter_overview_scot <- input$input_overview_scot
    print(to_filter_overview_scot)
    to_filter_overview_scot_2 <- input$input_overview_scot_2
    print(to_filter_overview_scot_2)
    
    if (to_filter_overview_scot == "ALL" &
        to_filter_overview_scot_2 == "ALL") {
      scot_overview <- final_Scot
    }
    else {
      if (to_filter_overview_scot == "ALL") {
        scot_overview <-
          filter(final_Scot, BusinessType == to_filter_overview_scot_2)
      }
      else {
        if (to_filter_overview_scot_2 == "ALL") {
          scot_overview <-
            filter(final_Scot,
                   LocalAuthorityName == to_filter_overview_scot)
        }
        else {
          scot_overview <-
            filter(
              final_Scot,
              LocalAuthorityName == to_filter_overview_scot &
                BusinessType == to_filter_overview_scot_2
            )
        }
      }
    }
    
    ggplot(scot_overview) + geom_histogram(aes(RatingValue), stat = "count", binwidth =
                                             1)
  })
  
  output$scot_overview_table <- renderTable({
    to_filter_overview_scot <- input$input_overview_scot
    print(to_filter_overview_scot)
    to_filter_overview_scot_2 <- input$input_overview_scot_2
    print(to_filter_overview_scot_2)
    
    if (to_filter_overview_scot == "ALL" &
        to_filter_overview_scot_2 == "ALL") {
      scot_overview <- final_Scot
    }
    else {
      if (to_filter_overview_scot == "ALL") {
        scot_overview <-
          filter(final_Scot, BusinessType == to_filter_overview_scot_2)
      }
      else {
        if (to_filter_overview_scot_2 == "ALL") {
          scot_overview <-
            filter(final_Scot,
                   LocalAuthorityName == to_filter_overview_scot)
        }
        else {
          scot_overview <-
            filter(
              final_Scot,
              LocalAuthorityName == to_filter_overview_scot &
                BusinessType == to_filter_overview_scot_2
            )
        }
      }
    }
    data.frame(Total_Number_of_Business = length(scot_overview$BusinessName))
  })
  
  output$scot_overview_percentage <- renderTable({
    to_filter_overview_scot <- input$input_overview_scot
    print(to_filter_overview_scot)
    to_filter_overview_scot_2 <- input$input_overview_scot_2
    print(to_filter_overview_scot_2)
    
    if (to_filter_overview_scot == "ALL" &
        to_filter_overview_scot_2 == "ALL") {
      scot_overview <- final_Scot
    }
    else {
      if (to_filter_overview_scot == "ALL") {
        scot_overview <-
          filter(final_Scot, BusinessType == to_filter_overview_scot_2)
      }
      else {
        if (to_filter_overview_scot_2 == "ALL") {
          scot_overview <-
            filter(final_Scot,
                   LocalAuthorityName == to_filter_overview_scot)
        }
        else {
          scot_overview <-
            filter(
              final_Scot,
              LocalAuthorityName == to_filter_overview_scot &
                BusinessType == to_filter_overview_scot_2
            )
        }
      }
    }
    scot_overview$RatingValue <-
      as.factor(scot_overview$RatingValue)
    
    cnt <-
      data.frame(
        RatingValue = levels(scot_overview$RatingValue),
        Number = tapply(
          scot_overview$BusinessName,
          INDEX = scot_overview$RatingValue,
          FUN = length
        )
      )
    
    cnt <-
      data.frame(RatingValue = cnt$RatingValue,
                 Percentage = percent(cnt$Number / sum(cnt$Number)))
    
    rownames(cnt) <- NULL
    
    cnt
    
  })
  
  output$overview_scot_pie <- renderPlot({
    to_filter_overview_scot <- input$input_overview_scot
    print(to_filter_overview_scot)
    to_filter_overview_scot_2 <- input$input_overview_scot_2
    print(to_filter_overview_scot_2)
    
    if (to_filter_overview_scot == "ALL" &
        to_filter_overview_scot_2 == "ALL") {
      scot_overview <- final_Scot
    }
    else {
      if (to_filter_overview_scot == "ALL") {
        scot_overview <-
          filter(final_Scot, BusinessType == to_filter_overview_scot_2)
      }
      else {
        if (to_filter_overview_scot_2 == "ALL") {
          scot_overview <-
            filter(final_Scot,
                   LocalAuthorityName == to_filter_overview_scot)
        }
        else {
          scot_overview <-
            filter(
              final_Scot,
              LocalAuthorityName == to_filter_overview_scot &
                BusinessType == to_filter_overview_scot_2
            )
        }
      }
    }
    scot_overview$RatingValue <-
      as.factor(scot_overview$RatingValue)
    
    cnt <-
      data.frame(
        RatingValue = levels(scot_overview$RatingValue),
        Number = tapply(
          scot_overview$BusinessName,
          INDEX = scot_overview$RatingValue,
          FUN = length
        )
      )
    
    cnt <-
      data.frame(
        RatingValue = cnt$RatingValue,
        Number = cnt$Number,
        Percentage = percent(cnt$Number / sum(cnt$Number))
      )
    
    rownames(cnt) <- NULL
    
    ggplot(cnt, aes(x = "", y = Number, fill = RatingValue)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) + scale_fill_grey() + theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 14, face = "bold")
      ) + theme(axis.text.x = element_blank())
    
  })
  
  output$overview_others <- renderPlot({
    to_filter_overview_others <- input$input_overview_others
    print(to_filter_overview_others)
    to_filter_overview_others_2 <- input$input_overview_others_2
    print(to_filter_overview_others_2)
    if (to_filter_overview_others == "ALL" &
        to_filter_overview_others_2 == "ALL") {
      ploth <-
        ggplot(final_others) + geom_histogram(aes(RatingValue), stat = "count", binwidth =
                                                1)
    }
    else {
      if (to_filter_overview_others == "ALL") {
        ploth <-
          ggplot(filter(
            final_others,
            BusinessType == to_filter_overview_others_2
          )) + geom_histogram(aes(RatingValue),
                              stat = "count",
                              binwidth = 1)
      }
      else {
        if (to_filter_overview_others_2 == "ALL") {
          ploth <-
            ggplot(filter(
              final_others,
              LocalAuthorityName == to_filter_overview_others
            )) + geom_histogram(aes(RatingValue),
                                stat = "count",
                                binwidth = 1)
        }
        else {
          ploth <-
            ggplot(
              filter(
                final_others,
                LocalAuthorityName == to_filter_overview_others &
                  BusinessType == to_filter_overview_others_2
              )
            ) + geom_histogram(aes(RatingValue),
                               stat = "count",
                               binwidth = 1)
        }
      }
    }
    
    ploth
  })
  
  output$others_overview_table <- renderTable({
    to_filter_overview_others <- input$input_overview_others
    print(to_filter_overview_others)
    to_filter_overview_others_2 <- input$input_overview_others_2
    print(to_filter_overview_others_2)
    
    if (to_filter_overview_others == "ALL" &
        to_filter_overview_others_2 == "ALL") {
      others_overview <- final_others
    }
    else {
      if (to_filter_overview_others == "ALL") {
        others_overview <-
          filter(final_others,
                 BusinessType == to_filter_overview_others_2)
      }
      else {
        if (to_filter_overview_others_2 == "ALL") {
          others_overview <-
            filter(final_others,
                   LocalAuthorityName == to_filter_overview_others)
        }
        else {
          others_overview <-
            filter(
              final_others,
              LocalAuthorityName == to_filter_overview_others &
                BusinessType == to_filter_overview_others_2
            )
        }
      }
    }
    data.frame(Total_Number_of_Business = length(others_overview$BusinessName))
  })
  
  output$others_overview_percentage <- renderTable({
    to_filter_overview_others <- input$input_overview_others
    print(to_filter_overview_others)
    to_filter_overview_others_2 <- input$input_overview_others_2
    print(to_filter_overview_others_2)
    
    if (to_filter_overview_others == "ALL" &
        to_filter_overview_others_2 == "ALL") {
      others_overview <- final_others
    }
    else {
      if (to_filter_overview_others == "ALL") {
        others_overview <-
          filter(final_others,
                 BusinessType == to_filter_overview_others_2)
      }
      else {
        if (to_filter_overview_others_2 == "ALL") {
          others_overview <-
            filter(final_others,
                   LocalAuthorityName == to_filter_overview_others)
        }
        else {
          others_overview <-
            filter(
              final_others,
              LocalAuthorityName == to_filter_overview_others &
                BusinessType == to_filter_overview_others_2
            )
        }
      }
    }
    others_overview$RatingValue <-
      as.factor(others_overview$RatingValue)
    
    cnt <-
      data.frame(
        RatingValue = levels(others_overview$RatingValue),
        Number = tapply(
          others_overview$BusinessName,
          INDEX = others_overview$RatingValue,
          FUN = length
        )
      )
    
    cnt <-
      data.frame(RatingValue = cnt$RatingValue,
                 Percentage = percent(cnt$Number / sum(cnt$Number)))
    
    rownames(cnt) <- NULL
    
    cnt
    
  })
  
  output$overview_others_pie <- renderPlot({
    to_filter_overview_others <- input$input_overview_others
    print(to_filter_overview_others)
    to_filter_overview_others_2 <- input$input_overview_others_2
    print(to_filter_overview_others_2)
    
    if (to_filter_overview_others == "ALL" &
        to_filter_overview_others_2 == "ALL") {
      others_overview <- final_others
    }
    else {
      if (to_filter_overview_others == "ALL") {
        others_overview <-
          filter(final_others,
                 BusinessType == to_filter_overview_others_2)
      }
      else {
        if (to_filter_overview_others_2 == "ALL") {
          others_overview <-
            filter(final_others,
                   LocalAuthorityName == to_filter_overview_others)
        }
        else {
          others_overview <-
            filter(
              final_others,
              LocalAuthorityName == to_filter_overview_others &
                BusinessType == to_filter_overview_others_2
            )
        }
      }
    }
    others_overview$RatingValue <-
      as.factor(others_overview$RatingValue)
    
    cnt <-
      data.frame(
        RatingValue = levels(others_overview$RatingValue),
        Number = tapply(
          others_overview$BusinessName,
          INDEX = others_overview$RatingValue,
          FUN = length
        )
      )
    
    cnt <-
      data.frame(
        RatingValue = cnt$RatingValue,
        Number = cnt$Number,
        Percentage = percent(cnt$Number / sum(cnt$Number))
      )
    
    rownames(cnt) <- NULL
    
    ggplot(cnt, aes(x = "", y = Number, fill = RatingValue)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) + scale_fill_grey() + theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 14, face = "bold")
      ) + theme(axis.text.x = element_blank())
    
  })
  
  #------------------------------------------------------------------
  #Tab1:datalab
  #Scotland datatable
  output$Scot_table = DT::renderDataTable(DT::datatable({
    data <- final_Scot
    if (input$bustype != "All") {
      data <- data[data$BusinessType == input$bustype, ]
    }
    if (input$rate != "All") {
      data <- data[data$RatingValue == input$rate, ]
    }
    data
  }))
  
  #Others datatable
  output$Others_table = DT::renderDataTable(DT::datatable({
    data <- final_others
    if (input$Bustype != "All") {
      data <- data[data$BusinessType == input$Bustype, ]
    }
    if (input$Rate != "All") {
      data <- data[data$RatingValue == input$Rate, ]
    }
    data
  }))
  
  #----------------------------------------------------------------#
  
  #Map for Scotland
  observeEvent(input$LocalAuthorityScot, {
    updateSelectInput(
      session,
      'authorityScot',
      choices = c(
        "Select Local Authority",
        final_Scot %>%
          filter(LocalAuthorityName == input$authorityScot) %>%
          distinct(BusinessType)
      )
    )
  })
  
  observeEvent(input$county, {
    updateSelectInput(
      session,
      'bustypeScot',
      choices = c(
        "Select Business Type",
        final_Scot %>%
          filter(BusinessType == input$bustypeScot) %>%
          distinct(RatingValue)
      )
    )
  })
  
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  
  output$scot_map <- renderLeaflet({
    leaflet(final_Scot) %>%
      addProviderTiles(providers$OpenStreetMap.DE) %>%
      setView(lng = 1.1743,
              lat = 52.3555,
              zoom = 6)
    
  })
  
  
  filteredDataScot <- reactive({
    df <- final_Scot %>%
      filter(LocalAuthorityName == input$authorityScot) %>%
      filter(BusinessType == input$bustypeScot) %>%
      filter(RatingValue == input$valueScot)
    return (df)
    
  })
  
  
  observe({
    getColor <- function(data) {
      sapply(filteredDataScot()$RatingValue, function(RatingValue) {
        if (RatingValue == "Awaiting Inspection") {
          "red"
        }
        else if (RatingValue == "Improvement Required") {
          "red"
        }
        else if (RatingValue == "AwaitingPublication") {
          "grey"
        }
        else if (RatingValue == "Exempt") {
          "grey"
        }
        else if (RatingValue == "Pass and Eat Safe") {
          "green"
        }
        else if (RatingValue == "Pass") {
          "green"
        }
      })
    }
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(filteredDataScot())
    )
    
    ScotMap <-
      leafletProxy("scot_map", data = filteredDataScot()) %>%
      clearMarkers() %>%
      addAwesomeMarkers(
        filteredDataScot()$Longitude,
        filteredDataScot()$Latitude,
        icon = icons,
        popup = paste(
          "<b>",
          filteredDataScot()$BusinessName,
          "</b>",
          "<br>",
          "<b>",
          "Type:",
          "</b>",
          filteredDataScot()$BusinessType,
          "<br>",
          "<b>",
          "Food hygiene ratings:",
          "</b>",
          filteredDataScot()$RatingValue,
          "<br>",
          "<b>",
          "Postcode:",
          "</b>",
          filteredDataScot()$PostCode
        ) %>% lapply(htmltools::HTML)
      ) %>%
      flyToBounds(
        lng1 = max(filteredDataScot()$Longitude),
        lng2 = min(filteredDataScot()$Longitude),
        lat1 = max(filteredDataScot()$Latitude),
        lat2 = min(filteredDataScot()$Latitude)
      )
    
  })
  
  #----------------
  #Rest of UK Map
  
  observeEvent(input$LocalAuthority, {
    updateSelectInput(
      session,
      'authorityOthers',
      choices = c(
        "Select Local Authority",
        final_others %>%
          filter(LocalAuthorityName == input$authorityOthers) %>%
          distinct(BusinessType)
      )
    )
  })
  observeEvent(input$county, {
    updateSelectInput(
      session,
      'bustypeOthers',
      choices = c(
        "Select Business Type",
        final_others %>%
          filter(BusinessType == input$bustypeOthers) %>%
          distinct(RatingValue)
      )
    )
  })
  
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  output$other_map <- renderLeaflet({
    leaflet(final_others) %>%
      addProviderTiles(providers$OpenStreetMap.DE) %>%
      setView(lng = 1.1743,
              lat = 52.3555,
              zoom = 6)
    
  })
  filteredData <- reactive({
    df <- final_others %>%
      filter(LocalAuthorityName == input$authorityOthers) %>%
      filter(BusinessType == input$bustypeOthers) %>%
      filter(RatingValue == input$valueOthers)
    return (df)
    
  })
  
  observe({
    getColor <- function(data) {
      sapply(filteredData()$RatingValue, function(RatingValue) {
        if (RatingValue == "Awaiting Inspection") {
          "red"
        }
        else if (RatingValue == "0" || RatingValue == "1") {
          "red"
        }
        else if (RatingValue == "AwaitingPublication") {
          "grey"
        }
        else if (RatingValue == "Exempt") {
          "grey"
        }
        else if (RatingValue == "2" || RatingValue == "3") {
          "green"
        }
        else if (RatingValue == "4" || RatingValue == "5") {
          "green"
        }
      })
    }
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(filteredData())
    )
    
    OtherMap <- leafletProxy("other_map", data = filteredData()) %>%
      clearMarkers() %>%
      addAwesomeMarkers(
        filteredData()$Longitude,
        filteredData()$Latitude,
        icon = icons,
        popup = paste(
          "<b>",
          filteredData()$BusinessName,
          "</b>",
          "<br>",
          "<b>",
          "Type:",
          "</b>",
          filteredData()$BusinessType,
          "<br>",
          "<b>",
          "Food hygiene ratings:",
          "</b>",
          filteredData()$RatingValue,
          "<br>",
          "<b>",
          "Postcode:",
          "</b>",
          filteredData()$PostCode
        ) %>% lapply(htmltools::HTML)
      ) %>%
      flyToBounds(
        lng1 = max(filteredData()$Longitude),
        lng2 = min(filteredData()$Longitude),
        lat1 = max(filteredData()$Latitude),
        lat2 = min(filteredData()$Latitude)
      )
    
  })
  
  
}