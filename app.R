# Install and load necessary packages
# list_packages <- c("shiny", "ggplot2", "dplyr", "leaflet", "sf", "maps", "readr", "devtools", "rnaturalearth", "rnaturalearthdata", "ropensci/rnaturalearthhires")
# new_packages <- list_packages[!(list_packages %in% installed.packages()[,"Package"])]
# if(length(new_packages)) install.packages(new_packages)

# devtools::install_github("ropensci/rnaturalearthhires")

# update.packages(ask = FALSE)

# Libraries
# Libraries
library(shiny)
library(dplyr)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(readr)
library(shinyjs)
library(plotly)

# Read the dataset
data <- read_csv("data/Unemployment_in_America_Per_US_State.csv") %>%
  filter(Year >= 2010)


# Get the spatial data for US states
states_sf <- ne_states(country = "United States of America", returnclass = "sf")

# UI
ui <- fluidPage(
  # Extend shiny with custom JavaScript functions
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("
      body {
        margin: 5px;
        padding: 0;
      }
      .container-fluid {
        height: 100%;
        padding: 0;
      }
      .plotly.html-widget {
        margin-bottom: 30px;  # Adjust this value as needed
      }
      .plotly .g-gtitle {
        margin-bottom: 30px !important;  # Adjust this value as needed
      }
    ")),
    tags$script("
      function showDetails(stateName) {
        Shiny.setInputValue('clickedState', stateName);
      }
    ")
  ),
  titlePanel(h1("Unemployment Rate in US", align="center")),
  
  # Custom CSS for the transparent dropdown menu
  tags$style(type = "text/css", "
             #control-panel {
               border-radius: 10px;
             }
             "),
  
  leafletOutput("map", width = "100%", height = "92vh"),  # Set map width and height
  
  # Absolute panel for dropdown menu
  absolutePanel(id = "control-panel", top = 85, left = 70, width = 90, 
                selectInput("selected_year", "Select Year:", choices = rev(unique(data$Year)))
  )
)

# Server
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    # Filter data for the selected year
    data_selected_year <- data %>%
      filter(Year == input$selected_year)
    
    # Join the unemployment data with the spatial data
    map_data <- left_join(states_sf, data_selected_year, by = c("name" = "State/Area"))
    
    leaflet(data = map_data) %>%
      setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~colorQuantile("YlOrRd", `Percent (%) of Labor Force Unemployed in State/Area`, n = 5)(`Percent (%) of Labor Force Unemployed in State/Area`),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 1,
          bringToFront = TRUE
        ),
        popup = ~paste0(
          "<strong>State:</strong> ", name, "<br>",
          "<strong>Unemployment Rate:</strong> ", round(`Percent (%) of Labor Force Unemployed in State/Area`, 2), "%<br>",
          "<button onclick='showDetails(\"", name, "\")'>Details</button>"
        )
      ) %>%
      addLegend(pal = colorQuantile("YlOrRd", map_data$`Percent (%) of Labor Force Unemployed in State/Area`, n = 5), 
                values = ~`Percent (%) of Labor Force Unemployed in State/Area`, 
                title = "Unemployment Rate",
                position = "bottomright")
  })
  
  # Observe the button click and display the modal dialog
  observeEvent(input$clickedState, {
    clicked_state <- input$clickedState
    selected_year <- input$selected_year
    
    # Filter the data based on the clicked state and the selected year
    selected_data <- filter(data, `State/Area` == clicked_state & Year == selected_year)
    
    # Convert the Month column to numeric
    selected_data$Month <- as.numeric(selected_data$Month)
    
    # Render the line chart using plotly
    output$lineChart <- renderPlotly({
      plot_ly(selected_data, x = ~Month, y = ~`Total Unemployment in State/Area`, type = 'scatter', mode = 'lines', color = ~"Unemployment", colors = c("red")) %>%
        layout(title = paste0(clicked_state, " Unemployment Population in ", selected_year),
               margin = list(t = 80, b = 20, l = 60, r = 10))
    })
    
    
    # Render the first pie chart using plotly
    output$pieChart1 <- renderPlotly({
      avg_labor_force <- round(mean(selected_data$`Total Civilian Labor Force in State/Area`))
      avg_non_institutional_population <- round(mean(selected_data$`Total Civilian Non-Institutional Population in State/Area`))
      pie_data1 <- data.frame(
        category = c("Labor Force", "Others"),
        value = c(avg_labor_force, avg_non_institutional_population - avg_labor_force)
      )
      plot_ly(pie_data1, labels = ~category, values = ~value, type = 'pie') %>%
        layout(title = paste0("Average Proportion of Labor Force in ", clicked_state, " for ", selected_year),
               margin = list(t = 80, b = 20, l = 60, r = 10))
    })
    
    # Render the second pie chart using plotly
    output$pieChart2 <- renderPlotly({
      avg_labor_force <- round(mean(selected_data$`Total Civilian Labor Force in State/Area`))
      avg_employed <- round(mean(selected_data$`Total Employment in State/Area`))
      avg_unemployed <- avg_labor_force - avg_employed
      pie_data2 <- data.frame(
        category = c("Employed", "Unemployed"),
        value = c(avg_employed, avg_unemployed)
      )
      plot_ly(pie_data2, labels = ~category, values = ~value, type = 'pie') %>%
        layout(title = paste0("Average Employment Status in ", clicked_state, " for ", selected_year),
               margin = list(t = 80, b = 20, l = 60, r = 10))
    })
    
    
    showModal(modalDialog(
      title = paste0("Detailed Information for ", clicked_state),
      plotlyOutput("lineChart"),
      plotlyOutput("pieChart1"),
      plotlyOutput("pieChart2"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close")
      )
    ))
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)