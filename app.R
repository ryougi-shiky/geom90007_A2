# Install and load necessary packages
# list_packages <- c("shiny", "ggplot2", "dplyr", "leaflet", "sf", "maps", "readr", "devtools", "rnaturalearth", "rnaturalearthdata", "ropensci/rnaturalearthhires")
# new_packages <- list_packages[!(list_packages %in% installed.packages()[,"Package"])]
# if(length(new_packages)) install.packages(new_packages)

# devtools::install_github("ropensci/rnaturalearthhires")

# update.packages(ask = FALSE)

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

# Read the dataset
data <- read_csv("data/Unemployment_in_America_Per_US_State.csv")

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
    selected_data <- filter(data, `State/Area` == clicked_state)
    
    details_text <- paste0(
      "<strong>Non-Institutional Population:</strong> ", selected_data$`Total Civilian Non-Institutional Population in State/Area`, "<br>",
      "<strong>Labor Force:</strong> ", selected_data$`Total Civilian Labor Force in State/Area`, "<br>",
      "<strong>Population Percentage:</strong> ", selected_data$`Percent (%) of State/Area's Population`, "<br>",
      "<strong>Employment:</strong> ", selected_data$`Total Employment in State/Area`, "<br>",
      "<strong>Employment Percentage:</strong> ", selected_data$`Percent (%) of Labor Force Employed in State/Area`, "<br>",
      "<strong>Unemployment:</strong> ", selected_data$`Total Unemployment in State/Area`, "<br>"
    )
    
    showModal(modalDialog(
      title = "Detailed Information",
      HTML(details_text),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close")
      )
    ))
  })
}


# Run the Shiny app
shinyApp(ui = ui, server = server)