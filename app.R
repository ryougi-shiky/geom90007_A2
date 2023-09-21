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
library(ggplot2)

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
    selected_year <- input$selected_year
    
    # Filter the data based on the clicked state and the selected year
    selected_data <- filter(data, `State/Area` == clicked_state & Year == selected_year)
    
    # Convert the Month column to numeric
    selected_data$Month <- as.numeric(selected_data$Month)
    
    # Render the line chart
    output$lineChart <- renderPlot({
      ggplot(selected_data, aes(x = Month)) +
        geom_line(aes(y = `Total Unemployment in State/Area`, color = "Unemployment")) +
        labs(y = "Population", color = "Legend") +
        theme_minimal() +
        scale_color_manual(values = c("Unemployment" = "red")) +
        scale_x_continuous(breaks = 1:12) +  # Ensure x-axis has breaks from 1 to 12
        ggtitle(paste0(clicked_state, " Unemployment Population in ", selected_year))  # Add title based on selected state and year
    })
    
    # Render the first pie chart
    output$pieChart1 <- renderPlot({
      # Calculate the values for the pie chart
      labor_force <- sum(selected_data$`Total Civilian Labor Force in State/Area`)
      non_institutional_population <- sum(selected_data$`Total Civilian Non-Institutional Population in State/Area`)
      
      # Create a data frame for the pie chart
      pie_data1 <- data.frame(
        category = c("Labor Force", "Others"),
        value = c(labor_force, non_institutional_population - labor_force)
      )
      
      # Plot the pie chart
      ggplot(pie_data1, aes(x = "", y = value, fill = category)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y") +
        labs(fill = "Population Status") +
        theme_minimal() +
        theme(
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank()
        ) +
        ggtitle(paste0("Proportion of Labor Force in ", clicked_state, " for ", selected_year))
    })
    
    # Render the second pie chart
    output$pieChart2 <- renderPlot({
      # Calculate the values for the pie chart
      labor_force <- sum(selected_data$`Total Civilian Labor Force in State/Area`)
      employed <- sum(selected_data$`Total Employment in State/Area`)
      unemployed <- labor_force - employed
      
      # Create a data frame for the pie chart
      pie_data2 <- data.frame(
        category = c("Employed", "Unemployed"),
        value = c(employed, unemployed)
      )
      
      # Plot the pie chart
      ggplot(pie_data2, aes(x = "", y = value, fill = category)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y") +
        labs(fill = "Employment Status") +
        theme_minimal() +
        theme(
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank()
        ) +
        ggtitle(paste0("Employment Status in ", clicked_state, " for ", selected_year))
    })
    
    showModal(modalDialog(
      title = paste0("Detailed Information for ", clicked_state),
      plotOutput("lineChart"),  # Line chart to show the state's unemployment rate for each month in selected year
      # Pie chart to show  Percent (%) of Labor Force Unemployed in State/Area and 
      # Total Employment in State/Area in Total Civilian Labor Force in State/Area
      plotOutput("pieChart1"),  # First pie chart
      plotOutput("pieChart2"),  # Second pie chart
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close")
      )
    ))
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)