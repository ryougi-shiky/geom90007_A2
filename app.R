library(shiny) # Shiny app
library(dplyr) # Manipulate data
library(leaflet) # Draw map
library(sf) # spatial data structure
library(spData) # Fetch spatial data
library(readr) # Read data set
library(shinyjs) # Use Javascript functions
library(plotly) # Render interactive plots

# Read the data set
data <- read_csv("data/Unemployment_in_America_Per_US_State.csv")

# Get the spatial data for US states
states_sf <- us_states
# Check columns names
print(colnames(states_sf))
# Transform the CRS of states_sf to WGS84
states_sf <- st_transform(states_sf, 4326)

# UI
ui <- fluidPage(
  # Use custom JavaScript functions in shiny
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(
      HTML(
        "
      /* Adjust map size and margin */
      body { 
        margin: 5px;
        padding: 0;
      }
      .container-fluid {
        height: 100%;
        padding: 0;
      }
      /* Adjust charts margin */
      .plotly.html-widget { 
        margin-bottom: 30px;
      }
      .plotly .g-gtitle {
        margin-bottom: 30px !important;
      }
      /* Adjust map title position */
      .map-title { 
        transform: translateX(-50%);
        right: 50% !important;
      }
      /* About button background color */
      #aboutBtn { 
            background-color: rgba(255, 255, 255, 0.7);
      }
      /* Modal background color */
      .modal-backdrop { 
            opacity: 0.7 !important;
        }
      .modal-content {
          background-color: rgba(255, 255, 255, 0.8) !important;
      }
    "
      )
    ),
    tags$script(
      "
      /* Click area to set state name */
      function showDetails(stateName) { 
        Shiny.setInputValue('clickedState', stateName);
      }
    "
    )
  ),
  
  # CSS for the transparent drop down menu
  tags$style(type = "text/css", "
             #control-panel {
               border-radius: 10px;
             }
             "),
  # Set the app's background to light grey
  tags$head(tags$style(
    HTML("
    body {
      background-color: #EBEBEB;
    }
  ")
  )),
  
  # Set map width and height
  leafletOutput(
    outputId = "map",
    width = "100%",
    height = "98.5vh"
  ),
  
  # Absolute panel for drop down menu
  absolutePanel(
    id = "control-panel",
    top = 15,
    left = 70,
    width = 80,
    selectInput("selected_year", "Select Year:", choices = rev(unique(data$Year)))
  ),
  
  # About button
  actionButton("aboutBtn", "About", style = "position: absolute; bottom: 13px; left: 10px;")
)

# Server
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    # Filter data for the selected year
    data_selected_year <- data %>%
      filter(Year == input$selected_year)
    
    # Join the unemployment data with the spatial data
    map_data <-
      left_join(states_sf, data_selected_year, by = c("NAME" = "State/Area"))
    
    # Customise the colorQuantile function for reusing
    color_scale <- colorQuantile("Blues", map_data$`Percent (%) of Labor Force Unemployed in State/Area`, n = 5)
    
    leaflet(data = map_data) %>%
      # Set map start view position and zoom status
      setView(lng = -98.583,
              lat = 39.833,
              zoom = 4) %>%
      # Set map color theme
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~color_scale(`Percent (%) of Labor Force Unemployed in State/Area`),
        
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
        # Click area, pop up the box to show state name and its Unemployment Rate
        popup = ~ paste0(
          "<strong>State:</strong> ",
          NAME,
          "<br>",
          "<strong>Unemployment Rate:</strong> ",
          `Percent (%) of Labor Force Unemployed in State/Area`,
          "%<br>",
          "<button onclick='showDetails(\"",
          NAME,
          "\")'>Details</button>"
        )
      ) %>%
    
      # Add map title
      addControl(
        html = tags$div(style = "background-color: rgba(255, 255, 255, 0.6); padding: 5px; border-radius: 5px;
                        font-size: 24px;", "Unemployment Status in US"),
        position = "topright",
        className = "map-title"
      )
  })
  
  # Click About button, pop out About window
  observeEvent(input$aboutBtn, {
    showModal(aboutModal())
  })
  
  # About window prints the description of this app and acknowledge from author
  aboutModal <- function() {
    modalDialog(
      title = "About this App",
      tagList(
        tags$h4("GEOM90007 Assignment 2: Interactive Data Visualisation in R"),
        tags$p(
          "This Shiny app visualizes unemployment data in the US from 1976 to 2022."
        ),
        tags$hr(),
        tags$h4("How to use this app?"),
        tags$p("Select year to see the data in specified year."),
        tags$p(
          "Click the state area on the map to see the unemployment rate in selected year."
        ),
        tags$p(
          "Click 'Details' to see the detailed unemployment information about this state."
        ),
        tags$h4("What do these charts mean?"),
        tags$p(
          "The 1st line chart shows the monthly unemployment population for the selected state and year."
        ),
        tags$p(
          "The 1st pie chart provides the proportion of the labor force for the selected state and year."
        ),
        tags$p(
          "The 2nd pie chart presents the employment and unemployment proportion for the selected state and year."
        ),
        tags$p(
          "The 2nd line chart illustrates the overall change of average unemployment rates over the years."
        ),
        tags$hr(),
        tags$h4("Developed By"),
        tags$p("Author: Hongda Zhu"),
        tags$p("Institution: The University of Melbourne"),
        tags$p("Student ID: 1259524"),
        tags$p(
          "Data Reference: https://www.kaggle.com/datasets/justin2028/unemployment-in-america-per-us-state"
        ),
      ),
      easyClose = TRUE,
      footer = tagList(modalButton("Close"))
    )
  }
  
  
  # click detail button and pop out the modal
  observeEvent(input$clickedState, {
    clicked_state <- input$clickedState
    selected_year <- input$selected_year
    
    # Filter the data based on the clicked state and the selected year
    selected_data <-
      filter(data, `State/Area` == clicked_state & Year == selected_year)
    
    # Convert the Month column to numeric
    selected_data$Month <- as.numeric(selected_data$Month)
    
    # Render the first line chart 
    # Display the monthly unemployment population during selected year
    output$lineChart <- renderPlotly({
      plot_ly(
        selected_data,
        x = ~ Month,
        y = ~ `Total Unemployment in State/Area`,
        type = 'scatter',
        mode = 'lines',
        color = ~ "Unemployment",
        colors = c("steelblue")
      ) %>%
        layout(
          title = paste0(
            clicked_state,
            " Unemployment Population in ",
            selected_year
          ),
          margin = list(
            t = 80,
            b = 20,
            l = 60,
            r = 10
          )
        )
    })
    
    # Render the first pie chart
    # Show the Labor Force population in whole population
    output$pieChart1 <- renderPlotly({
      # Get Total Civilian Labor Force
      avg_labor_force <-
        round(mean(selected_data$`Total Civilian Labor Force in State/Area`))
      # Get Total Civilian Non-Institutional Population
      avg_non_institutional_population <-
        round(
          mean(
            selected_data$`Total Civilian Non-Institutional Population in State/Area`
          )
        )
      # Prepare the data used in pie chart
      pie_data1 <- data.frame(
        category = c("Labor Force", "Others"),
        value = c(
          avg_labor_force,
          avg_non_institutional_population - avg_labor_force
        )
      )
      plot_ly(
        pie_data1,
        labels = ~ category,
        values = ~ value,
        type = 'pie',
        marker = list(colors = c("lightblue", "deepskyblue"))
      ) %>%
        layout(
          title = paste0(
            "Average Proportion of Labor Force in ",
            clicked_state,
            " for ",
            selected_year
          ),
          margin = list(
            t = 80,
            b = 20,
            l = 60,
            r = 10
          )
        )
    })
    
    # Render the second pie chart
    output$pieChart2 <- renderPlotly({
      # Labor Force population = employed + unemployed
      # Calculate the population of employed and unemployed
      avg_labor_force <-
        round(mean(selected_data$`Total Civilian Labor Force in State/Area`))
      avg_employed <-
        round(mean(selected_data$`Total Employment in State/Area`))
      avg_unemployed <- avg_labor_force - avg_employed
      # Draw pie chart for employed and unemployed
      pie_data2 <- data.frame(
        category = c("Employed", "Unemployed"),
        value = c(avg_employed, avg_unemployed)
      )
      plot_ly(
        pie_data2,
        labels = ~ category,
        values = ~ value,
        type = 'pie',
        marker = list(colors = c("cornflowerblue", "midnightblue"))
      ) %>%
        layout(
          title = paste0(
            "Average Employment Status in ",
            clicked_state,
            " for ",
            selected_year
          ),
          margin = list(
            t = 80,
            b = 20,
            l = 60,
            r = 10
          )
        )
    })
    
    # Calculate the average unemployment rate for each year for the clicked state
    year_unemployment_rate <- data %>%
      filter(`State/Area` == clicked_state) %>%
      group_by(Year) %>%
      summarize(Avg_Unemployment_Rate = round(
        mean(`Percent (%) of Labor Force Unemployed in State/Area`, na.rm = TRUE),
        2
      ))
    
    # Render the line chart about overall change of average unemployment rate in all years
    output$overallUnemploymentRateChart <- renderPlotly({
      plot_ly(
        year_unemployment_rate,
        x = ~ Year,
        y = ~ Avg_Unemployment_Rate,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = "#1E90FF")
      ) %>%
        layout(
          title = paste0(clicked_state, " Overall Unemployment Rate"),
          xaxis = list(title = "Year"),
          yaxis = list(title = "Unemployment Rate (%)"),
          margin = list(
            t = 80,
            b = 20,
            l = 60,
            r = 10
          )
        )
    })
    
    # The detail modal shows 4 charts we implemented above
    showModal(
      modalDialog(
        title = paste0("Detailed Information for ", clicked_state),
        plotlyOutput("lineChart"),
        plotlyOutput("pieChart1"),
        plotlyOutput("pieChart2"),
        plotlyOutput("overallUnemploymentRateChart"),
        easyClose = TRUE,
        footer = tagList(modalButton("Close"))
      )
    )
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)