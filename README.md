# geom90007_A2

Objective:

The primary goal of this application is to provide an interactive interface for users to explore and understand the unemployment trends in the US from 1976 to 2022.

Features:

- The main part of the app is the interactive geographical map for the US that can help users focus on exploring the unemployment rate in different states. The interactive geographical map is powered by the Leaflet package which is an easier way to build a map by spatial data.
- At the top left of the map, there is a drop-down menu where users can select the year. Then the map can present the data for the selected year. The title of the map is shown at the top center.
- Click the state area block, pop up a text box that displays the state name, the unemployment rate in the selected year, and a “Details" button to show more details.
- Click this “Details" button, and pop up a window that contains 4 charts. Plotly package is used to plot these interactive charts. When the mouse hovers over the chart, pop up a label to show the details information about the selected data. For example, displaying the specific unemployment population, rate, selected year, month, etc.
- The 1st line chart shows the monthly unemployment population for the selected state and year. Users can see the change in the unemployment population this year.
- The 1st pie chart provides the proportion of the labor force for the selected state and year. The average population in the selected year is used to plot the chart.
- The 2nd pie chart presents the proportion of employment and unemployment population for the selected state and year. The average population in the selected year is used to plot the chart.
- The 2nd line chart illustrates the overall unemployment rate change of average unemployment rates over the years.
- At the bottom left of the map, there is an “About” button. Click the button, and pop up a window showing a brief introduction of this app, explanations of the charts, developer information, and data source reference.
- The app’s color theme is blue, which is used to highlight valuable information in the whole map and all charts. Blue can suitably fit into the grey and white background color.
- The pop-up windows, “About” button, and title have transparent backgrounds that make the interface more aesthetically appealing and more cohesive to the map.


Data Sources:

1. Bureau of Labor Statistics's Official Website: The primary source of the unemployment data. The Bureau has been publishing monthly updates on unemployment rates since January 1976.
2. Bureau of Labor Statistics's Economic News Release on (Monthly) State Employment and Unemployment: This source provides monthly updates on unemployment rates.
3. Bureau of Labor Statistics's State Employment and Unemployment Technical Note: This guide offers essential contextual knowledge needed to understand the dataset, including the methodology behind the data collection and definitions of the variables tracked.
4. Link: https://www.kaggle.com/datasets/justin2028/unemployment-in-america-per-us-state


Dataset Structure:

- FIPS Code of State/Area
- State/Area
- Year
- Month
- Total Civilian Non-Institutional Population in State/Area
- Total Civilian Labor Force in State/Area
- Percent (%) of State/Area's Population
- Total Employment in State/Area
- Percent (%) of Labor Force Employed in State/Area
- Total Unemployment in State/Area
- Percent (%) of Labor Force Unemployed in State/Area
