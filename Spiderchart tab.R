################ Version 0 #################
{
# 
# # Read the dataset from GitHub
# reshaped_df <- readr::read_csv("https://raw.githubusercontent.com/Marcob88/Sustrack/main/datasetmerged.csv", locale = locale(encoding = "UTF-8"))
# 
# # Function to generate indicator datasets
# generate_indicator_datasets <- function(year) {
#   # Filter data by the selected year
#   filtered_data <- reshaped_df %>% filter(Year == year)
#   
#   # List of indicators
#   indicators <- c("Agricultural factor income per annual work unit (AWU)", 
#                   "Prevalence of moderate or severe food insecurity in the total population (percentage)", 
#                   "Food purchasing power (food and non-alcoholic beverages)")
#   
#   # Create datasets for each indicator
#   indicator_datasets <- map(indicators, ~{
#     indicator_name <- .x
#     dataset <- filtered_data %>%
#       select(Year, Country, !!sym(indicator_name)) %>%  # Select Year, Country, and the specific indicator
#       drop_na()  # Remove NA values
#     return(dataset)
#   })
#   
#   return(indicator_datasets)
# }
# 
# # Function to calculate deciles for each indicator dataset
# calculate_deciles <- function(data) {
#   data %>% 
#     mutate(decile = ntile(!!sym(names(data)[3]), 10))  # Calculate deciles for the third column (indicator)
# }
# 
# # Function to plot spider chart for a specific country
# plot_spider_chart <- function(country, year) {
#   # Generate indicator datasets for the selected year
#   indicator_datasets <- generate_indicator_datasets(year)
#   
#   # Calculate deciles for each indicator dataset
#   indicator_deciles <- map(indicator_datasets, calculate_deciles)
#   
#   # Select decile values for the specified country
#   country_deciles <- data.frame(
#     Category = c("Agricultural factor income", "Food insecurity", "Food purchasing power"),
#     Decile = c(
#       indicator_deciles[[1]]$decile[indicator_deciles[[1]]$Country == country],
#       indicator_deciles[[2]]$decile[indicator_deciles[[2]]$Country == country],
#       indicator_deciles[[3]]$decile[indicator_deciles[[3]]$Country == country]
#     )
#   )
#   
#   # Plot spider chart
#   spider_chart <- plot_ly(country_deciles, 
#                           type = 'scatterpolar', 
#                           fill = 'toself', 
#                           mode = 'lines+markers',
#                           marker = list(symbol = 'circle', size = 8)) %>%
#     add_trace(r = country_deciles$Decile, 
#               theta = country_deciles$Category,
#               name = country) %>%
#     layout(title = paste("Spider Chart for", country, "-", year),
#            polar = list(radialaxis = list(visible = TRUE, range = c(1, 10))),
#            showlegend = TRUE)
#   
#   return(spider_chart)
# }
# 
# # Plot spider chart for a specific country and year (e.g., Italy, 2019)
# spider_chart_Italy_2019 <- plot_spider_chart("Spain", 2015)
# 
# # Print the spider chart
# print(spider_chart_Italy_2019)
# 
# 
# 
# ################### SHINY APP ###############
# library(shiny)
# library(plotly)
# library(dplyr)
# library(purrr)
# 
# # Read the dataset from GitHub
# reshaped_df <- readr::read_csv("https://raw.githubusercontent.com/Marcob88/Sustrack/main/datasetmerged.csv", locale = locale(encoding = "UTF-8"))
# 
# # UI
# ui <- fluidPage(
#   titlePanel("Spider Chart for Country and Year"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("country", "Select Country", choices = unique(reshaped_df$Country)),
#       selectInput("year", "Select Year", choices = unique(reshaped_df$Year)),
#       actionButton("update", "Update")
#     ),
#     mainPanel(
#       plotlyOutput("spider_chart")
#     )
#   )
# )
# 
# # Server
# server <- function(input, output, session) {
#   
#   # Function to generate indicator datasets
#   generate_indicator_datasets <- function(year) {
#     # Filter data by the selected year
#     filtered_data <- reshaped_df %>% filter(Year == year)
#     
#     # List of indicators
#     indicators <- c("Agricultural factor income per annual work unit (AWU)", 
#                     "Prevalence of moderate or severe food insecurity in the total population (percentage)", 
#                     "Food purchasing power (food and non-alcoholic beverages)")
#     
#     # Create datasets for each indicator
#     indicator_datasets <- map(indicators, ~{
#       indicator_name <- .x
#       dataset <- filtered_data %>%
#         select(Year, Country, !!sym(indicator_name)) %>%  # Select Year, Country, and the specific indicator
#         drop_na()  # Remove NA values
#       return(dataset)
#     })
#     
#     return(indicator_datasets)
#   }
#   
#   # Function to calculate deciles for each indicator dataset
#   calculate_deciles <- function(data) {
#     data %>% 
#       mutate(decile = ntile(!!sym(names(data)[3]), 10))  # Calculate deciles for the third column (indicator)
#   }
#   
#   # Function to plot spider chart for a specific country and year
#   plot_spider_chart <- function(country, year) {
#     # Generate indicator datasets for the selected year
#     indicator_datasets <- generate_indicator_datasets(year)
#     
#     # Calculate deciles for each indicator dataset
#     indicator_deciles <- map(indicator_datasets, calculate_deciles)
#     
#     # Select decile values for the specified country
#     country_deciles <- data.frame(
#       Category = c("Agricultural factor income", "Food insecurity", "Food purchasing power"),
#       Decile = c(
#         indicator_deciles[[1]]$decile[indicator_deciles[[1]]$Country == country],
#         indicator_deciles[[2]]$decile[indicator_deciles[[2]]$Country == country],
#         indicator_deciles[[3]]$decile[indicator_deciles[[3]]$Country == country]
#       )
#     )
#     
#     # Plot spider chart
#     spider_chart <- plot_ly(country_deciles, 
#                             type = 'scatterpolar', 
#                             fill = 'toself', 
#                             mode = 'lines+markers',
#                             marker = list(symbol = 'circle', size = 8)) %>%
#       add_trace(r = country_deciles$Decile, 
#                 theta = country_deciles$Category,
#                 name = country) %>%
#       layout(title = paste("Spider Chart for", country, "-", year),
#              polar = list(radialaxis = list(visible = TRUE, range = c(1, 10))),
#              showlegend = TRUE)
#     
#     return(spider_chart)
#   }
#   
#   # Reactive function to update spider chart
#   observeEvent(input$update, {
#     output$spider_chart <- renderPlotly({
#       plot_spider_chart(input$country, input$year)
#     })
#   })
# }
# 
# # Run the application
# shinyApp(ui = ui, server = server)
# 
# ############################################# ###############################
# library(shiny)
# library(plotly)
# library(dplyr)
# library(purrr)
# 
# # Read the dataset from GitHub
# library(shiny)
# library(plotly)
# library(dplyr)
# library(purrr)
# 
# library(shiny)
# library(plotly)
# library(dplyr)
# library(purrr)
# 
# # Read the dataset from GitHub
# reshaped_df <- readr::read_csv("https://raw.githubusercontent.com/Marcob88/Sustrack/main/datasetmerged.csv", locale = locale(encoding = "UTF-8"))
# 
# # UI
# ui <- fluidPage(
#   titlePanel("Spider Charts for Country and Year"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("country", "Select Country", choices = unique(reshaped_df$Country)),
#       selectInput("year", "Select Year", choices = unique(reshaped_df$Year)),
#       actionButton("update", "Update")
#     ),
#     mainPanel(
#       plotlyOutput("spider_chart_1"),
#       plotlyOutput("spider_chart_2"),
#       plotlyOutput("spider_chart_3"),
#       plotlyOutput("spider_chart_4"),
#       plotlyOutput("spider_chart_5")
#     )
#   )
# )
# 
# # Server
# server <- function(input, output, session) {
#   
#   # Function to generate indicator datasets
#   generate_indicator_datasets <- function(year, indicators) {
#     # Filter data by the selected year
#     filtered_data <- reshaped_df %>% filter(Year == year)
#     
#     # Create datasets for each indicator
#     indicator_datasets <- map(indicators, ~{
#       indicator_name <- .x
#       dataset <- filtered_data %>%
#         select(Year, Country, !!sym(indicator_name)) %>%  # Select Year, Country, and the specific indicator
#         drop_na()  # Remove NA values
#       return(dataset)
#     })
#     
#     return(indicator_datasets)
#   }
#   
#   # Function to calculate deciles for each indicator dataset
#   calculate_deciles <- function(data) {
#     data %>% 
#       mutate(decile = ntile(!!sym(names(data)[3]), 10))  # Calculate deciles for the third column (indicator)
#   }
#   
#   # Function to plot spider chart for a specific country and year
#   plot_spider_chart <- function(country, year, indicators, chart_title) {
#     # Generate indicator datasets for the selected year
#     indicator_datasets <- generate_indicator_datasets(year, indicators)
#     
#     # Calculate deciles for each indicator dataset
#     indicator_deciles <- map(indicator_datasets, calculate_deciles)
#     
#     # Select decile values for the specified country
#     country_deciles <- data.frame(
#       Category = indicators,
#       Decile = sapply(1:length(indicators), function(i) {
#         indicator_deciles[[i]]$decile[indicator_deciles[[i]]$Country == country]
#       })
#     )
#     
#     # Plot spider chart
#     spider_chart <- plot_ly(country_deciles, 
#                             type = 'scatterpolar', 
#                             fill = 'toself', 
#                             mode = 'lines+markers',
#                             marker = list(symbol = 'circle', size = 8)) %>%
#       add_trace(r = country_deciles$Decile, 
#                 theta = country_deciles$Category,
#                 name = country) %>%
#       layout(title = paste(chart_title, country, "-", year),
#              polar = list(radialaxis = list(visible = TRUE, range = c(1, 10), tickvals = 1:10, ticksuffix = ""),
#                           angularaxis = list(tickvals = 1:length(indicators), ticktext = indicators, direction = 'clockwise')),
#              showlegend = TRUE)
#     
#     return(spider_chart)
#   }
#   
#   # Reactive function to update spider charts
#   observeEvent(input$update, {
#     output$spider_chart_1 <- renderPlotly({
#       indicators_1 <- c("Agricultural factor income per annual work unit (AWU)", 
#                         "Prevalence of moderate or severe food insecurity in the total population (percentage)", 
#                         "Food purchasing power (food and non-alcoholic beverages)")
#       plot_spider_chart(input$country, input$year, indicators_1, "Spider Chart 1 for")
#     })
#     
#     output$spider_chart_2 <- renderPlotly({
#       indicators_2 <- c("Share of organic farming in utilised agricultural area (percentage)", 
#                         "Circular material rate (percentage)", 
#                         "Recycling rate of municipal waste", 
#                         "Food services consumption (Food waste) (tonnes)")
#       plot_spider_chart(input$country, input$year, indicators_2, "Spider Chart 2 for")
#     })
#     
#     output$spider_chart_3 <- renderPlotly({
#       indicators_3 <- c("Domestic Material Consumption (biomass share)", 
#                         "Share of renewable energy in gross final energy consumption", 
#                         "Share of renewables for heating & cooling (percentage)", 
#                         "Share of renewables for electricity (percentage)", 
#                         "Share of renewables for transport (percentage)")
#       plot_spider_chart(input$country, input$year, indicators_3, "Spider Chart 3 for")
#     })
#     
#     output$spider_chart_4 <- renderPlotly({
#       indicators_4 <- c("Material footprint (Biomass)(kg per dollar of GDP in USD)", 
#                         "net GHG emissions (emissions and removals) from agriculture_CRF3", 
#                         "net GHG emissions (emissions and removals) from LULUCF_CRF4", 
#                         "Water exploitation index (WEI)_PC")
#       plot_spider_chart(input$country, input$year, indicators_4, "Spider Chart 4 for")
#     })
#     
#     output$spider_chart_5 <- renderPlotly({
#       indicators_5 <- c("Government support to agricultural research and development (by sector) euro per capita", 
#                         "Energy productivity (EUR per KG of oil equivalent)", 
#                         "Share of renewables for transport, electricity and heating & cooling (percent)", 
#                         "Persons employed per bioeconomy sectors_total", 
#                         "Turnover in bioeconomy per sector_total")
#       plot_spider_chart(input$country, input$year, indicators_5, "Spider Chart 5 for")
#     })
#   })
# }
# 
# # Run the application
# shinyApp(ui = ui, server = server)
}

############### Version 1 ##################
{
library(shiny)
library(plotly)
library(dplyr)
library(purrr)
library(tidyr)

# Read the dataset from GitHub
reshaped_df <- readr::read_csv("https://raw.githubusercontent.com/Marcob88/Sustrack/main/datasetmerged.csv", locale = locale(encoding = "UTF-8"))

# Function to generate indicator datasets
generate_indicator_datasets <- function(years, indicators) {
  # Filter data by the selected years
  filtered_data <- reshaped_df %>% filter(Year %in% years)
  
  # Create datasets for each indicator
  indicator_datasets <- map(indicators, ~{
    indicator_name <- .x
    dataset <- filtered_data %>%
      select(Year, Country, !!sym(indicator_name)) %>%  # Select Year, Country, and the specific indicator
      drop_na()  # Remove NA values
    return(dataset)
  })
  
  return(indicator_datasets)
}

# Function to calculate deciles for each indicator dataset
calculate_deciles <- function(data) {
  data %>% 
    group_by(Year) %>% 
    mutate(decile = ntile(!!sym(names(data)[3]), 10)) %>%  # Calculate deciles for the third column (indicator)
    ungroup()
}

# Function to plot spider chart for specific countries and years
plot_spider_chart <- function(countries, years, indicators, chart_title) {
  # Generate indicator datasets for the selected years
  indicator_datasets <- generate_indicator_datasets(years, indicators)
  
  # Calculate deciles for each indicator dataset
  indicator_deciles <- map(indicator_datasets, calculate_deciles)
  
  # Initialize spider chart
  spider_chart <- plot_ly(type = 'scatterpolar', fill = 'toself', mode = 'lines+markers', marker = list(symbol = 'circle', size = 8))
  
  # Loop through each country and year to add traces to the chart
  for (country in countries) {
    for (year in years) {
      country_deciles <- data.frame(
        Category = indicators,
        Decile = sapply(1:length(indicators), function(i) {
          indicator_deciles[[i]]$decile[indicator_deciles[[i]]$Country == country & indicator_deciles[[i]]$Year == year]
        }),
        Year = year,
        Country = country
      )
      
      spider_chart <- spider_chart %>%
        add_trace(r = country_deciles$Decile, 
                  theta = country_deciles$Category,
                  name = paste(country, year))
    }
  }
  
  spider_chart <- spider_chart %>%
    layout(title = chart_title,
           polar = list(radialaxis = list(visible = TRUE, range = c(1, 10), tickmode = 'array', tickvals = 1:10, ticktext = 1:10)),
           showlegend = TRUE)
  
  return(spider_chart)
}

# UI
ui <- fluidPage(
  titlePanel("Spider Charts for Countries and Years"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countries", "Select Countries", choices = unique(reshaped_df$Country), multiple = TRUE),
      selectInput("years", "Select Years", choices = unique(reshaped_df$Year), multiple = TRUE),
      actionButton("update", "Update")
    ),
    mainPanel(
      plotlyOutput("spider_chart_1"),
      plotlyOutput("spider_chart_2"),
      plotlyOutput("spider_chart_3"),
      plotlyOutput("spider_chart_4"),
      plotlyOutput("spider_chart_5")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive function to update spider charts
  observeEvent(input$update, {
    output$spider_chart_1 <- renderPlotly({
      indicators_1 <- c("Agricultural factor income per annual work unit (AWU)", 
                        "Prevalence of moderate or severe food insecurity in the total population (percentage)", 
                        "Food purchasing power (food and non-alcoholic beverages)")
      plot_spider_chart(input$countries, input$years, indicators_1, "Spider Chart 1")
    })
    
    output$spider_chart_2 <- renderPlotly({
      indicators_2 <- c("Share of organic farming in utilised agricultural area (percentage)", 
                        "Circular material rate (percentage)", 
                        "Recycling rate of municipal waste", 
                        "Food services consumption (Food waste) (tonnes)")
      plot_spider_chart(input$countries, input$years, indicators_2, "Spider Chart 2")
    })
    
    output$spider_chart_3 <- renderPlotly({
      indicators_3 <- c("Domestic Material Consumption (biomass share)", 
                        "Share of renewable energy in gross final energy consumption", 
                        "Share of renewables for heating & cooling (percentage)", 
                        "Share of renewables for electricity (percentage)", 
                        "Share of renewables for transport (percentage)")
      plot_spider_chart(input$countries, input$years, indicators_3, "Spider Chart 3")
    })
    
    output$spider_chart_4 <- renderPlotly({
      indicators_4 <- c("Material footprint (Biomass)(kg per dollar of GDP in USD)", 
                        "net GHG emissions (emissions and removals) from agriculture_CRF3", 
                        "net GHG emissions (emissions and removals) from LULUCF_CRF4", 
                        "Water exploitation index (WEI)_PC")
      plot_spider_chart(input$countries, input$years, indicators_4, "Spider Chart 4")
    })
    
    output$spider_chart_5 <- renderPlotly({
      indicators_5 <- c("Government support to agricultural research and development (by sector) euro per capita", 
                        "Energy productivity (EUR per KG of oil equivalent)", 
                        "Share of renewables for transport, electricity and heating & cooling (percent)", 
                        "Persons employed per bioeconomy sectors_total", 
                        "Turnover in bioeconomy per sector_total")
      plot_spider_chart(input$countries, input$years, indicators_5, "Spider Chart 5")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
}

############### Version 2 ##################
{
# # Load necessary libraries
# library(shiny)
# library(plotly)
# library(dplyr)
# library(purrr)
# library(tidyr)
# library(readr)
# library(shinyjs)
# library(fontawesome)
# 
# # Read the dataset from GitHub
# reshaped_df <- readr::read_csv("https://raw.githubusercontent.com/Marcob88/Sustrack/main/datasetmerged.csv", locale = locale(encoding = "UTF-8"))
# 
# # Function to generate indicator datasets
# generate_indicator_datasets <- function(years, indicators) {
#   # Filter data by the selected years
#   filtered_data <- reshaped_df %>% filter(Year %in% years)
#   
#   # Create datasets for each indicator
#   indicator_datasets <- map(indicators, ~{
#     indicator_name <- .x
#     dataset <- filtered_data %>%
#       select(Year, Country, !!sym(indicator_name)) %>%  # Select Year, Country, and the specific indicator
#       drop_na()  # Remove NA values
#     return(dataset)
#   })
#   
#   return(indicator_datasets)
# }
# 
# # Function to calculate deciles for each indicator dataset
# calculate_deciles <- function(data) {
#   data %>% 
#     group_by(Year) %>% 
#     mutate(decile = ntile(!!sym(names(data)[3]), 10)) %>%  # Calculate deciles for the third column (indicator)
#     ungroup()
# }
# 
# # Function to plot spider chart for specific countries and years
# plot_spider_chart <- function(countries, years, indicators) {
#   # Generate indicator datasets for the selected years
#   indicator_datasets <- generate_indicator_datasets(years, indicators)
#   
#   # Calculate deciles for each indicator dataset
#   indicator_deciles <- map(indicator_datasets, calculate_deciles)
#   
#   # Insert line breaks for long indicator names
#   indicators <- sapply(indicators, function(x) {
#     paste(strwrap(x, width = 20), collapse = "<br>")
#   })
#   
#   # Initialize spider chart
#   spider_chart <- plot_ly(type = 'scatterpolar', fill = 'toself', mode = 'lines+markers', marker = list(symbol = 'circle', size = 8))
#   
#   # Loop through each country and year to add traces to the chart
#   for (country in countries) {
#     for (year in years) {
#       country_deciles <- data.frame(
#         Category = indicators,
#         Decile = sapply(1:length(indicators), function(i) {
#           deciles <- indicator_deciles[[i]]$decile[indicator_deciles[[i]]$Country == country & indicator_deciles[[i]]$Year == year]
#           if (length(deciles) == 0) {
#             return(NA)
#           } else {
#             return(deciles)
#           }
#         }),
#         Year = year,
#         Country = country
#       )
#       
#       # Only add the trace if there is no missing data
#       if (!any(is.na(country_deciles$Decile))) {
#         spider_chart <- spider_chart %>%
#           add_trace(r = country_deciles$Decile, 
#                     theta = country_deciles$Category,
#                     name = paste(country, year))
#       }
#     }
#   }
#   
#   spider_chart <- spider_chart %>%
#     layout(
#       polar = list(radialaxis = list(visible = TRUE, range = c(1, 10), tickmode = 'array', tickvals = 1:10, ticktext = 1:10)),
#       showlegend = TRUE,
#       margin = list(l = 50, r = 50, b = 50, t = 50)  # Adjust margin to increase space around the chart
#     )
#   
#   return(spider_chart)
# }
# 
# # UI
# ui <- fluidPage(
#   titlePanel("Spider Charts for Countries and Years"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("countries", "Select Countries", choices = unique(reshaped_df$Country), multiple = TRUE, selected = "EU27(2020)"),
#       selectInput("years", "Select Years", choices = unique(reshaped_df$Year), multiple = TRUE, selected = 2019),
#       actionButton("update", "Update")
#     ),
#     mainPanel(
#       div(style = "margin-bottom: 60px;",
#           h3(tags$b("Social wellbeing")),
#           plotlyOutput("spider_chart_1")),
#       hr(),
#       div(style = "margin-bottom: 60px;",
#           h3(tags$b("Sustainable management of resources")),
#           plotlyOutput("spider_chart_2")),
#       hr(),
#       div(style = "margin-bottom: 60px;",
#           h3(tags$b("Reducing dependence on non-renewable, unsustainable resources")),
#           plotlyOutput("spider_chart_3")),
#       hr(),
#       div(style = "margin-bottom: 60px;margin-top: 30px;",
#           h3(tags$b("Mitigating and adapting to climate change")),
#           plotlyOutput("spider_chart_4")),
#       hr(),
#       div(style = "margin-bottom: 60px;",
#           h3(tags$b("Bioeconomy competitiveness")),
#           plotlyOutput("spider_chart_5"))
#     )
#   )
# )
# ########### alternative UI with fixed sidebar panel #######
# # # UI
# # ui <- fluidPage(
# #   tags$head(
# #     tags$style(
# #       HTML("
# #       #sidebar {
# #         position: fixed;
# #         top: 0;
# #         left: 0;
# #         bottom: 0;
# #         overflow-y: auto;
# #         padding: 20px;
# #         width: 250px;
# #       }
# # 
# #       #mainPanel {
# #         margin-left: 280px; /* Adjust this value based on the sidebar width */
# #         padding: 20px;
# #       }
# # 
# #       .spider-chart-section {
# #         margin-top: 40px;
# #       }
# # 
# #       .spider-chart-title {
# #         font-weight: bold;
# #         margin-top: 20px;
# #         margin-bottom: 20px;
# #       }
# # 
# #       .sidebar-title {
# #         font-weight: bold;
# #         margin-bottom: 10px;
# #       }
# #       ")
# #     )
# #   ),
# #   titlePanel("Spider Charts for Countries and Years"),
# #   sidebarLayout(
# #     sidebarPanel(
# #       id = "sidebar",
# #       h3("Countries and Year", class = "sidebar-title"),
# #       selectInput("countries", "Select Countries", choices = unique(reshaped_df$Country), multiple = TRUE, selected = "EU27(2020)"),
# #       selectInput("years", "Select Years", choices = unique(reshaped_df$Year), multiple = TRUE, selected = 2019),
# #       actionButton("update", "Update")
# #     ),
# #     mainPanel(
# #       id = "mainPanel",
# #       plotlyOutput("spider_chart_1"),
# #       div(class = "spider-chart-section",
# #           tags$h3("Social wellbeing", class = "spider-chart-title"),
# #           hr(),
# #           plotlyOutput("spider_chart_2")
# #       ),
# #       div(class = "spider-chart-section",
# #           tags$h3("Sustainable management of resources", class = "spider-chart-title"),
# #           hr(),
# #           plotlyOutput("spider_chart_3")
# #       ),
# #       div(class = "spider-chart-section",
# #           tags$h3("Reducing dependence on non-renewable, unsustainable resources", class = "spider-chart-title"),
# #           hr(),
# #           plotlyOutput("spider_chart_4")
# #       ),
# #       div(class = "spider-chart-section",
# #           tags$h3("Mitigating and adapting to climate change", class = "spider-chart-title"),
# #           hr(),
# #           plotlyOutput("spider_chart_5")
# #       ),
# #       div(class = "spider-chart-section",
# #           tags$h3("Bioeconomy competitiveness", class = "spider-chart-title")
# #           
# #       )
# #     )
# #   )
# # )
# 
# 
# 
# 
# # Server
# server <- function(input, output, session) {
#   
#   # Reactive function to update spider charts
#   update_charts <- function() {
#     output$spider_chart_1 <- renderPlotly({
#       indicators_1 <- c("Agricultural factor income per annual work unit (AWU)", 
#                         "Prevalence of moderate or severe food insecurity in the total population (percentage)", 
#                         "Food purchasing power (food and non-alcoholic beverages)")
#       plot_spider_chart(input$countries, input$years, indicators_1)
#     })
#     
#     output$spider_chart_2 <- renderPlotly({
#       indicators_2 <- c("Share of organic farming in utilised agricultural area (percentage)", 
#                         "Circular material rate (percentage)", 
#                         "Recycling rate of municipal waste", 
#                         "Food services consumption (Food waste) (tonnes)")
#       plot_spider_chart(input$countries, input$years, indicators_2)
#     })
#     
#     output$spider_chart_3 <- renderPlotly({
#       indicators_3 <- c("Domestic Material Consumption (biomass share)", 
#                         "Share of renewable energy in gross final energy consumption", 
#                         "Share of renewables for heating & cooling (percentage)", 
#                         "Share of renewables for electricity (percentage)", 
#                         "Share of renewables for transport (percentage)")
#       plot_spider_chart(input$countries, input$years, indicators_3)
#     })
#     
#     output$spider_chart_4 <- renderPlotly({
#       indicators_4 <- c("Material footprint (Biomass)(kg per dollar of GDP in USD)", 
#                         "net GHG emissions (emissions and removals) from agriculture_CRF3", 
#                         "net GHG emissions (emissions and removals) from LULUCF_CRF4", 
#                         "Water exploitation index (WEI)_PC")
#       plot_spider_chart(input$countries, input$years, indicators_4)
#     })
#     
#     output$spider_chart_5 <- renderPlotly({
#       indicators_5 <- c("Government support to agricultural research and development (by sector) euro per capita", 
#                         "Energy productivity (EUR per KG of oil equivalent)", 
#                         "Share of renewables for transport, electricity and heating & cooling (percent)", 
#                         "Persons employed per bioeconomy sectors_total", 
#                         "Turnover in bioeconomy per sector_total")
#       plot_spider_chart(input$countries, input$years, indicators_5)
#     })
#   }
#   
#   # Update charts when the button is clicked
#   observeEvent(input$update, {
#     update_charts()
#   })
#   
#   # Initial render of the spider charts with default selections
#   update_charts()
# }
# 
# # Run the application
# shinyApp(ui = ui, server = server)
 }

############## FINAL VERSION ###############

# Read the dataset from GitHub
reshaped_df <- readr::read_csv("https://raw.githubusercontent.com/Marcob88/Sustrack/main/datasetmerged.csv", locale = locale(encoding = "UTF-8"))

# Function to generate indicator datasets
generate_indicator_datasets <- function(years, indicators) {
  # Filter data by the selected years
  filtered_data <- reshaped_df %>% filter(Year %in% years)
  
  # Create datasets for each indicator
  indicator_datasets <- map(indicators, ~{
    indicator_name <- .x
    dataset <- filtered_data %>%
      select(Year, Country, !!sym(indicator_name)) %>%  # Select Year, Country, and the specific indicator
      drop_na()  # Remove NA values
    return(dataset)
  })
  
  return(indicator_datasets)
}

# Function to calculate deciles for each indicator dataset
calculate_deciles <- function(data) {
  data %>% 
    group_by(Year) %>% 
    mutate(decile = ntile(!!sym(names(data)[3]), 10)) %>%  # Calculate deciles for the third column (indicator)
    ungroup()
}

# Function to plot spider chart for specific countries and years
plot_spider_chart <- function(countries, years, indicators) {
  # Generate indicator datasets for the selected years
  indicator_datasets <- generate_indicator_datasets(years, indicators)
  
  # Calculate deciles for each indicator dataset
  indicator_deciles <- map(indicator_datasets, calculate_deciles)
  
  # Insert line breaks for long indicator names
  indicators <- sapply(indicators, function(x) {
    paste(strwrap(x, width = 20), collapse = "<br>")
  })
  
  # Initialize spider chart
  spider_chart <- plot_ly(type = 'scatterpolar', fill = 'toself', mode = 'lines+markers', marker = list(symbol = 'circle', size = 8))
  
  # Loop through each country and year to add traces to the chart
  for (country in countries) {
    for (year in years) {
      country_deciles <- data.frame(
        Category = indicators,
        Decile = sapply(1:length(indicators), function(i) {
          deciles <- indicator_deciles[[i]]$decile[indicator_deciles[[i]]$Country == country & indicator_deciles[[i]]$Year == year]
          if (length(deciles) == 0) {
            return(NA)
          } else {
            return(deciles)
          }
        }),
        Year = year,
        Country = country
      )
      
      # Only add the trace if there is no missing data
      if (!any(is.na(country_deciles$Decile))) {
        spider_chart <- spider_chart %>%
          add_trace(r = country_deciles$Decile, 
                    theta = country_deciles$Category,
                    name = paste(country, year))
      }
    }
  }
  
  spider_chart <- spider_chart %>%
    layout(
      polar = list(radialaxis = list(visible = TRUE, range = c(1, 10), tickmode = 'array', tickvals = 1:10, ticktext = 1:10)),
      showlegend = TRUE,
      margin = list(l = 50, r = 50, b = 50, t = 50)  # Adjust margin to increase space around the chart
    )
  
  return(spider_chart)
}

# UI
ui <- fluidPage(
  theme = "simple",  # Add a clean and simple theme
  titlePanel(
    h1("Spider Charts for Countries and Years", style = "color: #337ab7;")  # Add a blue color to the title
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("countries", "Select Countries", choices = unique(reshaped_df$Country), multiple = TRUE, selected = "EU27(2020)"),
      selectInput("years", "Select Years", choices = unique(reshaped_df$Year), multiple = TRUE, selected = 2019),
      actionButton("update", "Update", icon = icon("refresh"))  # Add a refresh icon to the update button
    ),
    mainPanel(
      div(style = "margin-bottom: 60px;",
          h3(tags$b("Social wellbeing"), style = "color: #337ab7;"),  # Add a blue color to the subtitle
          plotlyOutput("spider_chart_1")),
      hr(),
      div(style = "margin-bottom: 60px;",
          h3(tags$b("Sustainable management of resources"), style = "color: #337ab7;"),  # Add a blue color to the subtitle
          plotlyOutput("spider_chart_2")),
      hr(),
      div(style = "margin-bottom: 60px;",
          h3(tags$b("Reducing dependence on non-renewable, unsustainable resources"), style = "color: #337ab7;"),  # Add a blue color to the subtitle
          plotlyOutput("spider_chart_3")),
      hr(),
      div(style = "margin-bottom: 60px;margin-top: 30px;",
          h3(tags$b("Mitigating and adapting to climate change"), style = "color: #337ab7;"),  # Add a blue color to the subtitle
          plotlyOutput("spider_chart_4")),
      hr(),
      div(style = "margin-bottom: 60px;",
          h3(tags$b("Bioeconomy competitiveness"), style = "color: #337ab7;"),  # Add a blue color to the subtitle
          plotlyOutput("spider_chart_5"))
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive function to update spider charts
  update_charts <- function() {
    output$spider_chart_1 <- renderPlotly({
      indicators_1 <- c("Agricultural factor income per annual work unit (AWU)", 
                        "Prevalence of moderate or severe food insecurity in the total population (percentage)", 
                        "Food purchasing power (food and non-alcoholic beverages)")
      plot_spider_chart(input$countries, input$years, indicators_1)
    })
    
    output$spider_chart_2 <- renderPlotly({
      indicators_2 <- c("Share of organic farming in utilised agricultural area (percentage)", 
                        "Circular material rate (percentage)", 
                        "Recycling rate of municipal waste", 
                        "Food services consumption (Food waste) (tonnes)")
      plot_spider_chart(input$countries, input$years, indicators_2)
    })
    
    output$spider_chart_3 <- renderPlotly({
      indicators_3 <- c("Domestic Material Consumption (biomass share)", 
                        "Share of renewable energy in gross final energy consumption", 
                        "Share of renewables for heating & cooling (percentage)", 
                        "Share of renewables for electricity (percentage)", 
                        "Share of renewables for transport (percentage)")
      plot_spider_chart(input$countries, input$years, indicators_3)
    })
    
    output$spider_chart_4 <- renderPlotly({
      indicators_4 <- c("Material footprint (Biomass)(kg per dollar of GDP in USD)", 
                        "net GHG emissions (emissions and removals) from agriculture_CRF3", 
                        "net GHG emissions (emissions and removals) from LULUCF_CRF4", 
                        "Water exploitation index (WEI)_PC")
      plot_spider_chart(input$countries, input$years, indicators_4)
    })
    
    output$spider_chart_5 <- renderPlotly({
      indicators_5 <- c("Government support to agricultural research and development (by sector) euro per capita", 
                        "Energy productivity (EUR per KG of oil equivalent)", 
                        "Share of renewables for transport, electricity and heating & cooling (percent)", 
                        "Persons employed per bioeconomy sectors_total", 
                        "Turnover in bioeconomy per sector_total")
      plot_spider_chart(input$countries, input$years, indicators_5)
    })
  }
  
  # Update charts when the button is clicked
  observeEvent(input$update, {
    update_charts()
  })
  
  # Initial render of the spider charts with default selections
  update_charts()
}

# Run the application
shinyApp(ui = ui, server = server)
