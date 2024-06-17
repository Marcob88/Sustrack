#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

rsconnect::setAccountInfo(name='circularbioeconomyindicatorsystem',
                          #token='068C824643C9D5A39F60F59B01B95416',
                          #secret='qFnwQDwDCLLtJYP/jScHJ4VVE+eKgNFdxjg/XWbt')


library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(readxl)
library(shinythemes)
library(rsconnect)
library(readr)
library(DT)
library(shinyBS)
library(shinydashboard)
library(leaflet)
library(rworldmap)
library(sf)
library(plotly)
library(sp)



#### SUSTRACK #######################

 # SUSTRACK_monitoring_systemQ <- read_csv("C:/Users/109490/OneDrive - Fundacion Tecnalia Research & Innovation/Escritorio/Paper SUSTRACK/SUSTRACK/SUSTRACK monitoring systemR.csv")
SUSTRACK_monitoring_systemR <- readr::read_csv("https://raw.githubusercontent.com/Marcob88/Sustrack/main/CBEindicators.csv", locale = locale(encoding = "UTF-8"))
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

# Function to calculate the area of a polygon
calculate_polygon_area <- function(x, y) {
  # Ensure the polygon is closed
  x <- c(x, x[1])
  y <- c(y, y[1])
  
  # Apply Shoelace formula
  area <- sum(x[-1] * y[-length(y)]) - sum(x[-length(x)] * y[-1])
  area <- abs(area) / 2
  
  return(area)
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



# Define UI
ui <- navbarPage(
    title = "SUSTRACK - Circular Bioeconomy Monitoring System",
    tabPanel(
        "CBE Dashboard",
        dashboardPage(
            dashboardHeader(title = "Sub-themes"),
            dashboardSidebar(
                sidebarMenu(
                    menuItem("Cross-country Analysis", tabName = "cross_country_analysis"),
                    menuItem("Country Performance Analysis", tabName = "country_performance_analysis"),
                    menuItem("Country-specific Analysis", tabName = "country_specific_analysis")
                )
            ),
            dashboardBody(
                tabItems(
                    tabItem(tabName = "cross_country_analysis",
                            h2("Ensuring Food and Nutrition Security (SO1)"),
                            fluidRow(
                                column(6,
                                       selectInput("indicator", "Select Indicator", choices = c(
                                           "Agricultural factor income per annual work unit (AWU)",
                                           "Total biomass supply for food purposes including inputs (kilotonnes dry matter)",
                                           "Biomass directly consumed by EU citizens as food (kilotonnes dry matter)",
                                           "Prevalence of moderate or severe food insecurity in the total population (percentage)",
                                           "Food purchasing power (food and non-alcoholic beverages)",
                                           "Daily calorie supply per capita by animal source",
                                           "Daily calorie supply per capita by animal source (share)",
                                           "Total daily calorie supply per capita",
                                           "Daily calorie supply per capita by vegetal source",
                                           "Daily calorie supply per capita by vegetal source (share)",
                                           "Government support to agricultural research and development (by sector) euro per capita"
                                       ))),
                                column(3,
                                       selectInput("x_indicator", "Select X Indicator", choices = c("population"))),
                                column(3,
                                       selectInput("y_indicator", "Select Y Indicator", choices = c(
                                           "Agricultural factor income per annual work unit (AWU)",
                                           "Total biomass supply for food purposes including inputs (kilotonnes dry matter)",
                                           "Biomass directly consumed by EU citizens as food (kilotonnes dry matter)",
                                           "Prevalence of moderate or severe food insecurity in the total population (percentage)",
                                           "Food purchasing power (food and non-alcoholic beverages)",
                                           "Daily calorie supply per capita by animal source",
                                           "Daily calorie supply per capita by animal source (share)",
                                           "Total daily calorie supply per capita",
                                           "Daily calorie supply per capita by vegetal source",
                                           "Daily calorie supply per capita by vegetal source (share)",
                                           "Government support to agricultural research and development (by sector) euro per capita"
                                       )))
                            ),
                            fluidRow(
                                column(6,
                                       selectInput("year", "Select Year", choices = unique(reshaped_df$Year))),
                                column(3,
                                       selectInput("scatterplot_year", "Select Year for Scatterplot", choices = unique(reshaped_df$Year))),
                                column(3,
                                       selectInput("selected_country", "Select Country", choices = c("All", unique(reshaped_df$Country))))
                            ),
                            fluidRow(
                                column(6,
                                       leafletOutput("map")),
                                column(6,
                                       plotlyOutput("scatterplot"))
                            ),
                            hr(style = "border-top: 1px solid black; margin-top: 20px; margin-bottom: 20px;"),
                            h2("Managing Natural Resources Sustainably (SO2)"),
                                fluidRow(
                                    column(6,
                                           selectInput("indicator2", "Select Indicator", choices = c(
                                               "Biochemical oxygen demand in rivers (mg o2/l)",
                                               "Phosphate in rivers (mg PO4/l)",
                                               "Nitrate in groundwater (NO3/l)",
                                               "Share of organic farming in utilised agricultural area (percentage)",
                                               "Livestock density index (unit per ha)",
                                               "Forest growing stock (1000 m3)",
                                               "CO_FARM_I00_common farmland birds (index, 2000=100)_Bird and butterfly indices EU aggregate (common farmland bird Index, common forest bird index, grassland butterfly index)",
                                               "CO_FOR_I00_common forest birds (index, 2000=100)_Bird and butterfly indices EU aggregate (common farmland bird Index, common forest bird index, grassland butterfly index)",
                                               "Surface of terrestrial sites designated under NATURA 2000 (km2)",
                                               "Surface of terrestrial sites designated under NATURA 2000 (percentage)",
                                               "Surface of marine sites designated under NATURA 2000 (km2)",
                                               "Ratio of annual fellings (m3/ha/year) to net annual increment (m3/ha/year)",
                                               "trend_fellings_trend_fellings_Ratio of annual fellings (m3/ha/year) to net annual increment (m3/ha/year)",
                                               "Intensification of farming (share of high input farms in UAA)",
                                               "Intensification of farming (share of low input farms in UAA)",
                                               "Intensification of farming (share of medium input farms in UAA)",
                                               "trend_LOW_INP_trend_LOW_INP_Intensification of farming (share of high, medium and low input farms in UAA)",
                                               "Biomass production from agriculture (tonnes dry matter)",
                                               "Biomass production from fisheries (tonnes dry matter)",
                                               "Biomass production from forestry (tonnes dry matter)",
                                               "Roundwood removals (m3 overbark roundwood removals)",
                                               "Roundwood removals (m3 underbark roundwood removals)"
                                           ))),
                                    column(3,
                                           selectInput("x_indicator2", "Select X Indicator", choices = c("population"))),
                                    column(3,
                                           selectInput("y_indicator2", "Select Y Indicator", choices = c(
                                               "Biochemical oxygen demand in rivers (mg o2/l)",
                                               "Phosphate in rivers (mg PO4/l)",
                                               "Nitrate in groundwater (NO3/l)",
                                               "Share of organic farming in utilised agricultural area (percentage)",
                                               "Livestock density index (unit per ha)",
                                               "Forest growing stock (1000 m3)",
                                               "CO_FARM_I00_common farmland birds (index, 2000=100)_Bird and butterfly indices EU aggregate (common farmland bird Index, common forest bird index, grassland butterfly index)",
                                               "CO_FOR_I00_common forest birds (index, 2000=100)_Bird and butterfly indices EU aggregate (common farmland bird Index, common forest bird index, grassland butterfly index)",
                                               "Surface of terrestrial sites designated under NATURA 2000 (km2)",
                                               "Surface of terrestrial sites designated under NATURA 2000 (percentage)",
                                               "Surface of marine sites designated under NATURA 2000 (km2)",
                                               "Ratio of annual fellings (m3/ha/year) to net annual increment (m3/ha/year)",
                                               "trend_fellings_trend_fellings_Ratio of annual fellings (m3/ha/year) to net annual increment (m3/ha/year)",
                                               "Intensification of farming (share of high input farms in UAA)",
                                               "Intensification of farming (share of low input farms in UAA)",
                                               "Intensification of farming (share of medium input farms in UAA)",
                                               "trend_LOW_INP_trend_LOW_INP_Intensification of farming (share of high, medium and low input farms in UAA)",
                                               "Biomass production from agriculture (tonnes dry matter)",
                                               "Biomass production from fisheries (tonnes dry matter)",
                                               "Biomass production from forestry (tonnes dry matter)",
                                               "Roundwood removals (m3 overbark roundwood removals)",
                                               "Roundwood removals (m3 underbark roundwood removals)"
                                           )))
                                ),
                                fluidRow(
                                    column(6,
                                           selectInput("year2", "Select Year", choices = unique(reshaped_df$Year))),
                                    column(3,
                                           selectInput("scatterplot_year2", "Select Year for Scatterplot", choices = unique(reshaped_df$Year))),
                                    column(3,
                                           selectInput("selected_country2", "Select Country", choices = c("All", unique(reshaped_df$Country))))
                                ),
                                fluidRow(
                                    column(6,
                                           leafletOutput("map2")),
                                    column(6,
                                           plotlyOutput("scatterplot2"))
                                ),
                                hr(style = "border-top: 1px solid black; margin-top: 20px; margin-bottom: 20px;"),
                                h2("Reducing Dependence on Non-renewable Unsustainable Resources (SO3)"),

                                    fluidRow(
                                        column(6,
                                               selectInput("indicator3", "Select Indicator", choices = c(
                                                   "Domestic Material Consumption (biomass share)",
                                                   "Material footprint (Biomass)(kg per dollar of GDP in USD)",
                                                   "Energy productivity (EUR per KG of oil equivalent)",
                                                   "Share of renewable energy in gross final energy consumption",
                                                   "Cascade use of wood resources (rest of biomass)",
                                                   "Cascade use of wood resources (secondary woody biomass used for material industry)(1000 m3 solid wood equivalent)",
                                                   "Cascade use of wood resources (secondary woody biomass used for energy)(1000 m3 solid wood equivalent)",
                                                   "Cascade use of wood resources (share of secondary woody biomass for energy)",
                                                   "Cascade use of wood resources (share of secondary woody biomass for material industry)",
                                                   "Cascade use of wood resources (total uses of woody biomass)(1000 m3 solid wood equivalent)",
                                                   "Circular material rate (percentage)",
                                                   "Recycling rate of municipal waste",
                                                   "Total generated biowaste (kilotonnes dry)",
                                                   "Total disposed biowaste",
                                                   "Total recovered biowaste (kilotonnes dry)",
                                                   "Total food waste generation along supply chain (tonnes)",
                                                   "Total food waste by food category",
                                                   "Total biomass consumed for energy (kilotonnes dry matter)",
                                                   "Total biomass consumed for materials (kilotonnes dry matter)",
                                                   "Share of woody biomass used for energy (percent)",
                                                   "Share of woody biomass used for materials (percent)",
                                                   "Share of renewables for transport, electricity and heating & cooling (percent)"
                                               ))),
                                        column(3,
                                               selectInput("x_indicator3", "Select X Indicator", choices = c("population"))),
                                        column(3,
                                               selectInput("y_indicator3", "Select Y Indicator", choices = c(
                                                   "Domestic Material Consumption (biomass share)",
                                                   "Material footprint (Biomass)(kg per dollar of GDP in USD)",
                                                   "Energy productivity (EUR per KG of oil equivalent)",
                                                   "Share of renewable energy in gross final energy consumption",
                                                   "Cascade use of wood resources (rest of biomass)",
                                                   "Cascade use of wood resources (secondary woody biomass used for material industry)(1000 m3 solid wood equivalent)",
                                                   "Cascade use of wood resources (secondary woody biomass used for energy)(1000 m3 solid wood equivalent)",
                                                   "Cascade use of wood resources (share of secondary woody biomass for energy)",
                                                   "Cascade use of wood resources (share of secondary woody biomass for material industry)",
                                                   "Cascade use of wood resources (total uses of woody biomass)(1000 m3 solid wood equivalent)",
                                                   "Circular material rate (percentage)",
                                                   "Recycling rate of municipal waste",
                                                   "Total generated biowaste (kilotonnes dry)",
                                                   "Total disposed biowaste",
                                                   "Total recovered biowaste (kilotonnes dry)",
                                                   "Total food waste generation along supply chain (tonnes)",
                                                   "Total food waste by food category",
                                                   "Total biomass consumed for energy (kilotonnes dry matter)",
                                                   "Total biomass consumed for materials (kilotonnes dry matter)",
                                                   "Share of woody biomass used for energy (percent)",
                                                   "Share of woody biomass used for materials (percent)",
                                                   "Share of renewables for transport, electricity and heating & cooling (percent)"
                                               )))
                                    ),
                                    fluidRow(
                                        column(6,
                                               selectInput("year3", "Select Year", choices = unique(reshaped_df$Year))),
                                        column(3,
                                               selectInput("scatterplot_year3", "Select Year for Scatterplot", choices = unique(reshaped_df$Year))),
                                        column(3,
                                               selectInput("selected_country3", "Select Country", choices = c("All", unique(reshaped_df$Country))))
                                    ),
                                    fluidRow(
                                        column(6,
                                               leafletOutput("map3")),
                                        column(6,
                                               plotlyOutput("scatterplot3"))
                                    ),
                                    hr(style = "border-top: 1px solid black; margin-top: 20px; margin-bottom: 20px;"),
                                    h2("Mitigating and Adapting to Climate Change (SO4)"),

                                        fluidRow(
                                            column(6,
                                                   selectInput("indicator4", "Select Indicator", choices = c(
                                                       
                                                       "net GHG emissions (emissions and removals) from agriculture_CRF3",
                                                       "net GHG emissions (emissions and removals) from LULUCF_CRF4",
                                                       "Crop yield (3 main crops)_C1111",
                                                       "Crop yield (3 main crops)_C1112",
                                                       "Crop yield (3 main crops)_C1500",
                                                       "Water exploitation index (WEI)_PC"
                                                   ))),
                                            column(3,
                                                   selectInput("x_indicator4", "Select X Indicator", choices = c("population"))),
                                            column(3,
                                                   selectInput("y_indicator4", "Select Y Indicator", choices = c(
                                                       "net GHG emissions (emissions and removals) from agriculture_CRF3",
                                                       "net GHG emissions (emissions and removals) from LULUCF_CRF4",
                                                       "Crop yield (3 main crops)_C1111",
                                                       "Crop yield (3 main crops)_C1112",
                                                       "Crop yield (3 main crops)_C1500",
                                                       "Water exploitation index (WEI)_PC"
                                                   )))
                                        ),
                                        fluidRow(
                                            column(6,
                                                   selectInput("year4", "Select Year", choices = unique(reshaped_df$Year))),
                                            column(3,
                                                   selectInput("scatterplot_year4", "Select Year for Scatterplot", choices = unique(reshaped_df$Year))),
                                            column(3,
                                                   selectInput("selected_country4", "Select Country", choices = c("All", unique(reshaped_df$Country))))
                                        ),
                                        fluidRow(
                                            column(6,
                                                   leafletOutput("map4")),
                                            column(6,
                                                   plotlyOutput("scatterplot4"))
                                        ),
                                        hr(style = "border-top: 1px solid black; margin-top: 20px; margin-bottom: 20px;"),
                                        h2("Strengthening European Competitiveness and Creating Jobs (SO5)"),
                                        
                                            fluidRow(
                                                column(6,
                                                       selectInput("indicator5", "Select Indicator", choices = c(
                                                           
                                                           "Gross value added per person employed in bioeconomy_total",
                                                           "Turnover in bioeconomy per sector_total",
                                                           "Value-added per sector_total",
                                                           "Persons employed per bioeconomy sectors_total"
                                                       ))),
                                                column(3,
                                                       selectInput("x_indicator5", "Select X Indicator", choices = c("population"))),
                                                column(3,
                                                       selectInput("y_indicator5", "Select Y Indicator", choices = c(
                                                           "Gross value added per person employed in bioeconomy_total",
                                                           "Turnover in bioeconomy per sector_total",
                                                           "Value-added per sector_total",
                                                           "Persons employed per bioeconomy sectors_total"
                                                       )))
                                            ),
                                            fluidRow(
                                                column(6,
                                                       selectInput("year5", "Select Year", choices = unique(reshaped_df$Year))),
                                                column(3,
                                                       selectInput("scatterplot_year5", "Select Year for Scatterplot", choices = unique(reshaped_df$Year))),
                                                column(3,
                                                       selectInput("selected_country5", "Select Country", choices = c("All", unique(reshaped_df$Country))))
                                            ),
                                            fluidRow(
                                                column(6,
                                                       leafletOutput("map5")),
                                                column(6,
                                                       plotlyOutput("scatterplot5"))
                                            )
            
                                    
                                
                    ),
                    tabItem(tabName = "country_performance_analysis",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("countries", "Select Countries", choices = unique(reshaped_df$Country), multiple = TRUE, selected = "EU27(2020)"),
                                selectInput("years", "Select Years", choices = unique(reshaped_df$Year), multiple = TRUE, selected = 2019),
                                actionButton("update", "Update", icon = icon("refresh"))  # Add a refresh icon to the update button
                              ),
                              mainPanel(
                            h2("Country Performance Analysis"),
                            
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
                    )),
                    
                    tabItem(tabName = "country_specific_analysis",
                            h2("----UNDER CONSTRUCTION----"),
                            fluidRow(
                                column(6,
                                       selectInput("selected_country_analysis", "Select Country", choices = unique(reshaped_df$Country)))
                            ),
                            fluidRow(
                                column(6, plotlyOutput("default_plot_1")),
                                column(6, plotlyOutput("default_plot_2"))
                            ),
                            fluidRow(
                                column(6, plotlyOutput("default_plot_3")),
                                column(6, plotlyOutput("default_plot_4"))
                            ))
                    
                    
                )
            )
        )
    ),
    
    
    tabPanel(
        "CBE Indicators",
        sidebarLayout(
            # Sidebar content for Filtered Data tab
            sidebarPanel(
                tags$style(
                    ".sidebar-panel {padding: 5px; margin: 2px; background-color: #D9E5A5; color: #3E352C;}",
                    ".checkbox {color: #3E352C;}"
                ),
                downloadButton("download_filtered_data", "Download Filtered Data", class = "btn-primary", style = "background-color: #EAE5C5; border-color: #D9E5A5; color: #3E352C;"),
                br(),
                br(),
                textInput("keyword_input", "Enter keywords to filter indicators"),
                hr(),
                checkboxGroupInput("status_filter", "Indicator Status",
                                   choices = c("Existing", "Proposed")),
                hr(),
                h4("Implementation Level"),
                checkboxInput("EU_implementation_level_filter", "EU"),
                checkboxInput("National_implementation_level_filter", "National"),
                checkboxInput("Regions_implementation_level_filter", "Regions/Cities"),
                checkboxInput("Product_implementation_level_filter", "Product/Companies"),
                hr(),
                h4("Sustainability Dimensions"),
                checkboxInput("environmental_filter", "Environmental"),
                checkboxInput("economic_filter", "Economic"),
                checkboxInput("social_filter", "Social"),
                checkboxInput("circularity_filter", "Circularity"),
                hr(),
                h4("Contribution to SDGs"),
                fluidRow(
                    column(3, div(checkboxInput("SDG_1", "#1"), title = "SDG 1: No poverty")),
                    column(3, div(checkboxInput("SDG_6", "#6"), title = "SDG 6: Clean water and sanitation")),
                    column(3, div(checkboxInput("SDG_11", "#11"), title = "SDG 11: Sustainable cities and communities")),
                    column(3, div(checkboxInput("SDG_16", "#16"), title = "SDG 16: Peace, justice, and strong institutions"))
                ),
                fluidRow(
                    column(3, div(checkboxInput("SDG_2", "#2"), title = "SDG 2: Zero hunger")),
                    column(3, div(checkboxInput("SDG_7", "#7"), title = "SDG 7: Affordable and clean energy")),
                    column(3, div(checkboxInput("SDG_12", "#12"), title = "SDG 12: Responsible consumption and production")),
                    column(3, div(checkboxInput("SDG_17", "#17"), title = "SDG 17: Partnerships for the goals"))
                ),
                fluidRow(
                    column(3, div(checkboxInput("SDG_3", "#3"), title = "SDG 3: Good health and well-being")),
                    column(3, div(checkboxInput("SDG_8", "#8"), title = "SDG 8: Decent work and economic growth")),
                    column(3, div(checkboxInput("SDG_13", "#13"), title = "SDG 13: Climate action"))
                ),
                fluidRow(
                    column(3, div(checkboxInput("SDG_4", "#4"), title = "SDG 4: Quality education")),
                    column(3, div(checkboxInput("SDG_9", "#9"), title = "SDG 9: Industry, innovation, and infrastructure")),
                    column(3, div(checkboxInput("SDG_14", "#14"), title = "SDG 14: Life below water"))
                ),
                fluidRow(
                    column(3, div(checkboxInput("SDG_5", "#5"), title = "SDG 5: Gender equality")),
                    column(3, div(checkboxInput("SDG_10", "#10"), title = "SDG 10: Reduced inequalities")),
                    column(3, div(checkboxInput("SDG_15", "#15"), title = "SDG 15: Life on land"))
                ),
                checkboxGroupInput("Societal_goals", "Societal Goals:",
                                   choices = c("SO1 Food and nutrition security", "SO2 Managing Natural Resources Sustainably",
                                               "SO3 Reducing dependence on non-renewable unsustainable resources, whether sourced domestically or from abroad", "SO4 Mitigating and adapting to climate change",
                                               "SO5 Strengthening European Competitiveness and Creating Jobs")),
                tags$style(".form-group { margin-bottom: 1 px; }"),
                fluidRow(
                    column(12,
                           h2("Visitor Counter"),
                           textOutput("visitor_counter")
                    )
                )
            ),
            mainPanel(
                DTOutput("filtered_table")
            )
        )
    )
)



# Define server logic
server <- function(input, output) {
    
    # Originally, the data should be read from a github account (github_url <- "https://github.com/Marcob88/Sustrack/blob/main/SUSTRACK%20monitoring%20systemR.csv")
    # As it is now, data is read from a different directory within the same work folder.
    SUSTRACK_monitoring_systemR <- readr::read_csv("https://raw.githubusercontent.com/Marcob88/Sustrack/main/CBEindicators.csv", locale = locale(encoding = "UTF-8"))
    reshaped_df <- readr::read_csv("https://raw.githubusercontent.com/Marcob88/Sustrack/main/datasetmerged.csv", locale = locale(encoding = "UTF-8"))
    
    filtered_data <- reactive({
        conditions <- list()
        
        if (!is.null(input$status_filter)) {
            conditions$status <- SUSTRACK_monitoring_systemR$`Status of indicator (existing, proposed).` %in% input$status_filter
        }
        
        # SDG filters
        if (input$SDG_1) {
            conditions$sdg1 <- SUSTRACK_monitoring_systemR$`SDG 1: No poverty` %in% "x"
        }
        
        if (input$SDG_2) {
            conditions$sdg2 <- SUSTRACK_monitoring_systemR$`SDG 2: Zero hunger` %in% "x"
        }
        
        if (input$SDG_3) {
            conditions$sdg3 <- SUSTRACK_monitoring_systemR$`SDG 3: Good health and well being` %in% "x"
        }
        
        
        if (input$SDG_4) {
            conditions$sdg4 <- SUSTRACK_monitoring_systemR$`SDG 4: Quality education` %in% "x"
        }
        
        if (input$SDG_5) {
            conditions$sdg2 <- SUSTRACK_monitoring_systemR$`SDG 5: Gender equality` %in% "x"
        }
        
        if (input$SDG_6) {
            conditions$sdg6 <- SUSTRACK_monitoring_systemR$`SDG 6: Clean water and sanitation` %in% "x"
        }
        if (input$SDG_7) {
            conditions$sdg7 <- SUSTRACK_monitoring_systemR$`SDG 7: Affordable and clean energy` %in% "x"
        }
        if (input$SDG_8) {
            conditions$sdg8 <- SUSTRACK_monitoring_systemR$`SDG 8: Decent work and economic growth` %in% "x"
        }
        
        if (input$SDG_9) {
            conditions$sdg9 <- SUSTRACK_monitoring_systemR$`SDG 9: Industry, innovation and infrastructure` %in% "x"
        }
        if (input$SDG_10) {
            conditions$sdg10 <- SUSTRACK_monitoring_systemR$`SDG 10: Reduced inequalities` %in% "x"
        }
        if (input$SDG_11) {
            conditions$sdg11 <- SUSTRACK_monitoring_systemR$`SDG 11: Sustainable cities and communities` %in% "x"
        }
        if (input$SDG_12) {
            conditions$sdg12 <- SUSTRACK_monitoring_systemR$`SDG 12: Responsible consumption and production` %in% "x"
        }
        if (input$SDG_13) {
            conditions$sdg13 <- SUSTRACK_monitoring_systemR$`SDG 13: Climate action` %in% "x"
        }
        if (input$SDG_14) {
            conditions$sdg14 <- SUSTRACK_monitoring_systemR$`SDG 14: Life below water` %in% "x"
        }
        if (input$SDG_15) {
            conditions$sdg15 <- SUSTRACK_monitoring_systemR$`SDG 15: Life on land` %in% "x"
        }
        if (input$SDG_16) {
            conditions$sdg16 <- SUSTRACK_monitoring_systemR$`SDG 16: Peace, justice, and strong institutions` %in% "x"
        }
        if (input$SDG_17) {
            conditions$sdg17 <- SUSTRACK_monitoring_systemR$`SDG 17: Partnerships for the goals` %in% "x"
        }
        
        
        if (any(input$EU_implementation_level_filter )) {
            conditions$EU <- SUSTRACK_monitoring_systemR$EU %in% "x"
        }
        
        if (any(input$National_implementation_level_filter )) {
            conditions$National <- SUSTRACK_monitoring_systemR$National %in% "x"
        }
        
        if (any(input$Regions_implementation_level_filter )) {
            conditions$Regions <- SUSTRACK_monitoring_systemR$`Regional/Cities` %in% "x"
        }
        
        if (any(input$Product_implementation_level_filter )) {
            conditions$Product <- SUSTRACK_monitoring_systemR$`Products /  Companies` %in% "x"
        }
        
        if (input$environmental_filter) {
            conditions$Environment <- SUSTRACK_monitoring_systemR$Environmental %in% "x"
        }
        
        if (input$economic_filter) {
            conditions$Economy <- SUSTRACK_monitoring_systemR$Economic %in% "x"
        }
        
        if (input$social_filter) {
            conditions$Society <- SUSTRACK_monitoring_systemR$Social %in% "x"
        }
        
        if (input$circularity_filter) {
            conditions$Circularity <- SUSTRACK_monitoring_systemR$Circularity %in% "x"
        }
        
        if (!is.null(input$Societal_goals)) {
            conditions$goals <- Reduce(`&`, lapply(input$Societal_goals, function(goal) SUSTRACK_monitoring_systemR$`Societal Goals` == goal))
        }
        
        # Keyword search
        keyword <- input$keyword_input
        if (nchar(keyword) > 0) {
            keyword_conditions <- sapply(SUSTRACK_monitoring_systemR[, "INDICATOR:", drop = FALSE], function(indicator) {
                grepl(keyword, indicator, ignore.case = TRUE)
            })
            conditions$keyword <- rowSums(keyword_conditions) > 0
        }
        
        # Apply filters
        if (length(conditions) == 0) {
            filtered_data <- SUSTRACK_monitoring_systemR
        } else {
            condition_values <- do.call("cbind", conditions)
            filtered_data <- SUSTRACK_monitoring_systemR[rowSums(condition_values) == length(conditions), ]
        }
        
        return(filtered_data)
    })
    
    output$filtered_table <- renderDT({
        # Exclude the columns used as filters
        filtered_data_subset <- filtered_data()
        DT::datatable(filtered_data_subset[, !(names(filtered_data_subset) %in% c("Status of indicator (existing, proposed).", 
                                                                                  "Qualitative indicator", 
                                                                                  "Quantitative indicator", 
                                                                                  "Boolean indicator (yes/No)", 
                                                                                  "EU", 
                                                                                  "National", 
                                                                                  "Regional/Cities", 
                                                                                  "Products /  Companies", 
                                                                                  "Environmental",
                                                                                  "Required methodology",
                                                                                  "Economic", 
                                                                                  "Social", 
                                                                                  "Circularity", 
                                                                                  "Societal Goals",
                                                                                  "Temporal", "Geographic", 
                                                                                  "Description of the indicator/signal/potential",
                                                                                  "Methodological considerations",
                                                                                  "Link to reference",
                                                                                  "Link to data"))],
                      options = list(
                          scrollX = TRUE,  # Enable horizontal scrolling
                          scrollY = "600px",  # Set vertical scrolling height
                          fixedColumns = list(leftColumns = 1),  # Fix the first column
                          ordering = FALSE  # Disable column ordering
                      ))
    })
    
    # Function to generate download file
    output$download_filtered_data <- downloadHandler(
        filename = function() {
            paste("filtered_data", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(filtered_data(), file, row.names = FALSE)
        }
    )
    
    
    
    # Cross-Country Comparison Tab
    
    # Function to render map
    render_map <- function(selected_indicator, filtered_data, color_scale) {
       
         # Remove rows with NA values for the selected indicator
        filtered_data <- filtered_data[!is.na(filtered_data[[selected_indicator]]), ]
        
        # Create world map and merge data with it. For a finer resolution, the "rworldextra" package may be used.
        
        world <- st_as_sf(rworldmap::getMap(resolution = "coarse"))
        
        map_data <- left_join(world, filtered_data, by = c("ISO_A2" = "Geo_code"))
        
        # Create color palette. For more color scales check: https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
        color_palette <- colorQuantile(color_scale, map_data[[selected_indicator]])
        
        # Create map
        leaflet(data = map_data) %>%
            addTiles() %>%
            setView(lng = 10, lat = 50, zoom = 4) %>%
            addPolygons(fillColor = ~color_palette(map_data[[selected_indicator]]),
                        fillOpacity = 0.7, 
                        weight = 1,
                        color = "white",
                        dashArray = "3",
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE
                        ),
                        label = ~paste(NAME, ": ", map_data[[selected_indicator]])) %>%
            addLegend(position = "bottomright", 
                      pal = color_palette, 
                      values = ~map_data[[selected_indicator]], 
                      title = "Legend", 
                      opacity = 0.7)
    }
    
    # Function to render scatterplot
    render_scatterplot <- function(selected_x_indicator, selected_y_indicator, filtered_data_scatter, selected_selected_country) {
        # Create scatterplot using plot_ly
        if (selected_selected_country == "All"){
          country_df <- filtered_data_scatter
        } else {
          country_df <- subset(filtered_data_scatter, Country == selected_selected_country)
        }
        fig<-plot_ly(data = filtered_data_scatter,
                x = ~get(selected_x_indicator),
                y = ~get(selected_y_indicator),
                text = ~Geo_code,  # Text to display when hovering over points
                mode = "markers",
                color = I("blue"),
                type = "scatter",
                marker = list(size = 10)) %>%
        add_trace(data = country_df, # This trace highlights the point corresponding to the selected country.
                       x = ~get(selected_x_indicator),
                       y = ~get(selected_y_indicator),
                       type = "scatter",
                       color = I("red"),
                       mode = "markers", 
                       marker = list(size = 12)) %>%
        layout(title = "Cross-Country Comparison Scatterplot",
                   xaxis = list(title = selected_x_indicator, linecolor = "white", gridcolor = "rgba(144,238,144,0.3)"),
                   yaxis = list(title = selected_y_indicator, linecolor = "white", gridcolor = "rgba(144,238,144,0.3)"),
                   plot_bgcolor = "black",  # Set background color to black
                   paper_bgcolor = "black", # Set paper (plot area) color to black
                   font = list(color = "white"), # Set font color to white
                   showlegend = FALSE,
                   margin = list(l = 50, r = 50, b = 50, t = 50) # Set margins with white color
            )
        fig
    }
    
    # Map SO1 
    output$map <- renderLeaflet({
        selected_year <- input$year
        selected_indicator <- input$indicator
        filtered_data <- subset(reshaped_df, Year == selected_year)
        render_map(selected_indicator, filtered_data, "YlOrRd")
    })
    
    # Scatterplot 1
    output$scatterplot <- renderPlotly({
        selected_year_scatter <- input$scatterplot_year
        selected_x_indicator <- input$x_indicator
        selected_y_indicator <- input$y_indicator
        selected_country <- input$selected_country
        
        filtered_data_scatter <- subset(reshaped_df, 
                                        Year == selected_year_scatter & 
                                            !(Geo_code %in% c("EU27_2020", "EU28")))
        
        render_scatterplot(selected_x_indicator, selected_y_indicator, filtered_data_scatter, selected_country)
    })
    
    # Map SO2 
    output$map2 <- renderLeaflet({
        selected_year <- input$year2
        selected_indicator <- input$indicator2
        filtered_data <- subset(reshaped_df, Year == selected_year)
        render_map(selected_indicator, filtered_data, "YlGnBu")
    })
    
    # Scatterplot SO2
    output$scatterplot2 <- renderPlotly({
        selected_year_scatter <- input$scatterplot_year2
        selected_x_indicator <- input$x_indicator2
        selected_y_indicator <- input$y_indicator2
        selected_country <- input$selected_country2
        
        filtered_data_scatter <- subset(reshaped_df, 
                                        Year == selected_year_scatter & 
                                            !(Geo_code %in% c("EU27_2020", "EU28")))
        
        render_scatterplot(selected_x_indicator, selected_y_indicator, filtered_data_scatter,selected_country)
    })
    # Map SO3 
    output$map3 <- renderLeaflet({
        selected_year <- input$year3
        selected_indicator <- input$indicator3
        filtered_data <- subset(reshaped_df, Year == selected_year)
        render_map(selected_indicator, filtered_data, "OrRd")
    })
    
    # Scatterplot3
    output$scatterplot3 <- renderPlotly({
        selected_year_scatter <- input$scatterplot_year3
        selected_x_indicator <- input$x_indicator3
        selected_y_indicator <- input$y_indicator3
        selected_country <- input$selected_country3
        
        filtered_data_scatter <- subset(reshaped_df, 
                                        Year == selected_year_scatter & 
                                            !(Geo_code %in% c("EU27_2020", "EU28")))
        
        render_scatterplot(selected_x_indicator, selected_y_indicator, filtered_data_scatter,selected_country)
    })
    # Map SO4 
    output$map4 <- renderLeaflet({
        selected_year <- input$year4
        selected_indicator <- input$indicator4
        filtered_data <- subset(reshaped_df, Year == selected_year)
        render_map(selected_indicator, filtered_data, "Greens")
    })
    
    # Scatterplot4
    output$scatterplot4 <- renderPlotly({
        selected_year_scatter <- input$scatterplot_year4
        selected_x_indicator <- input$x_indicator4
        selected_y_indicator <- input$y_indicator4
        selected_country <- input$selected_country4
        
        filtered_data_scatter <- subset(reshaped_df, 
                                        Year == selected_year_scatter & 
                                            !(Geo_code %in% c("EU27_2020", "EU28")))
        
        render_scatterplot(selected_x_indicator, selected_y_indicator, filtered_data_scatter,selected_country)
    })
    # Map SO5 
    output$map5 <- renderLeaflet({
        selected_year <- input$year5
        selected_indicator <- input$indicator5
        filtered_data <- subset(reshaped_df, Year == selected_year)
        render_map(selected_indicator, filtered_data, "Blues")
    })
    
    # Scatterplot5
    output$scatterplot5 <- renderPlotly({
        selected_year_scatter <- input$scatterplot_year5
        selected_x_indicator <- input$x_indicator5
        selected_y_indicator <- input$y_indicator5
        selected_country <- input$selected_country5
        
        filtered_data_scatter <- subset(reshaped_df, 
                                        Year == selected_year_scatter & 
                                            !(Geo_code %in% c("EU27_2020", "EU28")))
        
        render_scatterplot(selected_x_indicator, selected_y_indicator, filtered_data_scatter,selected_country)
    })
    
    #Country performance analysis

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
