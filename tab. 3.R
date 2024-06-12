
# Define UI
ui <- navbarPage(
  title = "SUSTRACK - Circular Bioeconomy Monitoring System",
  tabPanel(
    "CBE Dashboard",
    dashboardPage(
      dashboardHeader(title = "Sub-themes"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Country-specific Analysis", tabName = "country_specific_analysis")
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "country_specific_analysis",
                  h2("Waste and Circularity"),
                  fluidRow(
                    column(4,
                           selectInput("selected_country_analysis", "Select Country", choices = unique(reshaped_df$Country)))
                  ),
                  fluidRow(
                    column(4, checkboxInput("relative_view", "Relative View", value = FALSE))
                  ),
                  fluidRow(
                    column(4, plotlyOutput("stacked_bar_plot3")),
                    column(4, plotlyOutput("stacked_bar_plot2")),
                    column(4, plotlyOutput("stacked_bar_plot1"))
                  )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # # Read the dataset from GitHub
  
  SUSTRACK_monitoring_systemR <- readr::read_csv("https://raw.githubusercontent.com/Marcob88/Sustrack/main/CBEindicators.csv", locale = locale(encoding = "UTF-8"))
  reshaped_df <- readr::read_csv("https://raw.githubusercontent.com/Marcob88/Sustrack/main/datasetmerged.csv", locale = locale(encoding = "UTF-8"))
  
  # Reactive filtered data based on selected country
  country_data <- reactive({
    filter(reshaped_df, Country == input$selected_country_analysis)
  })
  
  # Render stacked bar plot 1
  output$stacked_bar_plot1 <- renderPlotly({
    data <- country_data()
    
    if(input$relative_view) {
      # Calculate total for each year
      data$total <- rowSums(data[, c("Household - Paper and cardboard wastes (W072)", 
                                     "Household - Untreated wood (W075)",
                                     "Household food waste (W091)",
                                     "Household green waste (W092)",
                                     "Households textiles, leather and rubber waste (W7376)",
                                     "Household composites, human hygiene waste (W9999)"), 
                                 with = FALSE])
      # Calculate percentages
      data <- data %>%
        mutate_at(vars(starts_with("Household")), funs(./total * 100)) %>%
        select(-total)
    }
    
    plot_ly(data, x = ~Year, y = ~`Household - Paper and cardboard wastes (W072)`, type = 'bar', name = 'Paper and cardboard wastes (W072)') %>%
      add_trace(y = ~`Household - Untreated wood (W075)`, name = 'Untreated wood waste (W075)') %>%
      add_trace(y = ~`Household food waste (W091)`, name = 'Food waste (W091)') %>%
      add_trace(y = ~`Household green waste (W092)`, name = 'Green waste (W092)') %>%
      add_trace(y = ~`Households textiles, leather and rubber waste (W7376)`, name = 'Textiles, leather and rubber waste (W7376)') %>%
      add_trace(y = ~`Household composites, human hygiene waste (W9999)`, name = 'Composites, human hygiene waste (W9999)') %>%
      layout(title = "Household Biowaste",
             xaxis = list(title = "Year"),
             yaxis = list(title = if(input$relative_view) "Percentage" else "Kilotonnes dry"),
             barmode = if(input$relative_view) 'relative' else 'stack',  # Change to relative for 100% stacked bar
             legend = list(x = 0.1, y = -0.3, orientation = "h"))  # Adjust legend position
  })
  
  # Render stacked bar plot 2 (example with different sectors)
  output$stacked_bar_plot2 <- renderPlotly({
    data <- country_data()
    
    if(input$relative_view) {
      # Calculate total for each year
      data$total <- rowSums(data[, c("Industrial and agricultural paper and cardboard wastes (W072)", 
                                     "Industrial rubber wastes (W073)",
                                     "Industrial wood wastes (W075)",
                                     "Industrial textile wastes (W076)",
                                     "Industrial vegetal waste (W092)", 
                                     "Industrial animal feces, urine and manure waste (W093)"), 
                                 with = FALSE])
      # Calculate percentages
      data <- data %>%
        mutate_at(vars(starts_with("Industrial")), funs(./total * 100)) %>%
        select(-total)
    }
    
    plot_ly(data, x = ~Year, y = ~`Industrial and agricultural paper and cardboard wastes (W072)`, type = 'bar', name = 'Paper and cardboard wastes (W072)') %>%
      add_trace(y = ~`Industrial rubber wastes (W073)`, name = 'Rubber wastes (W073)') %>%
      add_trace(y = ~`Industrial wood wastes (W075)`, name = 'Wood wastes (W075)') %>%
      add_trace(y = ~`Industrial textile wastes (W076)`, name = 'Textile wastes (W076)') %>%
      add_trace(y = ~`Industrial vegetal waste (W092)`, name = 'Vegetal wastes (W092)') %>%
      add_trace(y = ~`Industrial textile wastes (W076)`, name = 'Textile wastes (W076)') %>%
      add_trace(y = ~`Industrial animal feces, urine and manure waste (W093)`, name = 'Animal feces, urine and manure wastes (W093)') %>%
      layout(title = "Industrial waste",
             xaxis = list(title = "Year"),
             yaxis = list(title = if(input$relative_view) "Percentage" else "Kilotonnes dry"),
             barmode = if(input$relative_view) 'relative' else 'stack',  # Change to relative for 100% stacked bar
             legend = list(x = 0.1, y = -0.3, orientation = "h"))  # Adjust legend position
  })

  # Render stacked bar plot 3 (total biowaste)
output$stacked_bar_plot3 <- renderPlotly({
  data <- country_data()
  
  if(input$relative_view) {
    # Calculate total for each year
    data$total <- rowSums(data[, c("Biowaste generated by household (kilotonnes dry)", 
                                   "Generated industrial and agricultural biowaste (kilotonnes dry)"),
                               with = FALSE])
    # Calculate percentages
    data <- data %>%
      mutate_at(vars(starts_with("Industrial")), funs(./total * 100)) %>%
      select(-total)
  }
  
  plot_ly(data, x = ~Year, y = ~`Biowaste generated by household (kilotonnes dry)`, type = 'bar', name = 'Household biowaste') %>%
    add_trace(y = ~`Generated industrial and agricultural biowaste (kilotonnes dry)`, name = 'Industrial and agricultural biowaste') %>%
    layout(title = "Total biowaste",
           xaxis = list(title = "Year"),
           yaxis = list(title = if(input$relative_view) "Percentage" else "Kilotonnes dry"),
           barmode = if(input$relative_view) 'relative' else 'stack',  # Change to relative for 100% stacked bar
           legend = list(x = 0.1, y = -0.3, orientation = "h"))  # Adjust legend position
})
}

# Run the application
shinyApp(ui = ui, server = server)
