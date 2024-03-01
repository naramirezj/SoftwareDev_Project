#
# This is a Shiny web application for an Airbnb Market Analysis.
# Data used: Airbnb Market Analysis & Real Estate Sales Data
# Source: https://www.kaggle.com/datasets/computingvictor/zillow-market-analysis-and-real-estate-sales-data
#

library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(shinyjs)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Airbnb Market Analysis"),

    sidebarLayout(
    sidebarPanel(
      selectInput("city_selector", "Select City", choices = ""),
      sliderInput("bedroom_selector", "Select Number of Bedrooms", min = 1, max = 5, value = c(2, 5)),
      sliderInput("bathroom_selector", "Select Number of Bathrooms", min = 1, max = 5, value = c(2, 5)),
      sliderInput("guests_selector", "Select Number of Guests", min = 3, max = 15, value = c(3, 15)),
      actionButton("filter_highest_earning", "Filter Highest Earning Airbnbs"),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map"))
        )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("comparison_selector_bed", "Select Metric", choices = c("Average Revenue", "Average Nightly Rate", "Average Occupancy")),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Comparison by Bedrooms", plotOutput("comparison_plot"))
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("comparison_selector_bath", "Select Metric", choices = c("Average Revenue", "Average Nightly Rate", "Average Occupancy")),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Comparison by Bathrooms", plotOutput("comparison_plot_bathroom"))
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("city_selector2", "Select City", choices = ""),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Revenue vs Nightly Rate", plotOutput("revenue_night")),  
      )
    )
  ),
  fluidRow(
    column(12, plotOutput("timelog", width = "100%", height = "800px"))  
  )
)


# Define server logic 
server <- function(input, output, session) {
  time_log <- read.csv('/Users/nataliaramirez/Desktop/SoftwareDev_Project/time_log.csv', sep = ',')
  df1 <- read.csv('/Users/nataliaramirez/Desktop/CSC324/market_analysis.csv', sep = ';')
  df2 <- read.csv('/Users/nataliaramirez/Desktop/CSC324/geolocation.csv', sep = ';')
  merged_data <- merge(df1, df2 %>% mutate(unified_id = sub("^AIR", "", unified_id)), by = "unified_id")
  merged_data <- merged_data %>%
    select(-month.x, -month.y)
  
  result_data <- merged_data %>%
    group_by(unified_id, zipcode, city, host_type, bedrooms, bathrooms, guests, latitude, longitude) %>%
    mutate(guests = as.numeric(gsub("\\+", "", guests)),
           revenue = as.numeric(gsub(",", ".", revenue)),
           nightly.rate = as.numeric(gsub(",", ".", nightly.rate)),
           occupancy = as.numeric(gsub(",", ".",occupancy)),
           latitude = as.numeric(gsub(",", ".",latitude)),
           longitude = as.numeric(gsub(",", ".",longitude)),
           lead.time = as.numeric(gsub(",", ".",lead.time)),
           length.stay = as.numeric(gsub(",", ".",length.stay))) %>%
    filter(revenue != 0.00) %>%
    summarise(
      avg_revenue = mean(revenue),
      avg_nightly_rate = mean(nightly.rate),
      avg_occupancy = mean(occupancy),
      avg_lead_time = mean(lead.time),
      avg_length_stay = mean(length.stay))
  
  result_data <- result_data[!duplicated(result_data$unified_id), ]
  
  observe({
    updateSelectInput(session, "city_selector", choices = unique(result_data$city))
  })
  
  observe({
    updateSelectInput(session, "city_selector2", choices = unique(result_data$city))
  })
  
  output$revenue_night <- renderPlot({
    selected_city_data2 <- result_data %>%
      filter(city == input$city_selector2)
    
    ggplot(selected_city_data2, aes(x = avg_nightly_rate, y = avg_revenue)) +
      geom_point(color = "skyblue", alpha = 0.7) +
      labs(x = "Average Nightly Rate", y = "Average Revenue", title = "Revenue vs Nightly Rate") +
      theme_minimal()
  })
  
  selected_city_data <- reactive({
    result_data %>%
      filter(
        city == input$city_selector,
         bedrooms %in% seq(input$bedroom_selector[1], input$bedroom_selector[2]),
         bathrooms %in% seq(input$bathroom_selector[1], input$bathroom_selector[2]),
         guests %in% seq(input$guests_selector[1], input$guests_selector[2])
        )
  })

  observeEvent(input$filter_highest_earning, {
    highest_earning_data <- selected_city_data() %>%
      arrange(desc(avg_revenue)) %>%
      head(10)  # Show top 10 highest earning Airbnbs
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(
        data = highest_earning_data,
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste("City:", city, "<br>Revenue:", avg_revenue, "<br>Bedrooms:", bedrooms)  
      )
  })
  
  output$map <- renderLeaflet({
    leaflet(selected_city_data()) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste("City:", city, "<br>Revenue:", avg_revenue, "<br>Bedrooms:", bedrooms)  
      )
  })
  
  output$comparison_plot <- renderPlot({
    selected_metric <- switch(input$comparison_selector_bed,
                              "Average Revenue" = "avg_revenue",
                              "Average Nightly Rate" = "avg_nightly_rate",
                              "Average Occupancy" = "avg_occupancy",
    )
    
    ggplot(result_data, aes(x = as.factor(bedrooms), y = .data[[selected_metric]])) +
      geom_jitter(aes(color = as.factor(bedrooms)), position = position_jitter(width = 0.2), alpha = 0.7) +
      labs(x = "Number of Bedrooms", y = input$comparison_selector_bed, title = "Comparison by Bedroom") +
      theme_minimal()
  })
  
  output$comparison_plot_bathroom <- renderPlot({
    selected_metric_bath <- switch(input$comparison_selector_bath,
                              "Average Revenue" = "avg_revenue",
                              "Average Nightly Rate" = "avg_nightly_rate",
                              "Average Occupancy" = "avg_occupancy",
    )
    
    
    ggplot(result_data, aes(x = as.factor(bathrooms), y = .data[[selected_metric_bath]])) +
      geom_bar(stat = "identity", fill = "skyblue", position = "dodge") +
      labs(x = "Number of Bathrooms", y = input$comparison_selector_bath, title = "Comparison by Bathrooms") +
      theme_minimal()
  })

  output$timelog <- renderPlot({
    time_log$Date <- as.Date(time_log$Date, format = "%m/%d/%y")
    
    ggplot(time_log, aes(x = Date, y = Hours, fill = Task)) +
      geom_bar(stat = "identity") +
      labs(x = "Date", y = "Hours", title = "Time Log Visualization") +
      scale_x_date(breaks = seq(min(time_log$Date, na.rm = TRUE), max(time_log$Date, na.rm = TRUE), by = "1 day"), date_labels = "%m-%d") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), 
        axis.title = element_text(size = 14) 
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
