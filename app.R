library(shiny)
library(readr)
library(tidyverse)
library(ggplot2)
library(AmesHousing)
library(leaflet)
library(scales) 
library(rsconnect)

# Load Dataset and Build a Linear Regression Model
data <- read.csv("ames.csv", stringsAsFactors = FALSE)
model <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built + Year_Remod_Add + Bedroom_AbvGr + 
              Garage_Cars + Garage_Area + Lot_Area + Full_Bath + Half_Bath + Fireplaces + 
              Pool_Area + Total_Bsmt_SF, data = data)

# User Interface
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Times New Roman', serif;
        background-color: #f9f9f9;
      }
      .well {
        background-color: #f0f0f0;
        border: none;
        border-radius: 10px;
        padding: 20px;
      }
      h2, h3 {
        color: #2c3e50;
      }
      .btn {
        background-color: #2c3e50 !important;
        color: white !important;
        font-weight: bold;
        border-radius: 5px;
        padding: 10px 20px;
      }
    "))
  ),
  titlePanel("🏡 Real Estate Price Estimator"),
  h4("By Santiago Freile"),
  tags$div(
    style = "background-color: #ffffff; padding: 15px; border-left: 5px solid #2c3e50; margin-bottom: 15px;",
    HTML("
      <p><strong>🔍 About this App:</strong></p>
      <p>This real estate price estimator was created as part of a Data Visualization project at Linfield University. 
      The model is trained on the <strong>Ames Housing dataset</strong>, a well-known dataset used in the data science community for predictive modeling.</p>
      <p>💡 <em>Goal:</em> Provide an estimate for the price of a house based on user input, using a linear regression model.</p>
      <p>📊 <em>Dataset Source:</em> The Ames Housing dataset is included in the <code>AmesHousing</code> R package and contains real housing data from Ames, Iowa.</p>
      <p>📦 <em>Model:</em> Multiple linear regression trained with key house features such as square footage, number of bedrooms, garage size, and more.</p>
    ")
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput("area", "Living Area (sqft):", min = 300, max = 4000, value = 1500, step = 50),
      selectInput("year", "Year Built:",
                  choices = sort(unique(data$Year_Built)), selected = 2000),
      selectInput("remod", "Year Remodeled:",
                  choices = sort(unique(data$Year_Remod_Add)), selected = 2005),
      radioButtons("bed", "Number of Bedrooms:",
                   choices = 0:10, selected = 3, inline = TRUE),
      selectInput("garage", "Garage Spaces:",
                  choices = list("No garage" = 0, "1 car" = 1, "2 cars" = 2, "3 cars" = 3, "4+ cars" = 4), selected = 2),
      sliderInput("garage_area", "Garage Area (sqft):", min = 0, max = 1500, value = 400, step = 10),
      numericInput("bath_full", "Full Bathrooms:", value = 2, min = 0),
      numericInput("bath_half", "Half Bathrooms:", value = 1, min = 0),
      checkboxInput("fireplace", "Has Fireplace?", value = TRUE),
      sliderInput("pool", "Pool Area (sqft):", min = 0, max = 1000, value = 0, step = 10),
      sliderInput("basement", "Total Basement Area (sqft):", min = 0, max = 2000, value = 800, step = 10),
      sliderInput("lot_area", "Lot Area (sqft):", min = 1000, max = 20000, value = 8000, step = 100),
      actionButton("predict", "Estimate Price"),
    ),
    mainPanel(
      tags$h3("📉 Price Distribution of Similar Houses"),
      tags$p("This plot shows the distribution of sale prices for houses with a similar living area."),
      plotOutput("histogramPlot"),
      
      tags$h3("📍 Estimated Price vs Living Area"),
      tags$p("This scatter plot shows the relationship between living area and price. The red dot represents your estimated house."),
      plotOutput("comparisonPlot"),
      
      tags$h3("🗺️ House Map of Ames, Iowa"),
      tags$p("This map displays houses similar to yours. The red point represents your selected house."),
      leafletOutput("mapPlot", height = 400),
      tags$div(
        style = "margin-top: 20px; padding: 10px; background-color: #ecf0f1; border-radius: 10px; border: 1px solid #2c3e50;",
        h3(textOutput("priceText"))
      )
    )
  ),
)

# Server logic
server <- function(input, output) {
  filtered_data <- reactive({
    req(input$area, input$year, input$bed, input$garage)  
    
    data |>
      filter(
        abs(Gr_Liv_Area - as.numeric(input$area)) <= 200,
        abs(Year_Built - as.numeric(input$year)) <= 5,
        Bedroom_AbvGr == as.numeric(input$bed),
        Garage_Cars == as.numeric(input$garage)
      )
  })
  observeEvent(input$predict, {
    new_data <- data.frame(
      Gr_Liv_Area = as.numeric(input$area),
      Year_Built = as.numeric(input$year),
      Year_Remod_Add = as.numeric(input$remod),
      Bedroom_AbvGr = as.numeric(input$bed),
      Garage_Cars = as.numeric(input$garage),
      Garage_Area = as.numeric(input$garage_area),
      Lot_Area = as.numeric(input$lot_area),
      Full_Bath = as.numeric(input$bath_full),
      Half_Bath = as.numeric(input$bath_half),
      Fireplaces = ifelse(input$fireplace, 1, 0),
      Pool_Area = as.numeric(input$pool),
      Total_Bsmt_SF = as.numeric(input$basement)
    )
    
    predicted_price <- predict(model, newdata = new_data)
    
    output$histogramPlot <- renderPlot({
      similar_houses <- subset(data, abs(Gr_Liv_Area - input$area) < 200)
      
      ggplot(similar_houses, aes(x = Sale_Price)) +
        geom_histogram(binwidth = 20000, fill = "#3498db", alpha = 0.8, color = "black") +
        geom_vline(xintercept = predicted_price, color = "#e74c3c", linetype = "dashed", size = 1.5) +
        scale_x_continuous(labels = comma) +
        labs(
          title = NULL,
          x = "Sale Price ($)",
          y = "Number of Houses"
        ) +
        theme_classic(base_family = "Times New Roman") +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#2c3e50"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
        )
    })
    
    output$priceText <- renderText({
      paste("Estimated Sale Price: $", format(round(predicted_price, 0), big.mark = ","))
    })
    
    output$comparisonPlot <- renderPlot({
      ggplot(data, aes(x = Gr_Liv_Area, y = Sale_Price)) +
        geom_point(alpha = 0.1, color = "#7f8c8d", size = 2) +
        annotate("point", x = input$area, y = predicted_price, color = "#e74c3c", size = 3) +
        scale_y_continuous(labels = comma) + 
        labs(
          title = NULL,
          x = "Living Area (sqft)",
          y = "Sale Price ($)"
        ) +
        theme_classic(base_family = "Times New Roman") +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#2c3e50"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
        )
    })
    
    output$mapPlot <- renderLeaflet({
      filtered <- filtered_data()
      
      if (nrow(filtered) == 0 || any(is.na(filtered$Longitude)) || any(is.na(filtered$Latitude))) {
        leaflet() |>
          addProviderTiles("CartoDB.Positron") |>
          addPopups(
            lng = -93.65, lat = 42.03,
            popup = "No similar houses found with location data."
          )
      } else {
        leaflet(data = filtered) |>
          addProviderTiles("CartoDB.Positron") |>
          addCircleMarkers(
            lng = ~Longitude,
            lat = ~Latitude,
            radius = 5,
            color = "#3498db",
            opacity = 0.7,
            label = ~paste0("Price: $", format(Sale_Price, big.mark = ",")),
            popup = ~paste0(
              "<b>Price:</b> $", format(Sale_Price, big.mark = ","), "<br>",
              "<b>Living Area:</b> ", Gr_Liv_Area, " sqft<br>",
              "<b>Bedrooms:</b> ", Bedroom_AbvGr, "<br>",
              "<b>Year Built:</b> ", Year_Built
            )
          ) |>
          addCircleMarkers(
            lng = mean(filtered$Longitude, na.rm = TRUE),
            lat = mean(filtered$Latitude, na.rm = TRUE),
            radius = 8,
            color = "#e74c3c",
            fill = TRUE,
            fillOpacity = 0.9,
            label = paste0("Estimated Price: $", format(round(predict(model, newdata = filtered[1, ]), 0), big.mark = ",")),
            popup = paste0("<b>Estimated House</b><br>",
                           "Area: ", input$area, " sqft")
          )
      }
    })
    
  })
}

# Run the app
shinyApp(ui = ui, server = server)

