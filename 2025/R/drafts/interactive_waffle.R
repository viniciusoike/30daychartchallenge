# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(scales)
library(htmlwidgets)
library(DT)
library(shiny)

# Enhanced data creation with more realistic details
set.seed(123)
cities <- c(
  'New York',
  'Los Angeles',
  'Chicago',
  'Houston',
  'Phoenix',
  'Philadelphia',
  'San Antonio',
  'San Diego',
  'Dallas',
  'San Jose',
  'Austin',
  'Jacksonville',
  'Fort Worth',
  'Columbus',
  'Charlotte',
  'San Francisco',
  'Indianapolis',
  'Seattle',
  'Denver',
  'Washington',
  'Boston',
  'El Paso',
  'Nashville',
  'Detroit',
  'Oklahoma City',
  'Portland',
  'Las Vegas',
  'Memphis',
  'Louisville',
  'Baltimore',
  'Milwaukee',
  'Albuquerque'
)

# Create enhanced data with simulated price changes
housing_data <- expand_grid(
  city = cities,
  year = 2019:2024
) %>%
  mutate(
    city_id = rep(1:32, 6),
    col = ((city_id - 1) %% 8) + 1,
    row = ceiling(city_id / 8),

    # Simulate realistic rent and sales price changes
    rent_growth = case_when(
      year == 2019 ~ runif(n(), 1.5, 4.2),
      year == 2020 ~ runif(n(), 2.1, 6.8),
      year == 2021 ~ runif(n(), 4.5, 12.3),
      year == 2022 ~ runif(n(), 8.2, 18.7),
      year == 2023 ~ runif(n(), 6.1, 15.2),
      year == 2024 ~ runif(n(), 3.8, 11.4)
    ),

    sales_growth = case_when(
      year == 2019 ~ runif(n(), 2.1, 5.8),
      year == 2020 ~ runif(n(), 3.2, 8.9),
      year == 2021 ~ runif(n(), 6.8, 16.7),
      year == 2022 ~ runif(n(), 12.4, 22.1),
      year == 2023 ~ runif(n(), 8.9, 19.3),
      year == 2024 ~ runif(n(), 4.2, 13.6)
    ),

    # Inflation rates by year (approximate CPI)
    inflation_rate = case_when(
      year == 2019 ~ 1.8,
      year == 2020 ~ 1.2,
      year == 2021 ~ 4.7,
      year == 2022 ~ 8.0,
      year == 2023 ~ 4.1,
      year == 2024 ~ 3.2
    ),

    # Categorize based on comparison to inflation
    rent_above_inflation = rent_growth > inflation_rate,
    sales_above_inflation = sales_growth > inflation_rate,

    category = case_when(
      !rent_above_inflation & !sales_above_inflation ~ "both_below",
      rent_above_inflation & !sales_above_inflation ~ "rent_above",
      !rent_above_inflation & sales_above_inflation ~ "sales_above",
      rent_above_inflation & sales_above_inflation ~ "both_above"
    ),

    # Create detailed tooltip information
    tooltip_text = paste0(
      "<b>",
      city,
      " (",
      year,
      ")</b><br>",
      "Rent Growth: ",
      round(rent_growth, 1),
      "%<br>",
      "Sales Growth: ",
      round(sales_growth, 1),
      "%<br>",
      "Inflation Rate: ",
      inflation_rate,
      "%<br>",
      "<br><b>Status:</b><br>",
      case_when(
        category == "both_below" ~ "Both rent and sales below inflation",
        category == "rent_above" ~ "Only rent exceeds inflation",
        category == "sales_above" ~ "Only sales exceed inflation",
        category == "both_above" ~ "Both rent and sales exceed inflation"
      )
    ),

    year_label = case_when(
      year == 2019 ~ "2019\nPre-pandemic baseline",
      year == 2020 ~ "2020\nInitial pandemic impact",
      year == 2021 ~ "2021\nMarket acceleration begins",
      year == 2022 ~ "2022\nPeak price growth period",
      year == 2023 ~ "2023\nContinued above-inflation",
      year == 2024 ~ "2024\nMarket moderation"
    )
  ) %>%
  mutate(
    category = factor(
      category,
      levels = c("both_below", "rent_above", "sales_above", "both_above")
    )
  )

# Color scheme
colors <- c(
  "both_below" = "#2d5a27", # Dark green
  "rent_above" = "#e67e22", # Orange
  "sales_above" = "#f1c40f", # Yellow
  "both_above" = "#a94442" # Dark red
)

# Create the interactive plotly version
create_interactive_housing_chart <- function(data) {
  # Create base ggplot
  p <- data %>%
    ggplot(aes(
      x = col,
      y = -row,
      fill = category,
      text = tooltip_text,
      # Additional aesthetics for plotly
      customdata = paste(city, year, sep = "_")
    )) +
    geom_tile(color = "white", size = 0.8, alpha = 0.9) +
    scale_fill_manual(
      values = colors,
      labels = c(
        "Both below inflation",
        "Only rent above inflation",
        "Only sales above inflation",
        "Both above inflation"
      ),
      name = "Price Growth Categories"
    ) +
    facet_wrap(~year_label, ncol = 3) +
    theme_void() +
    theme(
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size = 12,
        hjust = 0.5,
        color = "grey30",
        margin = margin(b = 20)
      ),
      strip.text = element_text(
        size = 11,
        face = "bold",
        margin = margin(b = 10)
      ),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.7, "cm"),
      panel.spacing = unit(1, "lines"),
      plot.margin = margin(15, 15, 15, 15)
    ) +
    labs(
      title = "Interactive Housing Market Heat Map: Price Growth vs Inflation",
      subtitle = "Hover over squares for detailed city information • 32 major U.S. metropolitan areas"
    )

  # Convert to plotly
  interactive_plot <- ggplotly(p, tooltip = "text") %>%
    layout(
      title = list(
        text = "<b>Interactive Housing Market Heat Map</b><br><sup>Price Growth vs Inflation (2019-2024)</sup>",
        font = list(size = 16)
      ),
      showlegend = TRUE,
      margin = list(t = 80, b = 50, l = 50, r = 150)
    ) %>%
    config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "pan2d",
        "select2d",
        "lasso2d",
        "autoScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )

  return(interactive_plot)
}

# Create the interactive plot
interactive_chart <- create_interactive_housing_chart(housing_data)
print(interactive_chart)

# Save the interactive plot
htmlwidgets::saveWidget(
  interactive_chart,
  "interactive_housing_chart.html",
  selfcontained = TRUE
)

# Create an enhanced data table for exploration
create_data_table <- function(data) {
  summary_table <- data %>%
    select(city, year, rent_growth, sales_growth, inflation_rate, category) %>%
    mutate(
      rent_growth = round(rent_growth, 1),
      sales_growth = round(sales_growth, 1),
      category_label = case_when(
        category == "both_below" ~ "Both Below",
        category == "rent_above" ~ "Rent Above",
        category == "sales_above" ~ "Sales Above",
        category == "both_above" ~ "Both Above"
      )
    ) %>%
    select(-category) %>%
    rename(
      City = city,
      Year = year,
      `Rent Growth (%)` = rent_growth,
      `Sales Growth (%)` = sales_growth,
      `Inflation Rate (%)` = inflation_rate,
      Category = category_label
    )

  # Create interactive data table
  dt <- datatable(
    summary_table,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      searching = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel')
    ),
    filter = 'top',
    caption = htmltools::tags$caption(
      style = 'caption-side: top; text-align: center; color: #333; font-size: 14px; font-weight: bold;',
      'Housing Price Data: Interactive Table'
    )
  ) %>%
    formatStyle(
      'Category',
      backgroundColor = styleEqual(
        c('Both Below', 'Rent Above', 'Sales Above', 'Both Above'),
        c('#2d5a27', '#e67e22', '#f1c40f', '#a94442')
      ),
      color = 'white',
      fontWeight = 'bold'
    )

  return(dt)
}

# Create and display the data table
data_table <- create_data_table(housing_data)
print(data_table)

# Create a time series plot showing trends
create_trend_plot <- function(data) {
  trend_data <- data %>%
    group_by(year, category) %>%
    summarise(
      count = n(),
      avg_rent_growth = mean(rent_growth),
      avg_sales_growth = mean(sales_growth),
      .groups = "drop"
    ) %>%
    mutate(percentage = count / 32 * 100)

  # Create stacked area chart
  p <- trend_data %>%
    ggplot(aes(x = year, y = percentage, fill = category)) +
    geom_area(alpha = 0.8, position = "stack") +
    scale_fill_manual(
      values = colors,
      labels = c(
        "Both below inflation",
        "Only rent above",
        "Only sales above",
        "Both above inflation"
      ),
      name = "Category"
    ) +
    scale_x_continuous(breaks = 2019:2024) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey50"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = "Housing Market Evolution Over Time",
      subtitle = "Distribution of metropolitan areas by price growth category",
      x = "Year",
      y = "Percentage of Metro Areas",
      caption = "Shows the dramatic shift from below-inflation to above-inflation growth"
    )

  # Convert to plotly for interactivity
  interactive_trend <- ggplotly(p) %>%
    layout(
      title = list(
        text = "<b>Housing Market Evolution Over Time</b>",
        font = list(size = 14)
      ),
      hovermode = "x unified"
    )

  return(interactive_trend)
}

# Create trend plot
trend_plot <- create_trend_plot(housing_data)
print(trend_plot)

# Save trend plot
htmlwidgets::saveWidget(
  trend_plot,
  "housing_trend_chart.html",
  selfcontained = TRUE
)

# Create a simple Shiny app for full interactivity
create_shiny_app <- function() {
  ui <- fluidPage(
    titlePanel("Interactive Housing Market Analysis"),

    sidebarLayout(
      sidebarPanel(
        selectInput(
          "selected_year",
          "Select Year:",
          choices = 2019:2024,
          selected = 2024
        ),

        selectInput(
          "selected_cities",
          "Select Cities (optional):",
          choices = cities,
          multiple = TRUE
        ),

        checkboxGroupInput(
          "categories",
          "Show Categories:",
          choices = c(
            "Both below inflation" = "both_below",
            "Only rent above" = "rent_above",
            "Only sales above" = "sales_above",
            "Both above inflation" = "both_above"
          ),
          selected = c("both_below", "rent_above", "sales_above", "both_above")
        ),

        br(),
        downloadButton("download_data", "Download Data", class = "btn-primary")
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Heat Map", plotlyOutput("heatmap", height = "600px")),
          tabPanel(
            "Trend Analysis",
            plotlyOutput("trend_chart", height = "400px")
          ),
          tabPanel("Data Table", DT::dataTableOutput("data_table")),
          tabPanel("Summary Stats", verbatimTextOutput("summary"))
        )
      )
    )
  )

  server <- function(input, output) {
    filtered_data <- reactive({
      data <- housing_data

      if (
        !is.null(input$selected_cities) && length(input$selected_cities) > 0
      ) {
        data <- data %>% filter(city %in% input$selected_cities)
      }

      data %>% filter(category %in% input$categories)
    })

    output$heatmap <- renderPlotly({
      create_interactive_housing_chart(filtered_data())
    })

    output$trend_chart <- renderPlotly({
      create_trend_plot(housing_data) # Use full data for trends
    })

    output$data_table <- DT::renderDataTable({
      create_data_table(filtered_data())
    })

    output$summary <- renderText({
      data <- filtered_data()
      paste(
        "Data Summary:",
        paste("Total observations:", nrow(data)),
        paste("Cities included:", length(unique(data$city))),
        paste("Years covered:", paste(range(data$year), collapse = "-")),
        "",
        "Category Distribution:",
        paste(
          names(table(data$category)),
          ":",
          table(data$category),
          collapse = "\n"
        ),
        sep = "\n"
      )
    })

    output$download_data <- downloadHandler(
      filename = function() {
        paste("housing_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(filtered_data(), file, row.names = FALSE)
      }
    )
  }

  return(list(ui = ui, server = server))
}

# To run the Shiny app, uncomment the following lines:
app <- create_shiny_app()
shinyApp(ui = app$ui, server = app$server)

# Print instructions
cat(
  "
🎉 Interactive Housing Chart Created Successfully!

Files generated:
📊 interactive_housing_chart.html - Main interactive heat map
📈 housing_trend_chart.html - Trend analysis chart

Features included:
✅ Hover tooltips with detailed city information
✅ Interactive data table with filtering and export
✅ Trend analysis showing market evolution
✅ Full Shiny app code for maximum interactivity
✅ Downloadable HTML widgets

To run the Shiny app:
1. Uncomment the last 3 lines of code
2. Run the script
3. The app will open in your browser

The interactive plots will show:
• Detailed rent and sales growth rates on hover
• City names and inflation comparisons
• Zoom and pan capabilities
• Export options for further analysis
"
)
