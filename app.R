# Installing all necessary packages
install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", "plotly", "readr", "lubridate", "leaflet", "sf", "tigris", "RColorBrewer", "scales"))

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(lubridate)
library(leaflet)
library(sf)
library(tigris)
library(RColorBrewer)
library(scales)

# Loading datasets
read_date_csv <- function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  if ("Date" %in% names(df) && !inherits(df$Date, "Date")) {
    df$Date <- as.Date(df$Date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y"))
  }

  df
}

day_wise <- read_date_csv("day_wise.csv")
worldometer_data <- read_csv("worldometer_data.csv", show_col_types = FALSE)
country_latest <- read_csv("country_wise_latest.csv", show_col_types = FALSE)
full_grouped <- read_date_csv("full_grouped.csv")
covid_data <- read_date_csv("covid_19_clean_complete.csv")
usa_data <- read_csv("usa_county_wise.csv", show_col_types = FALSE)

# Prepare US county map data
options(tigris_use_cache = TRUE)

latest_usa <- usa_data %>%
  group_by(UID, FIPS, Admin2, Province_State) %>%
  summarise(Confirmed = max(Confirmed, na.rm = TRUE), .groups = "drop")

counties <- tigris::counties(cb = TRUE, class = "sf")

map_data <- counties %>%
  mutate(FIPS = as.numeric(GEOID)) %>%
  left_join(latest_usa, by = "FIPS")


map_data <- map_data %>% filter(!is.na(Confirmed))


# Defining UI
ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Global Overview", tabName = "global", icon = icon("globe")),
      menuItem("Country Analysis", tabName = "country", icon = icon("flag")),
      menuItem("USA View", tabName = "usa", icon = icon("map")),
      menuItem("World Comparison", tabName = "compare", icon = icon("globe-americas"))
    ),
    conditionalPanel(
      condition = "input.sidebar === 'compare'",
      selectInput("selected_metric", "Metric:",
                  choices = c("Confirmed", "Deaths", "Recovered")),
      sliderInput("top_n", "Top N Countries:", min = 5, max = 50, value = 20),
      sliderInput("map_date", "Date:",
                  min = min(covid_data$Date),
                  max = max(covid_data$Date),
                  value = as.Date("2020-06-01"),
                  timeFormat = "%Y-%m-%d",
                  animate = animationOptions(interval = 1000, loop = TRUE))
    ),
    conditionalPanel(
      condition = "input.sidebar === 'country'",
      selectInput("selected_country", "Select Country/Countries:",
                  choices = sort(unique(covid_data$`Country/Region`)),
                  selected = "Ghana",
                  multiple = TRUE),
      
      dateRangeInput("date_range", "Select Date Range:",
                     start = min(covid_data$Date),
                     end = max(covid_data$Date))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "global",
              fluidRow(
                valueBoxOutput("total_cases"),
                valueBoxOutput("total_deaths"),
                valueBoxOutput("total_recovered")
              ),
              fluidRow(
                box(title = "Global Cases Over Time", width = 12, plotlyOutput("global_trend", height = 300))
              ),
              
              fluidRow(
                box(
                  width = 12,
                  HTML('<h4 style="text-align:center;">
                       <span style="color:#82CAFF;">Confirmed Cases</span> and 
                       <span style="color:#800000;">Death Cases</span> Pyramid
                     </h4>'),
                  plotlyOutput("global_pyramid", height = 250)
                )
              )
              
              
      ),
      
      
      tabItem(tabName = "country",
              fluidRow(
                box(title = "Country Trend", width = 12, plotlyOutput("country_plot"))
              ),
              fluidRow(
                uiOutput("country_summary_boxes")
              )
      ),


      tabItem(tabName = "usa",
              fluidRow(
                box(title = "USA County-level Map", width = 12, leafletOutput("us_map", height = 300))
              ),
              fluidRow(
                column(6,
                       box(title = NULL,
                           width = 12,
                           HTML('<h4 style="text-align:center;">US 
                                 <span style="color:#b22222;">Cumulative</span> vs 
                                 <span style="color:#1e90ff;">Daily</span> Cases Curve
                              </h4>'),
                           plotlyOutput("usa_cases_curve", height = 300))
                ),
                column(6,
                       box(title = NULL,
                           width = 12,
                           HTML('<h4 style="text-align:center;">US 
                                 <span style="color:#b22222;">Cumulative</span> vs 
                                 <span style="color:#1e90ff;">Daily</span> Deaths Curve
                              </h4>'),
                           plotlyOutput("usa_deaths_curve", height = 300))
                )
              )
      ),

      tabItem(tabName = "compare",
              fluidRow(
                box(title = "COVID-19 Cases Worldwide (Excluding China)",
                    width = 12,
                    plotlyOutput("world_bubble_map", height = 600))
              )
      )
    )
  )
)

# Setting up the Server
server <- function(input, output, session) {

  filtered_data <- reactive({
    req(input$selected_country)
    covid_data %>%
      filter(`Country/Region` %in% input$selected_country,
             Date >= input$date_range[1],
             Date <= input$date_range[2])
  })
  

  output$total_cases <- renderValueBox({
    total <- sum(country_latest$Confirmed, na.rm = TRUE)
    valueBox(format(total, big.mark = ","), "Total Confirmed Cases", icon = icon("users"), color = "blue")
  })

  output$total_deaths <- renderValueBox({
    total <- sum(country_latest$Deaths, na.rm = TRUE)
    valueBox(format(total, big.mark = ","), "Total Deaths", icon = icon("skull"), color = "red")
  })

  output$total_recovered <- renderValueBox({
    total <- sum(country_latest$Recovered, na.rm = TRUE)
    valueBox(format(total, big.mark = ","), "Total Recovered", icon = icon("heartbeat"), color = "green")
  })

  output$global_trend <- renderPlotly({
    p <- ggplot(day_wise, aes(x = Date, y = Confirmed)) +
      geom_line(color = "steelblue") +
      labs(title = "Global Confirmed Cases Over Time", y = "Confirmed Cases")
    ggplotly(p)
  })
  
  output$global_pyramid <- renderPlotly({
    data <- country_latest %>%
      summarise(
        Confirmed = sum(Confirmed, na.rm = TRUE),
        Deaths = sum(Deaths, na.rm = TRUE)
      )
    
    df <- tibble(
      Category = c("Deaths", "Confirmed"),
      Value = c(-data$Deaths, data$Confirmed)
    )
    
    p <- ggplot(df, aes(x = Value, y = "", fill = Category)) +
      geom_bar(stat = "identity", width = 0.4, color = "black") +
      scale_x_continuous(
        labels = scales::comma,
        breaks = pretty(c(-data$Deaths, data$Confirmed), n = 5)
      ) +
      geom_text(aes(label = scales::comma(abs(Value)), hjust = ifelse(Value > 0, -0.1, 1.1)),
                color = "#800000", size = 4) +
      scale_fill_manual(values = c("Deaths" = "#2f2f4f", "Confirmed" = "#3cb3b3")) +
      labs(title = "Confirmed Cases and Death Cases Pyramid") +
      theme_minimal(base_size = 14) +
      theme(
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "#fff8f0", color = NA),
        panel.background = element_rect(fill = "#fff8f0"),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    ggplotly(p)
  })
  
  
  output$country_plot <- renderPlotly({
    df <- filtered_data()
    
    p <- ggplot(df, aes(x = Date, y = Confirmed, color = `Country/Region`)) +
      geom_line(size = 1.2) +
      labs(title = "Confirmed Cases Comparison", y = "Confirmed Cases") +
      theme_minimal()
    
    ggplotly(p)
  })
  


  output$country_summary_boxes <- renderUI({
    df <- filtered_data()
    countries <- input$selected_country
    report_range <- paste(format(min(df$Date), "%d/%m/%y"), "-", format(max(df$Date), "%d/%m/%y"))
    
    base_style <- "padding: 15px; text-align: center; background-color: #fdf6f0;"
    confirm_style <- paste(base_style, "border: 3px solid #800000;")
    death_style <- paste(base_style, "border: 3px solid #800000;")
    recover_style <- paste(base_style, "border: 3px solid #2e8b57;")
    fatality_style_base <- "border: 3px solid #800000;"
    
    tagList(
      tags$head(tags$style(HTML("
      .country-box-container {
        display: flex;
        flex-wrap: wrap;
        justify-content: space-around;
        gap: 15px;
        margin-bottom: 15px;
      }
      .country-box {
        min-width: 280px;
        flex: 1 1 45%;
      }
    "))),
      tags$div(class = "country-box-container",
               lapply(countries, function(country) {
                 cdf <- df %>% filter(`Country/Region` == country)
                 latest <- cdf %>% filter(Date == max(Date, na.rm = TRUE))
                 
                 confirmed <- formatC(latest$Confirmed, format = "d", big.mark = ",")
                 deaths <- formatC(latest$Deaths, format = "d", big.mark = ",")
                 recovered <- formatC(latest$Recovered, format = "d", big.mark = ",")
                 fatality_rate_raw <- if (nrow(latest) > 0 && latest$Confirmed[1] > 0) {
                   round((latest$Deaths[1] / latest$Confirmed[1]) * 100, 1)
                 } else {
                   NA
                 }
                 fatality_rate <- ifelse(is.na(fatality_rate_raw), "N/A", paste0(fatality_rate_raw, "%"))
                 fatality_style <- ifelse(!is.na(fatality_rate_raw) && fatality_rate_raw > 5,
                                          paste(base_style, "border: 3px solid #b22222; background-color: #ffecec;"),
                                          paste(base_style, fatality_style_base))
                 
                 tags$div(class = "country-box",
                          tags$h4(style = "text-align:center; color:#800000;", country),
                          tags$div(class = "summary-container",
                                   tags$div(class = "summary-box", style = confirm_style,
                                            h3(style = "font-weight: bold;", confirmed),
                                            p(style = "color: #800000;", paste("Confirmed (", report_range, ")", sep = ""))
                                   ),
                                   tags$div(class = "summary-box", style = death_style,
                                            h3(style = "font-weight: bold;", deaths),
                                            p(style = "color: #800000;", paste("Deaths (", report_range, ")", sep = ""))
                                   ),
                                   tags$div(class = "summary-box", style = recover_style,
                                            h3(style = "font-weight: bold;", recovered),
                                            p(style = "color: #2e8b57;", paste("Recovered (", report_range, ")", sep = ""))
                                   ),
                                   tags$div(class = "summary-box", style = fatality_style,
                                            h3(style = "font-weight: bold;", fatality_rate),
                                            p(style = "color: #800000;", paste("Fatality Rate (", report_range, ")", sep = ""))
                                   )
                          )
                 )
               })
      )
    )
  })
  


  output$us_map <- renderLeaflet({
    bins <- c(1, 100, 1000, 5000, 10000, 50000, max(usa_data$Confirmed))
    pal <- colorBin(palette = "Reds", domain = map_data$Confirmed, bins = bins, na.color = "#cccccc")

    leaflet(map_data, options = leafletOptions(minZoom = 4, maxZoom = 10)) %>%
      setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(Confirmed),
        weight = 0.5,
        opacity = 1,
        color = "#333",
        fillOpacity = 0.9,
        highlight = highlightOptions(
          weight = 2,
          color = "#000",
          fillOpacity = 1,
          bringToFront = TRUE),
        label = ~paste0(Admin2, ", ", Province_State, ": ", formatC(Confirmed, big.mark = ","), " cases"),
        labelOptions = labelOptions(direction = "auto")
      ) %>%
      addLegend(pal = pal, values = ~Confirmed, opacity = 0.8,
                title = "Confirmed Cases",
                position = "bottomright")
  })

  usa_timeseries <- reactive({
    usa_data %>%
      mutate(Date = as.Date(Date, format="%m/%d/%y")) %>%  # 🛠 force Date conversion
      group_by(Date) %>%
      summarise(
        Total_Cases = sum(Confirmed, na.rm = TRUE),
        Total_Deaths = sum(Deaths, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Date) %>%
      mutate(
        Daily_Cases = c(NA, diff(Total_Cases)),
        Daily_Deaths = c(NA, diff(Total_Deaths))
      )
  })
  
  output$usa_cases_curve <- renderPlotly({
    df <- usa_timeseries()
    
    p <- ggplot(df, aes(x = as.Date(Date))) +
      geom_line(aes(y = Total_Cases, group = 1), color = "#b22222", size = 1.5) +
      geom_text(
        data = df %>% filter(!is.na(Total_Cases) & row_number() %% 10 == 0),
        aes(y = Total_Cases, label = scales::comma(Total_Cases)),
        color = "#b22222", size = 3, vjust = -0.8
      ) +
      geom_line(aes(y = Daily_Cases, group = 1), color = "#1e90ff", size = 1) +
      labs(title = "US Cumulative Cases & Daily Cases Curve", y = "Cases") +
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
      scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "#fff8f0", color = NA),
        panel.background = element_rect(fill = "#fff8f0"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()
      )
    
    ggplotly(p)
  })

  
  output$usa_deaths_curve <- renderPlotly({
    df <- usa_timeseries()
    
    p <- ggplot(df, aes(x = as.Date(Date))) +
      geom_line(aes(y = Total_Deaths, group = 1), color = "#b22222", size = 1.5) +
      geom_text(
        data = df %>% filter(!is.na(Total_Deaths) & row_number() %% 10 == 0),
        aes(y = Total_Deaths, label = scales::comma(Total_Deaths)),
        color = "#b22222", size = 3, vjust = -0.8
      ) +
      geom_line(aes(y = Daily_Deaths, group = 1), color = "#1e90ff", size = 1) +
      labs(title = "US Cumulative Deaths & Daily Deaths Curve", y = "Deaths") +
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
      scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "#fff8f0", color = NA),
        panel.background = element_rect(fill = "#fff8f0"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()
      )
    
    ggplotly(p)
  })
  

  output$world_bubble_map <- renderPlotly({
    metric <- input$selected_metric
    top_n <- input$top_n
    selected_date <- input$map_date

    df <- covid_data %>%
      filter(Date == selected_date, `Country/Region` != "China") %>%
      group_by(`Country/Region`) %>%
      summarise(
        Value = sum(.data[[metric]], na.rm = TRUE),
        Lat = mean(Lat, na.rm = TRUE),
        Long = mean(Long, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Value)) %>%
      slice_head(n = top_n) %>%
      mutate(text = paste0(`Country/Region`, "<br>", metric, ": ", Value))

    plot_ly(
      df,
      type = 'scattergeo',
      mode = 'markers',
      lat = ~Lat,
      lon = ~Long,
      marker = list(
        size = ~sqrt(Value) / 10,
        color = 'red',
        opacity = 0.6,
        line = list(width = 0)
      ),
      text = ~text,
      hoverinfo = 'text'
    ) %>%
      layout(
        title = paste("Top", top_n, metric, "Cases as of", selected_date),
        geo = list(
          projection = list(type = 'natural earth'),
          bgcolor = 'black',
          showland = TRUE,
          landcolor = 'rgb(30,30,30)',
          showocean = TRUE,
          oceancolor = 'rgb(10,10,10)',
          countrycolor = 'rgb(100,100,100)'
        ),
        paper_bgcolor = 'black',
        plot_bgcolor = 'black',
        font = list(color = 'white')
      )
  })

}

# Connecting the UI and the Server
shinyApp(ui = ui, server = server)
