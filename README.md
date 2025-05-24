# DATA-VISUALIZATION-COVID-DASHBOARD-

🌍 COVID-19 Interactive Dashboard (R Shiny)
This project is an interactive COVID-19 dashboard built using R Shiny to visualize the spread, trends, and global impact of the COVID-19 pandemic. The dashboard allows users to explore data dynamically through multiple perspectives: global summaries, country-specific trends, US county-level maps, and animated global comparisons.

📊 Features
🌐 Global Overview
View total confirmed cases, deaths, recoveries, and trend charts over time with an interactive pyramid comparing global confirmed vs death counts.

🏳️ Country Analysis
Select up to two countries and compare their pandemic trends side-by-side — including confirmed cases, deaths, recovered numbers, and fatality rate.

🗺 USA County-Level Map
Interactive choropleth map using leaflet and tigris, displaying county-level confirmed case concentrations with zoom and tooltip support.

🌍 World Comparison (Animated Map)
Explore top N countries by confirmed, death, or recovered cases with animated time sliders and bubble size/color encodings via plotly.

🛠️ Technologies & Packages
Frontend & Server: shiny, shinydashboard

Data Manipulation: dplyr, readr, lubridate

Visualization: ggplot2, plotly, scales, RColorBrewer

Mapping: leaflet, sf, tigris

📁 Data Sources
This project uses multiple cleaned CSV datasets, including:

Global confirmed cases

Country-wise daily data

Worldometer statistics

USA county-level COVID-19 data

🧠 Key Insights Enabled
Comparative analysis between countries with similar populations (e.g., US vs Indonesia)

Time evolution of case/death metrics by region

Hotspot detection at county level in the US

Visual summary of pandemic dynamics over time

🚀 Getting Started
To run the COVID-19 Dashboard locally:

Download the ZIP file and extract all contents to a local folder.

Open RStudio, then open the app.R file in the root directory.

Install all required packages and start the app by clicking "Run App" in R Studio.
