# Subway
This project was intended for visualizing the trends in ridership data in three particular Chicago CTA stations: Ohare, Halsted and Sox 35th Dan Ryan. The project covers data from 2001 to 2021, providing insights into ridership patterns and notable events affecting these stations.

## Project Access

- **Demo URL:** [https://ktqjmz-kazi0shahrukh-omar.shinyapps.io/cs424-project1-kzo/](https://ktqjmz-kazi0shahrukh-omar.shinyapps.io/cs424-project1-kzo/)
- **Introduction video:** [https://youtu.be/crEsxcrTsXU](https://youtu.be/crEsxcrTsXU)
- **GitHub repo:** [https://github.com/komar41/CS-424-Project-1-kzo](https://github.com/komar41/CS-424-Project-1-kzo)
- **Tools used:** R and Shiny

## User Interface

### Overview Page
The overview page provides a general view of ridership data:
- Select a particular station
- View yearly bar chart on the left
- Choose between daily, monthly, or weekday charts on the right
- Select a specific year for the right-side chart
- Access raw data below each chart

<p>
  <img src="https://komar41.github.io/assets/img/projects/subway/overview/Overview%201.png" alt="Overview page">
</p>

### Comparison Page
The comparison page allows users to compare ridership data between two stations:
- Select stations, chart types, and year filters for both sides
- View raw data below each chart

<p>
  <img src="https://komar41.github.io/assets/img/projects/subway/overview/Overview%202.png" alt="Comparison page">
</p>


### Interesting Findings Page
This page showcases interesting insights from the ridership data, such as the impact of the COVID-19 outbreak in March 2020.

<p>
  <img src="https://komar41.github.io/assets/img/projects/subway/overview/Overview%203.png" alt="Interesting findings page">
</p>

## About the Data

- **Data source:** [Chicago Data Portal](https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f/about_data)
- **File size:** 39MB
- **Content:** Ridership data for all CTA stations from 2001 to 2021
- **Day types:** W = Weekday, A = Saturday, U = Sunday/Holiday

<p>
  <img src="https://komar41.github.io/assets/img/projects/subway/data/Data%201.png" alt="Sample data">
</p>

### Data Processing

The original dataset was filtered for the three stations using R:

```R
library(dplyr)

cta <- read.csv(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.tsv", sep = "\t", header = TRUE)

cta_halsted <- cta %>% filter(stationname == "UIC-Halsted")
write.table(cta_halsted, file = "cta_halsted.tsv", row.names=FALSE, sep="\t")

cta_ohare <- cta %>% filter(stationname == "O'Hare Airport")
write.table(cta_ohare, file = "cta_ohare.tsv", row.names=FALSE, sep="\t")

cta_sox <- cta %>% filter(stationname == "Sox-35th-Dan Ryan")
write.table(cta_sox, file = "cta_sox.tsv", row.names=FALSE, sep="\t")
