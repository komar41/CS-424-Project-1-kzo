# Subway
This project was intended for visualizing the trends in ridership data in three particular Chicago CTA stations: Ohare, Halsted and Sox 35th Dan Ryan. The project covers data from 2001 to 2021, providing insights into ridership patterns and notable events affecting these stations.

## Project Access

- **Demo URL:** [https://ktqjmz-kazi0shahrukh-omar.shinyapps.io/cs424-project1-kzo/](https://ktqjmz-kazi0shahrukh-omar.shinyapps.io/cs424-project1-kzo/)
- **Introduction video:** [https://youtu.be/crEsxcrTsXU](https://youtu.be/crEsxcrTsXU)
- **GitHub repo:** [https://github.com/komar41/CS-424-Project-1-kzo](https://github.com/komar41/CS-424-Project-1-kzo)
- **Tools used:** R and Shiny

## User Interface

### Overview Page

<p>
  <img src="https://komar41.github.io/assets/img/projects/subway/overview/Overview%201.png" alt="Overview page">
</p>

The overview page provides a general view of ridership data:
- Select a particular station
- View yearly bar chart on the left
- Choose between daily, monthly, or weekday charts on the right
- Select a specific year for the right-side chart
- Access raw data below each chart

### Comparison Page

<p align="center">
  <img src="comparison.png" alt="Comparison page" width="50%">
</p>

The comparison page allows users to compare ridership data between two stations:
- Select stations, chart types, and year filters for both sides
- View raw data below each chart

### Interesting Findings Page

<p align="center">
  <img src="findings.png" alt="Interesting findings page" width="50%">
</p>

This page showcases interesting insights from the ridership data, such as the impact of the COVID-19 outbreak in March 2020.

## About the Data

- **Data source:** Chicago Data Portal
- **File size:** 39MB
- **Content:** Ridership data for all CTA stations from 2001 to 2021
- **Day types:** W = Weekday, A = Saturday, U = Sunday/Holiday

<p align="center">
  <img src="data.png" alt="Sample data" width="50%">
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
