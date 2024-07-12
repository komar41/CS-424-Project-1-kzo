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

## Interesting Findings
**Findings 1:** Starting August 23, 2021 UIC reopened for in-person classes for the first time since Covid lockdown restrictions.

![image](https://github.com/user-attachments/assets/fe7e8fd8-12e1-4f13-8365-0bc0d94c526d)


**Findings 2:** Starting March 2020 CTA ridership declined due to the covid outbreak. However, throughout the year O'Hare was the busiest among the three stations due to being located at the O'Hare International Airport.

![image](https://github.com/user-attachments/assets/0bf1dbf4-4f51-4eb6-b7db-a49db49f0add)

**Findings 3:** On March 24, 2014 at 2:50 a.m. local time, a CTA passenger train overran the bumper at O'Hare, injuring 34 people. Following the accident, the line between O'Hare and Rosemont was closed, with a replacement bus service in place.

![image](https://github.com/user-attachments/assets/d7404971-a4a2-4780-a673-049bf8911d16)

**Findings 4:** In July 2008, service was suspended on the Blue Line for approximately 3 weeks between the O’Hare and Rosemont stations for construction.

![image](https://github.com/user-attachments/assets/3f5aaef0-f2c8-44b0-b1f4-fc2c64e37df3)

**Findings 5:** The coldest temperature in Chicago in 34 years (-23°) was recorded on the morning of January 30, 2019 during a bitter cold couple of days. Presumably, UIC remained closed and thus the drop in CTA ridership.

![image](https://github.com/user-attachments/assets/c28396b0-69ea-4b7d-be22-726e40fc9af2)

**Findings 6:** During each year, the lowest ridership recorded at Halsted station is 25th December. (With exception of polar vortex on January 30, 2019 and Covid outbreak in 2020 and 2021)

![image](https://github.com/user-attachments/assets/aabc295b-e084-45c1-93d8-75d596914f5c)

**Findings 7:** O'Hare station was temporarily closed from Sep 28, 2019 to Oct 6, 2019 due to construction (signal improvements).

![image](https://github.com/user-attachments/assets/18e66da1-ddd4-49e3-afbf-1c522da6e04c)

**Findings 8:** On September 24, 2016, Chicago White Sox stadium had a record attendance of 47,754 hosting a concert of Chance the Rapper.

![image](https://github.com/user-attachments/assets/d2de204c-5c0d-407d-ba59-085ded670d0a)

**Findings 9:** October 22, 2005: The first-ever World Series game in Chicago White Sox stadium. The White Sox get their first World Series game victory since 1959, defeating the Houston Astros 5–3. Attendance: (41,206)
October 23, 2005: The stadium hosted Game 2 of the World Series. The Sox won 7–6. Additionally, The Sox would win the next two games in Houston to win their first World Series title since 1917. Attendance: (41,432)

![image](https://github.com/user-attachments/assets/f2035084-810d-4b1f-ba18-81c5c8d85dbb)

**Findings 10:** October 10, 2021: Guaranteed Rate Field hosted its first playoff game since 2008 with the White Sox facing the Houston Astros in the ALDS with the Sox trailing 2 games to none. (Attendance: 40,288)
October 12, 2021: Guaranteed Rate Field hosted game 4 of the ALDS between the White Sox and Astros. The Astros won 10-1 and advanced to the ALCS. (Attendance: 40,170)

![image](https://github.com/user-attachments/assets/8b00e3f8-8cfd-4b3e-a209-568fbff40f24)

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
