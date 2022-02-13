
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#include necessary libraries
library(shiny)
library(ggplot2)
library(lubridate)
library(shinydashboard)
library(dplyr)
library(DT)
library( tidyverse )

#data cleaning section 
cta_sox <- read.csv(file = "cta_sox.tsv", sep = "\t", header = TRUE)
cta_sox$date <- mdy(cta_sox$date)
cta_sox <- cta_sox %>% arrange(cta_sox$date)

cta_halsted <- read.csv(file = "cta_halsted.tsv", sep = "\t", header = TRUE)
cta_halsted$date <- mdy(cta_halsted$date)
cta_halsted <- cta_halsted %>% arrange(cta_halsted$date)

cta_ohare <- read.csv(file = "cta_ohare.tsv", sep = "\t", header = TRUE)
cta_ohare$date <- mdy(cta_ohare$date)
cta_ohare <- cta_ohare %>% arrange(cta_ohare$date)

halsted_by_year <- cta_halsted %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarise(rides = sum(rides))

ohare_by_year <- cta_ohare %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarise(rides = sum(rides))

sox_by_year <- cta_sox %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarise(rides = sum(rides))

CSS <- "
.dataTables_wrapper .dataTables_paginate .paginate_button {
  min-width: 1em !important; 
  padding: 0.5em 0.5em !important;
} 
"


ui <- dashboardPage(
  
  
  
  #create dashboard and elements
  dashboardHeader(title = "CS 424 Project 1-kzo😎"),
  
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   #menu bar with all 3 panels and about page
                   sidebarMenu(
                     br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                     menuItem("Overview", tabName = "station", icon = NULL),
                     menuItem("Compare", tabName = "station_compare", icon = NULL),
                     menuItem("Interesting Findings", tabName = "findings", icon = NULL),
                     menuItem("About Page", tabName = "about", icon = NULL)
                   )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName="station",
              
              h1("Overview of Chicago CTA Data",align="center"),
              br(),br(),
              fluidPage(
                  tags$head(tags$style(HTML(CSS))),
                  fluidRow(
                    
                    column(2,
                           p("Choose a station"),
                           selectInput("Station", NULL,
                                       choices = c("Halsted", "O'Hare", "Sox-35th"), 
                                       selected = "Halsted"
                           )
                    ),
                    
                    column(2,offset=4,
                           p("Choose chart type"),
                           selectInput("Chart", NULL,
                                choices = c("Daily", "Monthly", "Weekdays"), 
                                selected = "Daily"
                       )
                    ),
                    column(2,
                           p("Choose year"),
                           selectInput("Year", NULL,
                                       choices=c(2001:2021), 
                                       selected = 2021
                           )
                    ),

                    column(6,
                           box(solidHeader = TRUE, status = "primary", width=200,
                               plotOutput("Station_Yearly_Bar")
                           )
                    ),
                    column(6, 
                           box(solidHeader = TRUE, status = "primary", width=200,
                               plotOutput("Station_Bar")
                           )
                    ),
                    column(6,
                           box(solidHeader = TRUE, status = "primary", width = 200,
                               dataTableOutput("Station_Yearly_Raw")
                           )
                           
                    ),
                   
                    column(6,
                           box(solidHeader = TRUE, status = "primary", width = 200,
                               dataTableOutput("Station_Raw")
                           )
                    )
                  )
              )
              
      ),
      
      #Create resource comparison per year tab
      tabItem(tabName="station_compare",
              
              h1("Station to Station Compare",align="center"),
              br(),br(),
              fluidPage(
                
                fluidRow(
                  column(2,
                         p("Choose station 1: "),
                         selectInput("Station1", NULL,
                                     choices = c("Halsted", "O'Hare", "Sox-35th"),
                                     selected = "Halsted"
                         )
                  ),
                  column(2,offset=1,
                         column(6,
                                p("Choose chart type"),
                                selectInput("Chart1", NULL,
                                            choices = c("Daily", "Monthly", "Weekdays"),
                                            selected = "Daily"
                                )
                                ),
                         column(6,
                               p("Choose year"),
                               selectInput("Year1", NULL,
                                           choices=c(2001:2021),
                                           selected = 2021
                               )
                               )
                         ),
                  column(2, offset=1,
                         p("Choose station 2: "),
                         selectInput("Station2", NULL,
                                     choices = c("Halsted", "O'Hare", "Sox-35th"),
                                     selected = "O'Hare"
                         )
                  ),
                  column(2,offset=1,
                         column(6,
                                p("Choose chart type"),
                                selectInput("Chart2", NULL,
                                            choices = c("Daily", "Monthly", "Weekdays"),
                                            selected = "Daily"
                                )
                         ),
                         column(6,
                                p("Choose year"),
                                selectInput("Year2", NULL,
                                            choices=c(2001:2021),
                                            selected = 2021
                                )
                         )
                  )
              ),
                
              fluidRow(
                column(3,
                       box(solidHeader = TRUE, status = "primary", width=200,
                           plotOutput("Station1_Yearly_Bar")
                       )
                ),
                
                column(3,
                       box(solidHeader = TRUE, status = "primary", width=200,
                           plotOutput("Station1_Bar")
                       )
                ),

                
                column(3,
                       box(solidHeader = TRUE, status = "primary", width=200,
                           plotOutput("Station2_Yearly_Bar")
                       )
                ),
                column(3,
                       box(solidHeader = TRUE, status = "primary", width=200,
                           plotOutput("Station2_Bar")
                       )
                ),
                
                column(3,
                       box(solidHeader = TRUE, status = "primary", width = 200,
                           dataTableOutput("Station1_Yearly_Raw")
                       )
                       
                ),
                column(3,
                       
                       box(solidHeader = TRUE, status = "primary", width = 200,
                           dataTableOutput("Station1_Raw")
                       )
                       
                ),
                column(3,
                       box(solidHeader = TRUE, status = "primary", width = 200,
                           dataTableOutput("Station2_Yearly_Raw")
                       )

                ),
                column(3,
                       
                       box(solidHeader = TRUE, status = "primary", width = 200,
                           dataTableOutput("Station2_Raw")
                       )
                )
                
              ),

              )
      ),
      
      #About page
      tabItem(tabName="findings",
              h1("Interesting Findings",align="center"),
              br(),br(),
              fluidPage(
                tags$head(tags$style(HTML(CSS))),
                fluidRow(
                  
                  column(2,
                         p("Pick an option: "),
                         selectInput("Findings", NULL,
                                     choices = c("August 23, 2021", "March 2020", "March 24, 2014",
                                                 "July 2008", "January 30, 2019", "December 25", 
                                                 "Sep 28, 2019 to Oct 6, 2019","September 24, 2016", "October 22-23, 2005", 
                                                 "October 10 & 12, 2021"),
                                     selected = "August 23, 2021"
                         )
                  ),
                  column(2,
                  conditionalPanel(
                    condition = "input.Findings == 'December 25'",
                    
                           p("Choose Year: "),
                           selectInput("FindingsYear", NULL,
                                       choices = c(2001:2021),
                                       selected = "2018"
                        )
                     )
                  )
                ),
                fluidRow(
                         verbatimTextOutput("FindingsOut")
                ),
                fluidRow(
                  column(4,
                         plotOutput("Findings_Bar1")
                  ),
                  column(4,
                         plotOutput("Findings_Bar2")
                  ),
                  column(4,
                         plotOutput("Findings_Bar3")
                  )
                ),
                br(),br(),
                fluidRow(
                  column(4,
                         dataTableOutput("Findings_Raw1")
                  ),
                  column(4,
                         dataTableOutput("Findings_Raw2")
                  ),
                  column(4,
                         dataTableOutput("Findings_Raw3")
                  )
                )
              )
      ),
      
      #About page
      tabItem(tabName="about",
              h2("About Page"),
              verbatimTextOutput("AboutOut")
      )
      
    )
  )
)





# Define server logic required to draw a histogram
server <- function(input, output) {
  
  halsted_2021 <- cta_halsted %>% filter(year(cta_halsted$date) == 2021) #selction$data1 Placeholder Value

  selection <- reactiveValues(station='Halsted', chart='Daily', year=2021,
                                             data1=halsted_by_year, data2=halsted_2021, color="#0099f9") 
  
  observeEvent({
    input$Station
    input$Chart
    input$Year},
    
    {
      selection$year <- input$Year
      selection$chart <- input$Chart
      selection$station <- input$Station
      
      if(input$Station == "Halsted"){
        selection$data1 <- halsted_by_year
        selection$data2 <- cta_halsted
        selection$color <- "#0099f9"
      }
      else if(input$Station == "O'Hare"){
        selection$data1 <- ohare_by_year
        selection$data2 <- cta_ohare
        selection$color <- "#d45087"
      }
      else{
        selection$data1 <- sox_by_year
        selection$data2 <- cta_sox
        selection$color <- "#2FAE3E"
      }
      
      
      
      if(input$Chart == "Daily"){
        selection$data2 <- selection$data2 %>% filter(year(selection$data2$date) == input$Year)
      }
      else if(input$Chart == "Monthly"){
        selection$data2 <- selection$data2 %>% filter(year(selection$data2$date) == input$Year) %>%
          mutate(month = lubridate::month(date)) %>%
          group_by(month) %>%
          summarise(rides = sum(rides))
      }
      else{
        selection$data2 <- selection$data2 %>% filter(year(selection$data2$date) == input$Year) %>%
          mutate(weekdays = weekdays(date)) %>%
          group_by(weekdays) %>%
          summarise(rides = sum(rides))
        
        selection$data2$weekdays <- factor(selection$data2$weekdays, levels= c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        selection$data2 <- selection$data2[order(selection$data2$weekdays), ]
      }

      sub <- paste("CTA", selection$chart, "Data:", selection$year, sep=" ")
      
      ### Yearly Bar Chart ###
      output$Station_Yearly_Bar <- renderPlot({
        ggplot(selection$data1, aes(x = year, y = rides))+
          geom_col(width = 0.7, fill=selection$color) + 
          labs(title=selection$station, 
               subtitle="CTA Yearly Data (2001 to 2021)")+
          theme(axis.text.x = element_text(angle=65, vjust=0.6))+
          scale_y_continuous(labels = scales::comma)
      })
      
      ### Daily Bar Chart ###
      if(selection$chart=='Daily'){
        
        datebreaks <- seq(as.Date(min(selection$data2$date)), as.Date(max(selection$data2$date)), by="2 month")
        #print(max(selection$data2$date))
        
        output$Station_Bar <- renderPlot({
          ggplot(selection$data2, aes(date, rides)) +
            geom_col(width = 0.5, fill=selection$color) +
            labs(title=selection$station, 
                 subtitle=sub,
                 x = "date", y = "rides")+
            scale_x_date(date_labels="%B",
                         breaks = datebreaks,
                         limits = c( as.Date(min(selection$data2$date)), as.Date(max(selection$data2$date))) )+
            scale_y_continuous(labels = scales::comma)
        })
      }
      ### Monthly Bar Chart ###
      else if(selection$chart=='Monthly'){
        output$Station_Bar <- renderPlot({
          ggplot(selection$data2, aes(x = month.abb[month], y = rides))+
            geom_col(width = 0.5, fill=selection$color) + 
            labs(title=selection$station, 
                 subtitle=sub,
                 x='month')+
            theme(axis.text.x = element_text(vjust=0.6))+
            scale_x_discrete(limits = month.abb)+
            scale_y_continuous(labels = scales::comma)
        })
      }
      ### Weekdays Bar Chart ###
      else{
        output$Station_Bar <- renderPlot({
          ggplot(selection$data2, aes(x = weekdays, y = rides))+
            geom_col(width = 0.5, fill=selection$color) + 
            labs(title=selection$station, 
                 subtitle=sub)+
            theme(axis.text.x = element_text(vjust=0.6))+
            scale_y_continuous(labels = scales::comma)
        })
      }
      
      ### Yearly Data Table ###
      output$Station_Yearly_Raw <- renderDataTable(
        datatable(selection$data1, 
                  options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15)
                  )) %>% 
          formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
          formatRound('rides', digits = 0)
      )
      
      ### Daily Data Table ###
      if(selection$chart=='Daily'){
        date_data <- selection$data2
        date_data$date <-format(date_data$date, format="%m/%d")
        output$Station_Raw <- renderDataTable(
          datatable(date_data[c(-1,-2,-4)], 
                    options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15, 20, 25, 30)
                    )) %>% 
          formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
          formatRound('rides', digits = 0)
        )
      }
      ### Monthly Data Table ###
      else if(selection$chart=='Monthly'){
        monthly_data = data.frame(month=month.name[selection$data2$month],
                         rides=selection$data2$rides)
        output$Station_Raw <- renderDataTable(
          datatable(monthly_data, 
                    options = list(searching = FALSE,pageLength = 12, lengthChange=FALSE
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
      }
      ### Weekdays Data Table ###
      else{
        output$Station_Raw <- renderDataTable(
          datatable(selection$data2, 
                    options = list(searching = FALSE,pageLength = 7, lengthChange=FALSE
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
      }
      
    })
  
  
  #"#0099f9", "#d45087", "#2FAE3E"
  halsted_2021 <- cta_halsted %>% filter(year(cta_halsted$date) == 2021) #selction_c$data1_c Placeholder Value
  ohare_2021 <- cta_ohare %>% filter(year(cta_ohare$date) == 2021) #selction_c$data2_c Placeholder Value
  selection_c <- reactiveValues(station1='Halsted', chart1='Daily', year1=2021,
                                station2="O'Hare", chart2='Daily', year2=2021,
                                data1_1=halsted_by_year, data1_2=halsted_2021,
                                data2_1=ohare_by_year, data2_2=ohare_2021,
                                color1="#0099f9", color2="#d45087") 
  
  observeEvent({
    input$Station1
    input$Chart1
    input$Year1
    input$Station2
    input$Chart2
    input$Year2},
    
    {
      selection_c$year1 <- input$Year1
      selection_c$chart1 <- input$Chart1
      selection_c$station1 <- input$Station1
      
      if(input$Station1 == "Halsted"){
        selection_c$data1_1 <- halsted_by_year
        selection_c$data1_2 <- cta_halsted
        selection_c$color1 <- "#0099f9"
      }
      else if(input$Station1 == "O'Hare"){
        selection_c$data1_1 <- ohare_by_year
        selection_c$data1_2 <- cta_ohare
        selection_c$color1 <- "#d45087"
      }
      else{
        selection_c$data1_1 <- sox_by_year
        selection_c$data1_2 <- cta_sox
        selection_c$color1 <- "#2FAE3E"
      }
      
      if(input$Chart1 == "Daily"){
        selection_c$data1_2 <- selection_c$data1_2 %>% filter(year(selection_c$data1_2$date) == input$Year1)
      }
      else if(input$Chart1 == "Monthly"){
        selection_c$data1_2 <- selection_c$data1_2 %>% filter(year(selection_c$data1_2$date) == input$Year1) %>%
          mutate(month = lubridate::month(date)) %>%
          group_by(month) %>%
          summarise(rides = sum(rides))
      }
      else{
        selection_c$data1_2 <- selection_c$data1_2 %>% filter(year(selection_c$data1_2$date) == input$Year1) %>%
          mutate(weekdays = weekdays(date)) %>%
          group_by(weekdays) %>%
          summarise(rides = sum(rides))
        
        selection_c$data1_2$weekdays <- factor(selection_c$data1_2$weekdays, levels= c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        selection_c$data1_2 <- selection_c$data1_2[order(selection_c$data1_2$weekdays), ]
      }
      
      #"#0099f9", "#d45087", "#2FAE3E"
      
      selection_c$year2 <- input$Year2
      selection_c$chart2 <- input$Chart2
      selection_c$station2 <- input$Station2
      
      
      if(input$Station2 == "Halsted"){
        selection_c$data2_1 <- halsted_by_year
        selection_c$data2_2 <- cta_halsted
        selection_c$color2 <- "#0099f9"
      }
      else if(input$Station2 == "O'Hare"){
        selection_c$data2_1 <- ohare_by_year
        selection_c$data2_2 <- cta_ohare
        selection_c$color2 <- "#d45087"
      }
      else{
        selection_c$data2_1 <- sox_by_year
        selection_c$data2_2 <- cta_sox
        selection_c$color2 <- "#2FAE3E"
      }
      
      if(input$Chart2 == "Daily"){
        selection_c$data2_2 <- selection_c$data2_2 %>% filter(year(selection_c$data2_2$date) == input$Year2)
      }
      else if(input$Chart2 == "Monthly"){
        selection_c$data2_2 <- selection_c$data2_2 %>% filter(year(selection_c$data2_2$date) == input$Year2) %>%
          mutate(month = lubridate::month(date)) %>%
          group_by(month) %>%
          summarise(rides = sum(rides))
      }
      else{
        selection_c$data2_2 <- selection_c$data2_2 %>% filter(year(selection_c$data2_2$date) == input$Year2) %>%
          mutate(weekdays = weekdays(date)) %>%
          group_by(weekdays) %>%
          summarise(rides = sum(rides))
        
        selection_c$data2_2$weekdays <- factor(selection_c$data2_2$weekdays, levels= c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        selection_c$data2_2 <- selection_c$data2_2[order(selection_c$data2_2$weekdays), ]
      }
      
      # max_y1_year = max(selection_c$data1_1$rides)
      # max_y2_year = max(selection_c$data2_1$rides)
      # 
      # print(max_y1_year)
      # print(max_y2_year)

      sub1 <- paste("CTA", selection_c$chart1, "Data:", selection_c$year1, sep=" ")
      
      ### Yearly Bar Chart 1 ###
      output$Station1_Yearly_Bar <- renderPlot({
        ggplot(selection_c$data1_1, aes(x = year, y = rides))+
          geom_col(width = 0.7, fill=selection_c$color1) + 
          labs(title=selection_c$station1, 
               subtitle="CTA Yearly Data (2001 to 2021)")+
          theme(axis.text.x = element_text(angle=65, vjust=0.6))+
          scale_y_continuous(labels = scales::comma)
      })
      
      
      
      ### Daily Bar Chart 1 ###
      if(selection_c$chart1=='Daily'){
        datebreaks1 <- seq(as.Date(min(selection_c$data1_2$date)), as.Date(max(selection_c$data1_2$date)), by="2 month")
        
        output$Station1_Bar <- renderPlot({
          ggplot(selection_c$data1_2, aes(date, rides)) +
            geom_col(width = 0.5, fill=selection_c$color1) +
            labs(title=selection_c$station1, 
                 subtitle=sub1,
                 x = "date", y = "rides")+
            scale_x_date(date_labels="%B",
                         breaks = datebreaks1,
                         limits = c( as.Date(min(selection_c$data1_2$date)), as.Date(max(selection_c$data1_2$date))))+
            scale_y_continuous(labels = scales::comma)
        })
      }
      ### Monthly Bar Chart 1 ###
      else if(selection_c$chart1=='Monthly'){
        output$Station1_Bar <- renderPlot({
          ggplot(selection_c$data1_2, aes(x = month.abb[month], y = rides))+
            geom_col(width = 0.5, fill=selection_c$color1) + 
            labs(title=selection_c$station1, 
                 subtitle=sub1,
                 x='month')+
            theme(axis.text.x = element_text(vjust=0.6))+
            scale_x_discrete(limits = month.abb)+
            scale_y_continuous(labels = scales::comma)
        })
      }
      ### Weekdays Bar Chart 1 ###
      else{
        output$Station1_Bar <- renderPlot({
          ggplot(selection_c$data1_2, aes(x = weekdays, y = rides))+
            geom_col(width = 0.5, fill=selection_c$color1) + 
            labs(title=selection_c$station1, 
                 subtitle=sub1)+
            theme(axis.text.x = element_text(vjust=0.6))+
            scale_y_continuous(labels = scales::comma)
        })
      }
      
      ### Yearly Data Table 1 ###
      output$Station1_Yearly_Raw <- renderDataTable(
        datatable(selection_c$data1_1, 
                  options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15)
                  )) %>% 
          formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
          formatRound('rides', digits = 0)
      )
      
      ### Daily Data Table 1 ###
      if(selection_c$chart1=='Daily'){
        date_data1_2 <- selection_c$data1_2
        date_data1_2$date <-format(date_data1_2$date, format="%m/%d")
        
        output$Station1_Raw <- renderDataTable(
          datatable(date_data1_2[c(-1,-2,-4)], 
                    options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15, 20, 25, 30)
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
      }
      ### Monthly Data Table 1 ###
      else if(selection_c$chart1=='Monthly'){
        monthly1_data = data.frame(month=month.name[selection_c$data1_2$month],
                                   rides=selection_c$data1_2$rides)
        output$Station1_Raw <- renderDataTable(
          datatable(monthly1_data, 
                    options = list(searching = FALSE,pageLength = 12, lengthChange=FALSE
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
      }
      ### Weekdays Data Table 1 ###
      else{
        output$Station1_Raw <- renderDataTable(
          datatable(selection_c$data1_2, 
                    options = list(searching = FALSE,pageLength = 7, lengthChange=FALSE
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
      }
      
      sub2 <- paste("CTA", selection_c$chart2, "Data:", selection_c$year2, sep=" ")
      
      ### Yearly Bar Chart 2 ###
      output$Station2_Yearly_Bar <- renderPlot({
        ggplot(selection_c$data2_1, aes(x = year, y = rides))+
          geom_col(width = 0.7, fill=selection_c$color2) + 
          labs(title=selection_c$station2, 
               subtitle="CTA Yearly Data (2001 to 2021)")+
          theme(axis.text.x = element_text(angle=65, vjust=0.6))+
          scale_y_continuous(labels = scales::comma)
      })
      
      ### Daily Bar Chart 2 ###
      if(selection_c$chart2=='Daily'){
        #print(max(selection_c$data2_2$date))
        datebreaks2 <- seq(as.Date(min(selection_c$data2_2$date)), as.Date(max(selection_c$data2_2$date)), by="2 month")
        
        output$Station2_Bar <- renderPlot({
          ggplot(selection_c$data2_2, aes(date, rides)) +
            geom_col(width = 0.5, fill=selection_c$color2) +
            labs(title=selection_c$station2, 
                 subtitle=sub2,
                 x = "date", y = "rides")+
            scale_x_date(date_labels="%B",
                         breaks = datebreaks2,
                         limits = c( as.Date(min(selection_c$data2_2$date)), as.Date(max(selection_c$data2_2$date))) )+
            scale_y_continuous(labels = scales::comma)
        })
      }
      ### Monthly Bar Chart 2 ###
      else if(selection_c$chart2=='Monthly'){
        output$Station2_Bar <- renderPlot({
          ggplot(selection_c$data2_2, aes(x = month.abb[month], y = rides))+
            geom_col(width = 0.5, fill=selection_c$color2) + 
            labs(title=selection_c$station2, 
                 subtitle=sub2,
                 x='month')+
            theme(axis.text.x = element_text(vjust=0.6))+
            scale_x_discrete(limits = month.abb)+
            scale_y_continuous(labels = scales::comma)
        })
      }
      ### Weekdays Bar Chart 2 ###
      else{
        output$Station2_Bar <- renderPlot({
          ggplot(selection_c$data2_2, aes(x = weekdays, y = rides))+
            geom_col(width = 0.5, fill=selection_c$color2) + 
            labs(title=selection_c$station2, 
                 subtitle=sub2)+
            theme(axis.text.x = element_text(vjust=0.6))+
            scale_y_continuous(labels = scales::comma)
        })
      }
      
      ### Yearly Data Table 2 ###
      output$Station2_Yearly_Raw <- renderDataTable(
        datatable(selection_c$data2_1, 
                  options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15)
                  )) %>% 
          formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
          formatRound('rides', digits = 0)
      )
      
      ### Daily Data Table 2 ###
      if(selection_c$chart2=='Daily'){
        date_data2_2 <- selection_c$data2_2
        date_data2_2$date <-format(date_data2_2$date, format="%m/%d")
        
        output$Station2_Raw <- renderDataTable(
          datatable(date_data2_2[c(-1,-2,-4)], 
                    options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15, 20, 25, 30)
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
      }
      ### Monthly Data Table 2 ###
      else if(selection_c$chart2=='Monthly'){
        monthly2_data = data.frame(month=month.name[selection_c$data2_2$month],
                                   rides=selection_c$data2_2$rides)
        output$Station2_Raw <- renderDataTable(
          datatable(monthly2_data, 
                    options = list(searching = FALSE,pageLength = 12, lengthChange=FALSE
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
      }
      ### Weekdays Data Table 2 ###
      else{
        output$Station2_Raw <- renderDataTable(
          datatable(selection_c$data2_2, 
                    options = list(searching = FALSE,pageLength = 7, lengthChange=FALSE
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
      }
      
    })
  
  observeEvent(
    {input$Findings
     input$FindingsYear},
    
    {
      # "August 23, 2021", "March 2020", "March 24, 2014",
      # "July 2008", "January 30, 2019", "December 25", 
      # "Sep 27, 2019 to Oct 7, 2019","September 24, 2016", "October 22-23, 2005", 
      # "October 10, 2021"
      # Findings_Bar1, Findings_Bar2, Findings_Bar3
      # Findings_Raw1, Findings_Raw2, Findings_Raw3
      
      if(input$Findings == "August 23, 2021"){
        halsted_2021 <- cta_halsted %>% filter(year(cta_halsted$date) == 2021)
        halsted_2021 <- halsted_2021 %>% mutate( ToHighlight = ifelse( date == ymd('2021-08-23'), "yes", "no" ) )
        datebreaks <- seq(as.Date(min(halsted_2021$date)), as.Date(max(halsted_2021$date)), by="2 month")
        
        
        output$Findings_Bar1 <- renderPlot({
          ggplot(halsted_2021, aes(date, rides, fill=ToHighlight)) +
            geom_col(width = 0.5) +
            labs(title="UIC Halsted ", 
                 subtitle="CTA Daily Data 2021",
                 x = "date", y = "rides")+
            scale_x_date(date_labels="%B",
                         breaks = datebreaks,
                         limits = c( as.Date(min(halsted_2021$date)), as.Date(max(halsted_2021$date))))+
            scale_fill_manual( values = c( "yes"="#FB4D3D", "no"="#0099f9" ), guide = FALSE )+
            scale_y_continuous(labels = scales::comma)
        })
        
        output$Findings_Bar2 <- NULL
        output$Findings_Bar3 <- NULL
        
        halsted_2021_date_data <- halsted_2021
        halsted_2021_date_data$date <-format(halsted_2021_date_data$date, format="%m/%d")
        
        output$Findings_Raw1 <- renderDataTable(
          datatable(halsted_2021_date_data[c(-1,-2,-4,-6)], 
                    options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15, 20, 25, 30),
                                   displayStart=230
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
        
        output$Findings_Raw2 <- NULL
        output$Findings_Raw3 <- NULL
        
        output$FindingsOut <- renderText({
          "Findings: 
           \nStarting August 23, 2021 UIC reopened for in-person classes for the first time since Covid lockdown restrictions."   
        })
      }
      
      else if(input$Findings == "March 2020"){
        halsted_2020 <- cta_halsted %>% filter(year(cta_halsted$date) == 2020)
        datebreaks <- seq(as.Date(min(halsted_2020$date)), as.Date(max(halsted_2020$date)), by="2 month")
        
        halsted_2020_daily <- ggplot(halsted_2020, aes(date, rides)) +
          geom_col(width = 0.5, fill='#0099f9') +
          labs(title="UIC Halsted ", 
               subtitle="CTA Daily Data 2020",
               x = "date", y = "rides")+
          scale_x_date(date_labels="%B",
                       breaks = datebreaks,
                       limits = c( as.Date(min(halsted_2020$date)), as.Date(max(halsted_2020$date))))
        
        ohare_2020 <- cta_ohare %>% filter(year(cta_ohare$date) == 2020)
        datebreaks <- seq(as.Date(min(ohare_2020$date)), as.Date(max(ohare_2020$date)), by="2 month")
        
        ohare_2020_daily <- ggplot(ohare_2020, aes(date, rides)) +
          geom_col(width = 0.5, fill='#d45087') +
          labs(title="O'Hare", 
               subtitle="CTA Daily Data 2020",
               x = "date", y = "rides")+
          scale_x_date(date_labels="%B",
                       breaks = datebreaks,
                       limits = c( as.Date(min(ohare_2020$date)), as.Date(max(ohare_2020$date))))
        
        sox_2020 <- cta_sox %>% filter(year(cta_sox$date) == 2020)
        datebreaks <- seq(as.Date(min(sox_2020$date)), as.Date(max(sox_2020$date)), by="2 month")
        
        sox_2020_daily <- ggplot(sox_2020, aes(date, rides)) +
          geom_col(width = 0.5, fill='#2FAE3E') +
          labs(title="Sox", 
               subtitle="CTA Daily Data 2020",
               x = "date", y = "rides")+
          scale_x_date(date_labels="%B",
                       breaks = datebreaks,
                       limits = c( as.Date(min(sox_2020$date)), as.Date(max(sox_2020$date))))
        
        sox_r <- layer_scales(sox_2020_daily)$y$range$range
        halsted_r <- layer_scales(halsted_2020_daily)$y$range$range
        ohare_r <- layer_scales(ohare_2020_daily)$y$range$range
        
        max_e <- max(c(sox_r[2], halsted_r[2], ohare_r[2]))
        
        
        output$Findings_Bar1 <- renderPlot({
          halsted_2020_daily + ylim(0,max_e)
        })
        
        output$Findings_Bar2 <- renderPlot({
          ohare_2020_daily + ylim(0,max_e)
        })
        
        output$Findings_Bar3 <- renderPlot({
          sox_2020_daily + ylim(0,max_e)
        })
        
        halsted_2020_date_data <- halsted_2020
        halsted_2020_date_data$date <-format(halsted_2020_date_data$date, format="%m/%d")
        
        output$Findings_Raw1 <- renderDataTable(
          datatable(halsted_2020_date_data[c(-1,-2,-4,-6)], 
                    options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15, 20, 25, 30)
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
        
        ohare_2020_date_data <- ohare_2020
        ohare_2020_date_data$date <-format(ohare_2020_date_data$date, format="%m/%d")
        
        output$Findings_Raw2 <- renderDataTable(
          datatable(ohare_2020_date_data[c(-1,-2,-4,-6)], 
                    options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15, 20, 25, 30)
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
        
        sox_2020_date_data <- sox_2020
        sox_2020_date_data$date <-format(sox_2020_date_data$date, format="%m/%d")
        
        output$Findings_Raw3 <- renderDataTable(
          datatable(sox_2020_date_data[c(-1,-2,-4,-6)], 
                    options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15, 20, 25, 30)
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
        
        output$FindingsOut <- renderText({
          "Findings: 
           \nStarting March 2020 CTA ridership declined due to covid outbreak. 
           \nHowever, throughout the year O'Hare was the busiest among the three stations due to being located at the O'Hare International Airport."   
        })
      }
      
      
      else if(input$Findings == "March 24, 2014"){
        ohare_2014 <- cta_ohare %>% filter(year(cta_ohare$date) == 2014)
        datebreaks <- seq(as.Date(min(ohare_2014$date)), as.Date(max(ohare_2014$date)), by="2 month")
        
        
        output$Findings_Bar1 <- renderPlot({
          ggplot(ohare_2014, aes(date, rides)) +
            geom_col(width = 0.5, fill='#d45087') +
            labs(title="O'Hare", 
                 subtitle="CTA Daily Data 2014",
                 x = "date", y = "rides")+
            scale_x_date(date_labels="%B",
                         breaks = datebreaks,
                         limits = c( as.Date(min(ohare_2014$date)), as.Date(max(ohare_2014$date))))+
            scale_y_continuous(labels = scales::comma)
        })
        
        output$Findings_Bar2 <- NULL
        output$Findings_Bar3 <- NULL
        
        ohare_2014_date_data <- ohare_2014
        ohare_2014_date_data$date <-format(ohare_2014_date_data$date, format="%m/%d")
        
        output$Findings_Raw1 <- renderDataTable(
          datatable(ohare_2014_date_data[c(-1,-2,-4,-6)], 
                    options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15, 20, 25, 30),
                                   displayStart=80
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
        
        output$Findings_Raw2 <- NULL
        output$Findings_Raw3 <- NULL
        
        output$FindingsOut <- renderText({
          "Findings: 
           \nOn March 24, 2014 at 2:50 a.m. local time, a CTA passenger train overran the bumper at O'Hare, injuring 34 people. 
           \nFollowing the accident, the line between O'Hare and Rosemont was closed, with a replacement bus service in place."   
        })
        
      }
      
      else if(input$Findings == "July 2008"){
        ohare_2008 <- cta_ohare %>% filter(year(cta_ohare$date) == 2008)
        datebreaks <- seq(as.Date(min(ohare_2008$date)), as.Date(max(ohare_2008$date)), by="2 month")
        
        
        output$Findings_Bar1 <- renderPlot({
          ggplot(ohare_2008, aes(date, rides)) +
            geom_col(width = 0.5, fill='#d45087') +
            labs(title="O'Hare", 
                 subtitle="CTA Daily Data 2008",
                 x = "date", y = "rides")+
            scale_x_date(date_labels="%B",
                         breaks = datebreaks,
                         limits = c( as.Date(min(ohare_2008$date)), as.Date(max(ohare_2008$date))))+
            scale_y_continuous(labels = scales::comma)
        })
        
        output$Findings_Bar2 <- NULL
        output$Findings_Bar3 <- NULL
        
        ohare_2008_date_data <- ohare_2008
        ohare_2008_date_data$date <-format(ohare_2008_date_data$date, format="%m/%d")
        
        output$Findings_Raw1 <- renderDataTable(
          datatable(ohare_2008_date_data[c(-1,-2,-4,-6)], 
                    options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15, 20, 25, 30),
                                   displayStart=190
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
        
        output$Findings_Raw2 <- NULL
        output$Findings_Raw3 <- NULL
        
        output$FindingsOut <- renderText({
          "Findings: 
           \nIn July  2008, service was suspended on the Blue Line for approximately 3 weeks between the O’Hare  and  Rosemont stations for construction."   
        })
        
      }
      
      
      
      else if(input$Findings == "January 30, 2019"){
        halsted_2019 <- cta_halsted %>% filter(year(cta_halsted$date) == 2019)
        halsted_2019 <- halsted_2019 %>% mutate( ToHighlight = ifelse( date == ymd('2019-01-30'), "yes", "no" ) )
        datebreaks <- seq(as.Date(min(halsted_2019$date)), as.Date(max(halsted_2019$date)), by="2 month")
        
        
        output$Findings_Bar1 <- renderPlot({
            ggplot(halsted_2019, aes(date, rides, fill = ToHighlight)) +
            geom_col(width = 0.5) +
            labs(title="UIC Halsted ", 
                 subtitle="CTA Daily Data 2019",
                 x = "date", y = "rides")+
            scale_x_date(date_labels="%B",
                         breaks = datebreaks,
                         limits = c( as.Date(min(halsted_2019$date)), as.Date(max(halsted_2019$date))))+
            scale_fill_manual( values = c( "yes"="#FB4D3D", "no"="#0099f9" ), guide = FALSE )+
            scale_y_continuous(labels = scales::comma)
        })
        
        output$Findings_Bar2 <- NULL
        output$Findings_Bar3 <- NULL
        
        halsted_2019_date_data <- halsted_2019
        halsted_2019_date_data$date <-format(halsted_2019_date_data$date, format="%m/%d")
        
        output$Findings_Raw1 <- renderDataTable(
          datatable(halsted_2019_date_data[c(-1,-2,-4,-6)], 
                    options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15, 20, 25, 30),
                                   displayStart=25
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
        
        output$Findings_Raw2 <- NULL
        output$Findings_Raw3 <- NULL
        
        output$FindingsOut <- renderText({
          "Findings: 
           \nThe coldest temperature in Chicago in 34 years (-23°) was recorded on the morning of January 30, 2019 during a bitter cold couple of days. Presumably, UIC remained closed and thus the drop in CTA ridership."   
        })
        
      }
      
      else if(input$Findings == "Sep 28, 2019 to Oct 6, 2019"){
        ohare_2019 <- cta_ohare %>% filter(year(cta_ohare$date) == 2019)
        datebreaks <- seq(as.Date(min(ohare_2019$date)), as.Date(max(ohare_2019$date)), by="2 month")
        
        
        output$Findings_Bar1 <- renderPlot({
          ggplot(ohare_2019, aes(date, rides)) +
            geom_col(width = 0.5, fill='#d45087') +
            labs(title="O'Hare", 
                 subtitle="CTA Daily Data 2019",
                 x = "date", y = "rides")+
            scale_x_date(date_labels="%B",
                         breaks = datebreaks,
                         limits = c( as.Date(min(ohare_2019$date)), as.Date(max(ohare_2019$date))))+
            scale_y_continuous(labels = scales::comma)
        })
        
        output$Findings_Bar2 <- NULL
        output$Findings_Bar3 <- NULL
        
        ohare_2019_date_data <- ohare_2019
        ohare_2019_date_data$date <-format(ohare_2019_date_data$date, format="%m/%d")
        
        output$Findings_Raw1 <- renderDataTable(
          datatable(ohare_2019_date_data[c(-1,-2,-4,-6)], 
                    options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15, 20, 25, 30),
                                   displayStart=270
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
        
        output$Findings_Raw2 <- NULL
        output$Findings_Raw3 <- NULL
        
        output$FindingsOut <- renderText({
          "Findings: 
           \nO'Hare station was temporarily closed from Sep 28, 2019 to Oct 6, 2019 due to construction (signal improvements)."   
        })
        
      }
      
      else if(input$Findings == "September 24, 2016"){
        sox_2016 <- cta_sox %>% filter(year(cta_sox$date) == 2016)
        sox_2016 <- sox_2016 %>% mutate( ToHighlight = ifelse( date == ymd('2016-09-24'), "yes", "no" ) )
        datebreaks <- seq(as.Date(min(sox_2016$date)), as.Date(max(sox_2016$date)), by="2 month")
        
        
        output$Findings_Bar1 <- renderPlot({
          ggplot(sox_2016, aes(date, rides, fill = ToHighlight)) +
            geom_col(width = 0.5) +
            labs(title="Sox-35th", 
                 subtitle="CTA Daily Data 2016",
                 x = "date", y = "rides")+
            scale_x_date(date_labels="%B",
                         breaks = datebreaks,
                         limits = c( as.Date(min(sox_2016$date)), as.Date(max(sox_2016$date))))+
            scale_fill_manual( values = c( "yes"="#FB4D3D", "no"="#2FAE3E" ), guide = FALSE )+
            scale_y_continuous(labels = scales::comma)
        })
        
        output$Findings_Bar2 <- NULL
        output$Findings_Bar3 <- NULL
        
        sox_2016_date_data <- sox_2016
        sox_2016_date_data$date <-format(sox_2016_date_data$date, format="%m/%d")
        
        output$Findings_Raw1 <- renderDataTable(
          datatable(sox_2016_date_data[c(-1,-2,-4,-6)], 
                    options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15, 20, 25, 30),
                                   displayStart=260
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
        
        output$Findings_Raw2 <- NULL
        output$Findings_Raw3 <- NULL
        
        output$FindingsOut <- renderText({
          "Findings: 
           \nOn September 24, 2016, Chicago White Sox home stadium had record attendance of 47,754 hosting a concert of Chance the Rapper."   
        })
        
      }
      
      else if(input$Findings == "October 22-23, 2005"){
        sox_2005 <- cta_sox %>% filter(year(cta_sox$date) == 2005)
        sox_2005 <- sox_2005 %>% mutate( ToHighlight = ifelse( date == ymd('2005-10-22') | date == ymd('2005-10-23'), "yes", "no" ) )
        datebreaks <- seq(as.Date(min(sox_2005$date)), as.Date(max(sox_2005$date)), by="2 month")
        
        
        output$Findings_Bar1 <- renderPlot({
          ggplot(sox_2005, aes(date, rides, fill = ToHighlight)) +
            geom_col(width = 0.5) +
            labs(title="Sox-35th", 
                 subtitle="CTA Daily Data 2005",
                 x = "date", y = "rides")+
            scale_x_date(date_labels="%B",
                         breaks = datebreaks,
                         limits = c( as.Date(min(sox_2005$date)), as.Date(max(sox_2005$date))))+
            scale_fill_manual( values = c( "yes"="#FB4D3D", "no"="#2FAE3E" ), guide = FALSE )+
            scale_y_continuous(labels = scales::comma)
        })
        
        output$Findings_Bar2 <- NULL
        output$Findings_Bar3 <- NULL
        
        sox_2005_date_data <- sox_2005
        sox_2005_date_data$date <-format(sox_2005_date_data$date, format="%m/%d")
        
        output$Findings_Raw1 <- renderDataTable(
          datatable(sox_2005_date_data[c(-1,-2,-4,-6)], 
                    options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15, 20, 25, 30),
                                   displayStart=290
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
        
        output$Findings_Raw2 <- NULL
        output$Findings_Raw3 <- NULL
        
        output$FindingsOut <- renderText({
          "Findings: 
           \nOctober 22, 2005: The first ever World Series game in this stadium. The White Sox get their first World Series game victory since 1959, defeating the Houston Astros 5–3. Attendance: (41,206)
           \nOctober 23, 2005: hosted Game 2 of the World Series. The Sox won 7–6. Additionally, The Sox would win the next two games in Houston to win their first World Series title since 1917. Attendance: (41,432)"   
        })
        
      }
      
      else if(input$Findings == "October 10 & 12, 2021"){
        sox_2021 <- cta_sox %>% filter(year(cta_sox$date) == 2021)
        sox_2021 <- sox_2021 %>% mutate( ToHighlight = ifelse( date == ymd('2021-10-10') | date == ymd('2021-10-12'), "yes", "no" ) )
        datebreaks <- seq(as.Date(min(sox_2021$date)), as.Date(max(sox_2021$date)), by="2 month")
        
        
        output$Findings_Bar1 <- renderPlot({
          ggplot(sox_2021, aes(date, rides, fill = ToHighlight)) +
            geom_col(width = 0.5) +
            labs(title="Sox-35th", 
                 subtitle="CTA Daily Data 2021",
                 x = "date", y = "rides")+
            scale_x_date(date_labels="%B",
                         breaks = datebreaks,
                         limits = c( as.Date(min(sox_2021$date)), as.Date(max(sox_2021$date))))+
            scale_fill_manual( values = c( "yes"="#FB4D3D", "no"="#2FAE3E" ), guide = FALSE )+
            scale_y_continuous(labels = scales::comma)
        })
        
        output$Findings_Bar2 <- NULL
        output$Findings_Bar3 <- NULL
        
        sox_2021_date_data <- sox_2021
        sox_2021_date_data$date <-format(sox_2021_date_data$date, format="%m/%d")
        
        output$Findings_Raw1 <- renderDataTable(
          datatable(sox_2021_date_data[c(-1,-2,-4,-6)], 
                    options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15, 20, 25, 30),
                                   displayStart=280
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
        
        output$Findings_Raw2 <- NULL
        output$Findings_Raw3 <- NULL
        
        output$FindingsOut <- renderText({
          "Findings: 
           \nOctober 10, 2021: Guaranteed Rate Field hosted its first playoff game since 2008 with the White Sox facing the Houston Astros in the ALDS with the Sox trailing 2 games to none. (Attendance: 40,288)
           \nOctober 12, 2021: Guaranteed Rate Field hosted game 4 of the ALDS between the White Sox and Astros. The Astros won 10-1 and advanced to the ALCS. (Attendance: 40,170)"   
        })
        
      }
      
      else if(input$Findings == "December 25"){
        data_halsted <- cta_halsted %>% filter(year(cta_halsted$date) == input$FindingsYear)
        data_halsted <- data_halsted %>% mutate( ToHighlight = ifelse(date == ymd(paste(factor(input$FindingsYear), "-12", "-25", sep="")), "yes", "no" ) )
        sub <- paste("CTA Daily", "Data:", input$FindingsYear, sep=" ")
        datebreaks <- seq(as.Date(min(data_halsted$date)), as.Date(max(data_halsted$date)), by="2 month")
        
        
        output$Findings_Bar1 <- renderPlot({
          ggplot(data_halsted, aes(date, rides, fill = ToHighlight)) +
            geom_col(width = 0.5) +
            labs(title="Halsted", 
                 subtitle=sub,
                 x = "date", y = "rides")+
            scale_x_date(date_labels="%B",
                         breaks = datebreaks,
                         limits = c( as.Date(min(data_halsted$date)), as.Date(max(data_halsted$date))) )+
            scale_fill_manual( values = c( "yes"="#FB4D3D", "no"="#0099f9" ), guide = FALSE )+
            scale_y_continuous(labels = scales::comma)
        })
        
        output$Findings_Bar2 <- NULL
        output$Findings_Bar3 <- NULL
        
        data_halsted_date_data <- data_halsted
        data_halsted_date_data$date <-format(data_halsted_date_data$date, format="%m/%d")
        
        output$Findings_Raw1 <- renderDataTable(
          datatable(data_halsted_date_data[c(-1,-2,-4,-6)], 
                    options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15, 20, 25, 30),
                                   displayStart=350
                    )) %>% 
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
        
        output$Findings_Raw2 <- NULL
        output$Findings_Raw3 <- NULL
        
        output$FindingsOut <- renderText({
          "Findings: 
          \nDuring each year, the lowest ridership recorded at Halsted station is 25th December. (With exception of polar vortex in January 30, 2019 and Covid outbreak in 2020 and 2021)"   
        })
        
      }

    })
  
  output$AboutOut <- renderText({
    "Created by: Kazi Shahrukh Omar\n
         Created: 9th February, 2022\n
         Data Source: https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f\n
         Data Category: Transportation\n
         Data Owner: Chicago Transit Authority\n
         Intended for visualizing the trends and interesting patterns in Chicago 'L' Station ridership data over the years (2001-2021)."   
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
