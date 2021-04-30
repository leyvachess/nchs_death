library(shiny)

library(tidyverse)
library(janitor)
causes <- read_csv("major_causes_ofdeath.csv") %>%
  clean_names() %>% 
  filter(age_adjusted_death_rate < 1000)
lifeexpectancy <- read_csv("life_expectancy_atbirth.csv") %>% clean_names()
usatopfive <- read_csv("usa_topfive_1990_1950_2000.csv") %>% clean_names()
topfiveexcess <- read_csv("topfive_excessdeaths.csv") %>% clean_names()
usaleadingcauses <- read_csv("usa_leadingcauses.csv") %>% clean_names()

ui <- fluidPage(
    titlePanel("NCHS Causes of Death in the US"),
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("causes",
                        "Select a cause of death to view on the plot.",
                        choices = unique(causes$cause),
                        selected = unique(causes$cause)[1]),
  

          checkboxGroupInput("sex",
                      "Please select a sex to view life expectancy for.",
                      choices = unique(lifeexpectancy$sex),
                      selected = unique(lifeexpectancy$sex)[1]),
          checkboxGroupInput("race",
                      "Please select a race to view life expectancy for.",
                      choices = unique(lifeexpectancy$race),
                      selected = unique(lifeexpectancy$race)[1])
        ),
        mainPanel(
           plotOutput("adjRatePlot"),
           plotOutput("lifeExpectancyPlot")
        )
    )
)

server <- function(input, output) {

  output$adjRatePlot <- renderPlot({
    
    if  (length(input$causes) > 0){
      cause_timelinedf <- causes %>% 
        filter(cause == input$causes)
    }
    
    else {cause_timelinedf <- causes}
  
    ggplot(cause_timelinedf, aes(x=year, y=age_adjusted_death_rate, color = cause)) + 
      geom_line() +
      labs(
        x = "Year",
        y = "Death Rate per 100,000",
        caption = "Data from NCHS, Public Domain",
        title = "Age adjusted death rates among US adults from 1900-2017"
      )
  })
  output$lifeExpectancyPlot <- renderPlot({
    
    if  (length(input$sex) > 0 & length(input$race) > 0){
     lifeTimelinedf <- lifeexpectancy %>%
        filter(sex %in% input$sex, race %in% input$race)
    }
    
    else {lifeTimelinedf <- lifeexpectancy}
    
    ggplot(lifeTimelinedf, aes(x=year, y=average_life_expectancy_years, color = sex, linetype = race)) + 
      geom_line() +
      labs(
        x = "Year",
        y = "Life Expectancy (in years)",
        caption = "Data from NCHS, Public Domain",
        title = "Average Life Expectancy by Year from 1900-2017"
      )
  })
}

shinyApp(ui = ui, server = server)
