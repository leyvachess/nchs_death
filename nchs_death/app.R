#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NCHS Causes of Death in the US"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("Causes",
                        "Select a cause of death to view on the plot.",
                        choices = unique(causes$cause),
                        selected = unique(causes$cause)[1])
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("adjRatePlot")
        )
        
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
    
    output$adjRatePlot <- renderPlot({
      
      
      cause_timelinedf <- causes %>% 
        filter(cause == input$causes)
      
        ggplot(cause_timelinedf, aes(x=year, y=age_adjusted_death_rate, color = cause)) + 
            geom_line() +
            labs(
                x = "Year",
                y = "Death Rate per 100,000",
                caption = "Data from NCHS, Public Domain",
                title = "Age adjusted death rates among US adults from 1900-2017"
            )
        
    })
    
    # output$debug <- renderText({
    #   paste(input$causes, sep = ",")
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
