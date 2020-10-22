library(shiny)
# library(RSocrata)
library(dplyr)
library(ggplot2)
library(echarts4r)
# library(waiter)
library(stringr)

source("funs.R")

# df <- RSocrata::read.socrata(
#   url = "https://data.ct.gov/resource/28fr-iqnx.csv?Town=Ellington"
# )

df <- read.csv("tmp_data.csv")

df <- df %>% 
  dplyr::mutate(date = as.Date(lastupdatedate)) %>% 
  dplyr::rename(
    total_cases = towntotalcases, 
    confirmed_cases = townconfirmedcases, 
    probable_cases = townprobablecases, 
    total_deaths = towntotaldeaths, 
    confirmed_deaths = townconfirmeddeaths, 
    probable_deaths = townprobabledeaths, 
    people_tested = peopletested, 
    number_of_tests = numberoftests, 
    number_of_positives = numberofpositives, 
    number_of_negatives = numberofnegatives, 
    number_of_indeterminates = numberofindeterminates
  ) %>% 
  dplyr::mutate(
    new_cases = confirmed_cases - dplyr::lag(confirmed_cases)
  ) %>% 
  dplyr::select(
    -c(
      lastupdatedate, 
      town_no, 
      town, 
      towncaserate, 
      ratetested100k
    )
  )
  

ui <- shiny::navbarPage(
  
  title = "Ellington, CT COVID-19 Tracker", 
  
  theme = shinythemes::shinytheme(theme = "superhero"), 
  
  collapsible = TRUE, 
  
  # waiter::use_waiter(), 
  
  # waiter::waiter_show_on_load(
  #   html = shiny::tagList(
  #     waiter::spin_ball(), 
  #     "Getting Data from CT DPH Database..."
  #   )
  # ), 
  
  shiny::tabPanel(
    
    title = "Home", 
    
    shiny::sidebarLayout(
      
      sidebarPanel = sidebarPanel(
        
        shiny::titlePanel("Choose Chart Elements to Include"), 
        
        shiny::fluidRow(
          
          shiny::radioButtons(
            inputId = "select_var", 
            label = "Select Variable", 
            choices = colnames(df)
          )
          
        )
        
      ), 
      
      mainPanel = shiny::mainPanel(
        
        shiny::fluidRow(
          
          echarts4r::echarts4rOutput(
            outputId = "calendar_heatmap"
          ),

          # waiter::waiter_hide_on_render(
          #   id = "calendar_heatmap"
          # )
          
        )
        
      )
      
    )
    
  )
  
)

server <- function(input, output, session) {
  
  output$calendar_heatmap <- echarts4r::renderEcharts4r({

    generate_calendar_viz(
      data = df,
      var = input$select_var
    )

  })

}

shinyApp(ui, server)