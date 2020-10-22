library(shiny)
# library(RSocrata)
library(dplyr)
library(ggplot2)
library(echarts4r)
library(waiter)
library(stringr)
library(lubridate)

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


  
  shiny::tabPanel(
    
    title = "Home", 
    
    waiter::use_waiter(), 
    
    waiter::waiter_show_on_load(
      html = shiny::tagList(
        waiter::spin_ball(),
        "Getting Data from CT DPH Database..."
      ),
      logo = "Ketchbrook_Logo_nobackground_cropped.png"
    ), 
    
    shiny::sidebarLayout(
      
      sidebarPanel = sidebarPanel(
        
        shiny::titlePanel("Choose Chart Elements to Include"), 
        
        shiny::fluidRow(
          
          shiny::column(
            
            width = 3, 
            
            shiny::radioButtons(
              inputId = "select_var", 
              label = "Select Variable", 
              choices = df %>% 
                dplyr::select(-date) %>% 
                colnames() %>% 
                stringr::str_replace_all("_", " ") %>% 
                tools::toTitleCase()
            )
            
          )
          
        )
        
      ), 
      
      mainPanel = shiny::mainPanel(
        
        shiny::fluidRow(
          
        ), 
        
        shiny::hr(), 
        
        shiny::fluidRow(
          
          shiny::column(
            
            width = 9, 
            
            shiny::tabsetPanel(
              
              shiny::tabPanel(
                title = "Calendar", 
                
                shiny::wellPanel(
                  echarts4r::echarts4rOutput(
                    outputId = "calendar_heatmap"
                  )
                )
                
              ), 
              
              shiny::tabPanel(
                title = "Bar Chart"
              ), 
              
              type = "pills"
              
            )
            
            
            
          )
          
        )
        
      )
      
    )
    
  )
  
)

server <- function(input, output, session) {
  
  # Simulate the app reaching out to data.ct.gov
  Sys.sleep(4)
  waiter::waiter_hide()

  # Build the calendar heatmap visual 
  output$calendar_heatmap <- echarts4r::renderEcharts4r({

    generate_calendar_viz(
      data = df,
      var = input$select_var
    )

  })
  
  

}

shinyApp(ui, server)