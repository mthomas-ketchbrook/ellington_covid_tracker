library(shiny)
library(RSocrata)
library(dplyr)
library(ggplot2)
library(echarts4r)
library(waiter)
library(stringr)
library(lubridate)

source("funs.R")
source("scripts/definitions.R")
source("scripts/get_definition.R")

df <- RSocrata::read.socrata(
  url = "https://data.ct.gov/resource/28fr-iqnx.csv?Town=Ellington"
)

# df <- read.csv("tmp_data.csv")

df <- df %>% 
  dplyr::mutate(date = as.Date(lastupdatedate)) %>% 
  dplyr::rename(
    total_cases = towntotalcases, 
    confirmed_cases = townconfirmedcases, 
    total_deaths = towntotaldeaths, 
    people_tested = peopletested, 
    number_of_tests = numberoftests, 
    number_of_positives = numberofpositives, 
    number_of_negatives = numberofnegatives
  ) %>% 
  dplyr::mutate(
    new_cases = total_cases - dplyr::lag(total_cases), 
    new_confirmed_cases = confirmed_cases - dplyr::lag(confirmed_cases), 
    new_tests = number_of_tests - dplyr::lag(number_of_tests), 
    new_people_tested = people_tested - dplyr::lag(people_tested), 
    new_positive_tests = number_of_positives - dplyr::lag(number_of_positives), 
    new_deaths = total_deaths - dplyr::lag(total_deaths)
  ) %>% 
  dplyr::select(
    -c(
      lastupdatedate, 
      town_no, 
      town, 
      towncaserate, 
      ratetested100k, 
      townprobabledeaths, 
      townprobablecases, 
      townconfirmeddeaths, 
      numberofindeterminates
    )
  )

# Create UI
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
    
    shiny::fluidRow(
      
      shiny::column(
        
        width = 6, 
        
        create_info_card(
          header = "Data as of", 
          main = paste0(format(max(df$date), "%B %d, %Y")), 
          subtext = "More recent data is subject to change", 
          fill = "#D9534F"
        )
        
      ), 
      
      shiny::column(
        
        width = 6, 
        
        create_info_card(
          header = "Total Test Positivity Rate", 
          main = df %>% 
            dplyr::filter(date == max(date)) %>% 
            dplyr::mutate(test_positivity_rate = (number_of_positives / number_of_tests) * 100) %>% 
            dplyr::pull(test_positivity_rate) %>% 
            round(2) %>% 
            paste0("%"), 
          subtext = "Calculated using the total positive tests divided by the total tests", 
          fill = "#D9534F"
        )
        
      )
      
    ), 
    
    shiny::hr(), 
    
    
    # Filters & Graphs - Cumulative ----
    shiny::fluidRow(
      
      shiny::column(
        
        width = 3, 
        
        shiny::h3(""), 
        
        shiny::wellPanel(
          
          shiny::h2("Choose Metric to Display in the Chart"), 
          
          shiny::selectInput(
            inputId = "select_var_1", 
            label = "Select Cumulative Metric to Display", 
            choices = df %>% 
              dplyr::select(
                -c(
                  date, 
                  new_cases, 
                  new_confirmed_cases, 
                  new_people_tested, 
                  new_tests, 
                  new_positive_tests, 
                  new_deaths
                )
              )%>% 
              colnames() %>% 
              stringr::str_replace_all("_", " ") %>% 
              tools::toTitleCase(), 
            selected = "Total Cases", 
            multiple = FALSE
          ), 
          
          shiny::br(), 
          
          shiny::uiOutput(outputId = "defs_ui_1")
          
        )
        
      ), 
      
      shiny::column(
        
        width = 9, 
        
        shiny::h3("Cumulative Statistics"), 
        
        shiny::tabsetPanel(
          
          shiny::tabPanel(
            title = "Area Chart", 
            
            shiny::wellPanel(
              style = "background: #F0F0F0", 
              echarts4r::echarts4rOutput(
                outputId = "area_chart"
              )
            )
            
          ), 
          
          shiny::tabPanel(
            title = "Bar Chart", 
            
            shiny::wellPanel(
              style = "background: #F0F0F0", 
              echarts4r::echarts4rOutput(
                outputId = "bar_chart_1"
              )
            )
            
          ), 
          
          shiny::p("By default, last 4 months are shown; to change this, move the slider below the chart.")
          
        )
        
      )
      
    ), 
    
    shiny::hr(), 
    
    # Filters & Graphs - Non-Cumulative ----
    shiny::fluidRow(
      
      shiny::column(
        
        width = 3, 
        
        shiny::wellPanel(
          
          shiny::h2("Choose Metric to Display in the Chart"), 
          
          shiny::selectInput(
            inputId = "select_var_2", 
            label = "Select Non-Cumulative Metric", 
            choices = df %>% 
              dplyr::select(
                new_cases, 
                new_confirmed_cases, 
                new_people_tested, 
                new_tests, 
                new_positive_tests, 
                new_deaths
              )%>% 
              colnames() %>% 
              stringr::str_replace_all("_", " ") %>% 
              tools::toTitleCase(), 
            selected = "New Cases", 
            multiple = FALSE
          ),
          
          shiny::br(), 
          
          shiny::uiOutput(outputId = "defs_ui_2")
          
        )
        
      ), 
      
      shiny::column(
        
        width = 9, 
        
        shiny::h3("Non-Cumulative Statistics"), 
        
        shiny::tabsetPanel(
          
          shiny::tabPanel(
            title = "Calendar", 
            
            shiny::wellPanel(
              style = "background: #F0F0F0", 
              echarts4r::echarts4rOutput(
                outputId = "calendar_heatmap"
              )
            )
            
          ), 
          
          shiny::tabPanel(
            title = "Bar Chart", 
            
            shiny::wellPanel(
              style = "background: #F0F0F0", 
              echarts4r::echarts4rOutput(
                outputId = "bar_chart_2"
              )
            ), 
            
            shiny::p("By default, last 4 months are shown; to change this, move the slider below the chart.")
            
          )
          
        )
        
      )
      
    )
    
  ), 
  
  shiny::tabPanel(
    
    title = "About", 
    
    shiny::fluidRow(
      
      shiny::column(
        width = 12, 
        
        shiny::div(
          class = "jumbotron", 
          shiny::h1("Enjoying This App?"), 
          shiny::p(
            class = "lead", 
            "Check out what else Ketchbrook Analytics can do for you."
          ), 
          shiny::a(
            class = "btn btn-info btn-lg", 
            href = "https://www.ketchbrookanalytics.com/", 
            target = "_blank", 
            "Visit Us"
          )
        )
        
      )
      
    )
    
  )
  
)

server <- function(input, output, session) {
  
  # Simulate the app reaching out to data.ct.gov
  Sys.sleep(3)
  waiter::waiter_hide()
  
  # Build the area chart
  output$area_chart <- echarts4r::renderEcharts4r({
    
    generate_area_chart(
      data = df,
      var = input$select_var_1
    )
    
  })
  
  # Build the bar chart
  output$bar_chart_1 <- echarts4r::renderEcharts4r({
    
    generate_bar_chart(
      data = df,
      var = input$select_var_1
    )
    
  })
  
  # Build the calendar heatmap visual 
  output$calendar_heatmap <- echarts4r::renderEcharts4r({
    
    generate_calendar_viz(
      data = df,
      var = input$select_var_2
    )
    
  })
  
  # Build the bar chart
  output$bar_chart_2 <- echarts4r::renderEcharts4r({
    
    generate_bar_chart(
      data = df,
      var = input$select_var_2
    )
    
  })
  
  # Build the cumulative definitions
  output$defs_ui_1 <- shiny::renderUI({
    
    get_definition(
      var = input$select_var_1, 
      lookup_tbl = definitions
    )
    
  })
  
  # Build the non-cumulative definitions
  output$defs_ui_2 <- shiny::renderUI({
    
    get_definition(
      var = input$select_var_2, 
      lookup_tbl = definitions
    )
    
  })
  
  
  
}

shinyApp(ui, server)