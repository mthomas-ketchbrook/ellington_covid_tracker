

definitions <- tibble::tibble(
  variable = c(
    "Total Cases", 
    "Confirmed Cases", 
    "Total Deaths", 
    "People Tested", 
    "Number of Tests", 
    "Number of Positives", 
    "Number of Negatives", 
    "New Cases", 
    "New Confirmed Cases", 
    "New People Tested", 
    "New Tests", 
    "New Positive Tests", 
    "New Deaths"
  ), 
  definition = c(
    "Sum of confirmed and probable COVID-19 cases.", 
    "Number of laboratory-confirmed COVID-19 cases.", 
    "Sum of confirmed and probable COVID-19-associated deaths.", 
    "Number of people tested.", 
    "Number of COVID-19 PCR tests reported.", 
    "Number of positive tests.", 
    "Number of negative tests.", 
    "Difference between most recent day's and prior day's \'Total Cases\' count.", 
    "Difference between most recent day's and prior day's \'Confirmed Cases\' count.", 
    "Difference between most recent day's and prior day's \'People Tested\' count.", 
    "Difference between most recent day's and prior day's \'Number of Tests\' count.", 
    "Difference between most recent day's and prior day's \'Number of Positives\' count.", 
    "Difference between most recent day's and prior day's \'Total Deaths\' count."
  )
)


get_definition <- function(var, lookup_tbl) {
  
  def <- lookup_tbl %>% 
    dplyr::filter(variable == var) %>% 
    dplyr::pull(definition)
  
  # shiny::p(def)
  
}