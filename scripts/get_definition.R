get_definition <- function(var, lookup_tbl) {
  
  def <- lookup_tbl %>% 
    dplyr::filter(variable == var) %>% 
    dplyr::pull(definition)
  
  shiny::p(def)
  
}