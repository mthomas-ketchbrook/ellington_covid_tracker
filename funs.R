generate_calendar_viz <- function(data, var) {
  
  var_name <- var %>% 
    stringr::str_replace_all(" ", "_") %>% 
    stringr::str_to_lower()
  
  var_sym <- rlang::sym(var_name)
  
  var_quo <- rlang::enquo(var_sym)
  
  max_val <- data %>% 
    dplyr::pull(!! var_quo) %>% 
    max()
  
  # calendar heatmap
  data %>% 
    echarts4r::e_charts(date) %>% 
    echarts4r::e_calendar(
      range = c(
        format(lubridate::today() %m+% months(1), "%Y-%m"), 
        format(lubridate::today() %m-% months(6), "%Y-%m")
      )
    ) %>% 
    echarts4r::e_heatmap_(var_name, coord_system = "calendar", name = var) %>% 
    echarts4r::e_visual_map_(serie = var_name) %>%
    echarts4r::e_title(var, "Heatmap") %>% 
    echarts4r::e_tooltip(
      formatter = htmlwidgets::JS("
        function(params){
          return('date: ' + params.value[0] + '<br />value: ' + params.value[1])
        }
      ")
    ) %>%
    echarts4r::e_brush(throttleDelay = 1000)
  
}


generate_bar_chart <- function(data, var) {
  
  var_name <- var %>% 
    stringr::str_replace_all(" ", "_") %>% 
    stringr::str_to_lower()
  
  # Line & Bar
  data %>% 
    tidyr::drop_na() %>% 
    echarts4r::e_chart(x = date) %>% 
    echarts4r::e_bar_(serie = var_name, name = var) %>% 
    echarts4r::e_tooltip(trigger = "item")
  
}