generate_calendar_viz <- function(data, var) {
  
  var_sym <- var %>% 
    stringr::str_replace_all(" ", "_") %>% 
    stringr::str_to_lower() %>% 
    rlang::sym()
  
  var_quo <- rlang::enquo(var_sym)
  
  var_name <- rlang::quo_name(var_quo)
  
  var_name_chart <- var_quo %>% 
    dplyr::quo_name() %>% 
    stringr::str_replace("_", " ") %>% 
    tools::toTitleCase()
  
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
    echarts4r::e_heatmap_(var_name, coord_system = "calendar", name = var_name_chart) %>% 
    echarts4r::e_visual_map_(serie = var_name) %>%
    echarts4r::e_title(var_name_chart, "Heatmap") %>% 
    # echarts4r::e_tooltip(trigger = "item") %>%
    echarts4r::e_tooltip(
      formatter = htmlwidgets::JS("
        function(params){
          return('date: ' + params.value[0] + '<br />value: ' + params.value[1])
        }
      ")
    ) %>%
    # echarts4r::e_show_loading() %>% 
    echarts4r::e_brush(throttleDelay = 1000)
  
}