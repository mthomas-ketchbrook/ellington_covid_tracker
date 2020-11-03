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
    dplyr::select("date", var_name) %>% 
    tidyr::drop_na() %>% 
    echarts4r::e_charts(date) %>% 
    echarts4r::e_calendar(
      range = c(
        format(lubridate::today() %m+% months(1), "%Y-%m"), 
        format(lubridate::today() %m-% months(4), "%Y-%m")
      )
    ) %>% 
    echarts4r::e_heatmap_(var_name, coord_system = "calendar", name = var) %>% 
    echarts4r::e_visual_map_(serie = var_name) %>%
    echarts4r::e_title(var, "Last 4 Months") %>% 
    echarts4r::e_tooltip(
      formatter = htmlwidgets::JS("
        function(params){
          return(params.value[0] + '<br />value: ' + params.value[1])
        }
      ")
    ) %>% 
    e_toolbox_feature(feature = "saveAsImage") %>% 
    echarts4r::e_brush(throttleDelay = 1000)
  
}


generate_bar_chart <- function(data, var) {
  
  var_name <- var %>% 
    stringr::str_replace_all(" ", "_") %>% 
    stringr::str_to_lower()
  
  # Bar Chart
  data %>% 
    dplyr::select("date", var_name) %>% 
    tidyr::drop_na() %>% 
    echarts4r::e_chart(x = date) %>% 
    echarts4r::e_bar_(serie = var_name, name = var) %>% 
    echarts4r::e_datazoom() %>%
    echarts4r::e_zoom(
      dataZoomIndex = 0,
      startValue = max(plot_data$date) %m-% months(4),
      endValue = max(plot_data$date)
    ) %>%
    echarts4r::e_tooltip(trigger = "axis") %>% 
    e_toolbox_feature(feature = "saveAsImage")
  
}


generate_area_chart <- function(data, var) {
  
  var_name <- var %>% 
    stringr::str_replace_all(" ", "_") %>% 
    stringr::str_to_lower()
  
  data %>% 
    dplyr::select("date", var_name) %>% 
    tidyr::drop_na() %>% 
    echarts4r::e_chart(x = date) %>% 
    echarts4r::e_area_(serie = var_name, name = var) %>% 
    echarts4r::e_datazoom() %>%
    echarts4r::e_zoom(
      dataZoomIndex = 0,
      startValue = max(plot_data$date) %m-% months(4),
      endValue = max(plot_data$date)
    ) %>%
    echarts4r::e_tooltip(trigger = "axis") %>% 
    e_toolbox_feature(feature = "saveAsImage")
  
}


create_info_card <- function(header, main, subtext, fill = NULL) {
  
  if (!is.null(fill)) {
    
    fill <- paste0("background: ", fill)
    
  }
  
  shiny::wellPanel(
    style = fill, 
    shiny::h4(header), 
    shiny::h2(main), 
    shiny::p(subtext)
  )
  
}