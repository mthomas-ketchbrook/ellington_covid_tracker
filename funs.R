generate_calendar_viz <- function(data, var) {
  
  var_sym <- rlang::sym(var)
  
  var_quo <- rlang::enquo(var_sym)
  
  var_name <- var_quo %>% 
    dplyr::quo_name() %>% 
    stringr::str_replace("_", " ") %>% 
    tools::toTitleCase()
  
  max_val <- data %>% 
    dplyr::pull(var) %>% 
    max()
  
  plot_data <- data %>% 
    dplyr::rename(var_for_plot = !! var_quo)
  
  # calendar heatmap
  plot_data %>% 
    echarts4r::e_charts(date) %>% 
    echarts4r::e_calendar(range = "2020") %>% 
    echarts4r::e_heatmap(var_for_plot, coord_system = "calendar", name = var_name) %>% 
    echarts4r::e_visual_map(max = max_val) %>%
    echarts4r::e_title("Calendar", "Heatmap") %>% 
    # echarts4r::e_show_loading() %>% 
    echarts4r::e_brush(throttleDelay = 1000)
  
  
  
}