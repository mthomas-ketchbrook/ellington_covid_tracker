


get_case_data <- function() {
  
  df_cols <- paste(
    "lastupdatedate", 
    "towntotalcases", 
    "townconfirmedcases", 
    "towntotaldeaths", 
    "peopletested", 
    "numberoftests", 
    "numberofpositives", 
    "numberofnegatives", 
    sep = ", "
  )
  
  df <- RSocrata::read.socrata(
    url = glue::glue(
      "https://data.ct.gov/resource/28fr-iqnx.csv?", 
      "town=Ellington&",   # filter for Ellington
      "$select={df_cols}"   # select only desired columns
    )
  )
  
  df %>% 
    dplyr::mutate(lastupdatedate = as.Date(lastupdatedate)) %>% 
    dplyr::rename(
      date = lastupdatedate, 
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
    )
  
}


get_vax_data <- function() {
  
   RSocrata::read.socrata(
    url = glue::glue(
      "https://data.ct.gov/resource/gngw-ukpw.csv?", 
      "Town=Ellington&", 
      "$select=age_group, fully_vaccinated_percent, dateupdated"
    )
  ) %>% 
    dplyr::mutate(dateupdated = as.Date(dateupdated)) %>% 
    dplyr::filter(dateupdated == max(dateupdated))
  
}


generate_calendar_viz <- function(data, var) {
  
  var_name <- var %>% 
    stringr::str_replace_all(" ", "_") %>% 
    stringr::str_to_lower()
  
  max_val <- data %>% 
    dplyr::pull(.data[[var_name]]) %>% 
    max()
  
  # calendar heatmap
  data %>% 
    dplyr::select(date, .data[[var_name]]) %>% 
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
    dplyr::select(date, .data[[var_name]]) %>% 
    tidyr::drop_na() %>% 
    echarts4r::e_chart(x = date) %>% 
    echarts4r::e_bar_(serie = var_name, name = var) %>% 
    echarts4r::e_datazoom() %>%
    echarts4r::e_zoom(
      dataZoomIndex = 0,
      startValue = max(data$date) %m-% months(4),
      endValue = max(data$date)
    ) %>%
    echarts4r::e_tooltip(trigger = "axis") %>% 
    e_toolbox_feature(feature = "saveAsImage")
  
}


generate_area_chart <- function(data, var) {
  
  var_name <- var %>% 
    stringr::str_replace_all(" ", "_") %>% 
    stringr::str_to_lower()
  
  data %>% 
    dplyr::select(date, .data[[var_name]]) %>% 
    tidyr::drop_na() %>% 
    echarts4r::e_chart(x = date) %>% 
    echarts4r::e_area_(serie = var_name, name = var) %>% 
    echarts4r::e_datazoom() %>%
    echarts4r::e_zoom(
      dataZoomIndex = 0,
      startValue = max(data$date) %m-% months(4),
      endValue = max(data$date)
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

display_info_cards <- function(data) {
  
  data %>% 
    dplyr::mutate(id = 1:nrow(data)) %>% 
    split(.$id) %>% 
    purrr::map(.f = function(x) shiny::tagList(
      shiny::column(
        width = 12 %/% nrow(data), 
        create_info_card(
          header = paste0("Age Group: ", x$age_group), 
          main = paste0(x$fully_vaccinated_percent, "%"), 
          subtext = "of residents fully vaccinated", 
          fill = "#D9534F"
        )
      )
    )) %>% 
    shiny::tagList()
  
}

