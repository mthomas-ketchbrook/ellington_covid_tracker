library(RSocrata)
library(dplyr)
library(ggplot2)
library(echarts4r)
library(glue)


# General Socrata API documentation can be found here:
# - https://dev.socrata.com/docs/endpoints.html

# API Docs for these datasets:
# - https://dev.socrata.com/foundry/data.ct.gov/28fr-iqnx
# - https://dev.socrata.com/foundry/data.ct.gov/gngw-ukpw


rate_cols <- paste(
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
    # filter for Ellington
    "town=Ellington&", 
    # select only desired columns
    "$select={rate_cols}"
  )
)



vax_data <- RSocrata::read.socrata(
  url = glue::glue(
    "https://data.ct.gov/resource/gngw-ukpw.csv?", 
    "Town=Ellington&", 
    "$select=age_group, fully_vaccinated_percent, dateupdated"
  )
) %>% 
  dplyr::mutate(dateupdated = as.Date(dateupdated)) %>% 
  dplyr::filter(dateupdated == max(dateupdated))



plot_data <- df %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(date = as.Date(lastupdatedate)) %>% 
  dplyr::rename(
    confirmed_cases = townconfirmedcases, 
    number_of_tests = peopletested, 
    number_of_positives = numberofpositives
  ) %>% 
  dplyr::select(
    date, 
    confirmed_cases, 
    number_of_tests, 
    number_of_positives
  ) %>% 
  dplyr::mutate(
    new_cases = confirmed_cases - dplyr::lag(confirmed_cases)#, 
    # new_tests = number_of_tests - dplyr::lag(number_of_tests), 
    # new_positives = number_of_positives - dplyr::lag(number_of_positives)
  ) %>% 
  tidyr::drop_na()

plot_data %>% 
  ggplot2::ggplot(
    ggplot2::aes(
      x = date, 
      y = confirmed_cases
    )
  ) + 
  ggplot2::geom_line()

# echarts4r
plot_data %>% 
  echarts4r::e_chart(x = date) %>% 
  echarts4r::e_bar_(serie = "confirmed_cases") %>% #, smooth = TRUE) %>% 
  echarts4r::e_datazoom() %>%
  echarts4r::e_zoom(
    dataZoomIndex = 0,
    startValue = max(plot_data$date) %m-% months(3),
    endValue = max(plot_data$date)
  ) %>%
  echarts4r::e_tooltip(trigger = "axis")

# Line & Bar
plot_data %>% 
  tidyr::drop_na() %>% 
  echarts4r::e_chart(x = date) %>% 
  # echarts4r::e_line(serie = confirmed_cases, name = "Cumulative Cases", smooth = TRUE) %>% 
  echarts4r::e_bar(serie = new_cases, name = "New Cases")



# calendar heatmap
plot_data %>% 
  e_charts(date) %>% 
  e_calendar(range = "2020") %>% 
  e_heatmap(new_cases, coord_system = "calendar", name = "New Cases") %>% 
  e_visual_map(max = max(plot_data$new_cases)) %>%
  e_title("Calendar", "Heatmap")

plot_data %>% 
  dplyr::mutate(test_positivity_rate = number_of_positives / number_of_tests) %>% 
  echarts4r::e_charts() %>% 
  echarts4r::e_liquid(test_positivity_rate)


