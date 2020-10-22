library(RSocrata)
library(dplyr)
library(ggplot2)
library(echarts4r)

df <- RSocrata::read.socrata(
  url = "https://data.ct.gov/resource/28fr-iqnx.csv?Town=Ellington"
)

plot_data <- df %>% 
  dplyr::mutate(date = as.Date(lastupdatedate)) %>% 
  dplyr::rename(confirmed_cases = townconfirmedcases) %>% 
  dplyr::select(
    date, 
    confirmed_cases
  ) %>% 
  dplyr::mutate(
    new_cases = confirmed_cases - dplyr::lag(confirmed_cases)
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
  echarts4r::e_line(serie = confirmed_cases, smooth = TRUE)

# Line & Bar
plot_data %>% 
  tidyr::drop_na() %>% 
  echarts4r::e_chart(x = date) %>% 
  echarts4r::e_line(serie = confirmed_cases, name = "Cumulative Cases", smooth = TRUE) %>% 
  echarts4r::e_bar(serie = new_cases, name = "New Cases")

# calendar heatmap
plot_data %>% 
  e_charts(date) %>% 
  e_calendar(range = "2020") %>% 
  e_heatmap(new_cases, coord_system = "calendar", name = "New Cases") %>% 
  e_visual_map(max = max(plot_data$new_cases)) %>%
  e_title("Calendar", "Heatmap")




