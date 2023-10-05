
library(tidyverse)
library(lubridate)

theme_set(theme_minimal())

df_raw <- rsconnect::showMetrics(metricSeries = "container_status",
                                 metricNames = "connect_count",
                                 appName = "Swiss_Elections_2023",
                                 server = "shinyapps.io",
                                 from = "2023-09-30")



df_raw %>%
  mutate(connect_count_total = cumsum(connect_count)) %>% 
  ggplot(aes(time, connect_count_total)) + 
  geom_line(linewidth = 1.5,
            alpha = 0.75) + 
  geom_vline(xintercept = as_datetime("2023-10-03 11:05:00")) +
  labs(title =  "Shiny-App-Benutzung",
       x = "Zeit",
       y = "Anzahl Verbindungen (Total)")


