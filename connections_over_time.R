
library(tidyverse)
library(lubridate)

theme_set(theme_minimal())

df <- rsconnect::showMetrics(metricSeries = "container_status",
                                 metricNames = "connect_count",
                                 appName = "Swiss_Elections_2023",
                                 server = "shinyapps.io",
                                 from = "2023-09-30") %>% 
  mutate(connect_count_total = cumsum(connect_count))



df %>%
  ggplot(aes(time, connect_count_total)) + 
  geom_line(color = "seagreen",
            linewidth = 2,
            alpha = 0.75) + 
  scale_y_continuous(breaks = seq(0, max(df$connect_count_total) + 100, 100)) +
  labs(title =  "Shiny-App-Benutzung",
       x = "Zeit",
       y = "Anzahl Verbindungen (Total)")


