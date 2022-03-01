library(tidyverse)
library(patchwork)

df <- readRDS("data/master_data.rds") %>%
  filter(
    date > "2015-01-01"
  )

#---- some plotting of temperature time series ----
#
# Shows periods sensitive to ITCZ influences

p <- ggplot(df) +
  geom_line(
    aes(
      date,
      mean_2m_air_temperature - 273.15,
      colour = temp_col,
      group = site
    )
  ) +
  scale_colour_discrete(
    name = "Precipitation events"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  facet_grid(site ~ .)

print(p)
