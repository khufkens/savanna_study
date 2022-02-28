library(tidyverse)
library(patchwork)
library(zoo)

# read in raw data, remove some
# unrequired columns and convert
# a date to date type to allow
# for a left join
evi <- readRDS("data/EVI.rds") %>%
  select(-id, -veg_type)
era5 <- readRDS("data/ERA5.rds") %>%
  mutate(
    date = as.Date(date)
  ) %>%
  select(-veg_type)
trmm <- readRDS("data/TRMM.rds")
phen <- readRDS("data/phenology.rds")

# create a big dataframe of daily values
df <- left_join(trmm, era5, by = c("site", "date"))
df <- left_join(df, evi, by = c("site", "date"))

# do some linear interpolation for the
# evi values (this is by site)
df <- df %>%
  group_by(site) %>%
  mutate(
    evi = ifelse(evi < quantile(evi, 0.1, na.rm = TRUE), NA, evi),
    evi = zoo::na.approx(evi, na.rm = TRUE),
    evi_norm = scales::rescale(evi, c(0, 1)),
    date_shifted = date - 180,
    year_shifted = format(date_shifted, "%Y"),
    precip_norm = scales::rescale(precipitation, c(0,1))
  ) %>%
  ungroup() %>%
  mutate(
    temp_norm = scales::rescale(mean_2m_air_temperature, c(0, 1)),
    temp_col = ifelse(dewpoint_2m_temperature < 288.15, "low", "high")
  ) %>%
  ungroup() %>%
  group_by(site, year_shifted) %>%
  mutate(
    precip_cum = cumsum(precipitation)
  ) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(
    precip_cum_norm = scales::rescale(precip_cum, c(0,1))
  )

#---- annual summaries ----

df_ann <- df %>%
  group_by(site, year_shifted) %>%
  summarize(
    ap = sum(precipitation),
    evi_auc = sum(evi, na.rm = TRUE),
    evi_max = max(evi, na.rm = TRUE)
  )

p1 <- ggplot(df_ann) +
  geom_point(
    aes(
      ap,
      evi_max,
      colour = site
    )
  ) +
  geom_smooth(
    aes(
      ap,
      evi_max,
      colour = site
    ),
    method=lm,
    se=FALSE
    ) +
  labs(
    x = "Annual precipitation (mm yr-1)",
    y = "EVI (seasonal max)"
  ) +
  theme_minimal()

p2 <- ggplot(df_ann) +
  geom_point(
    aes(
      ap,
      evi_auc,
      colour = site
    )
  ) +
  geom_smooth(
    aes(
      ap,
      evi_auc,
      colour = site
    ),
    method=lm,
    se=FALSE
  ) +
  labs(
    x = "Annual precipitation (mm yr-1)",
    y = "EVI (total AUC)"
  ) +
  theme_minimal()

plot(p1 | p2)

#---- some plotting of time series ----

p <- ggplot(df) +
  geom_line(
    aes(
      date,
      evi_norm
    )
  ) +
  geom_line(
    aes(
      date,
      temp_norm,
      colour = temp_col,
      group = site
    )
  ) +
  geom_line(
    aes(
      date,
      precip_cum_norm,
      group = year_shifted
    ),
    colour = "blue"
  ) +
  geom_line(
    aes(
      date,
      precip_norm,
      group = year_shifted
    ),
    colour = "blue"
  ) +
  geom_vline(
    data = phen,
    aes(
      xintercept = MidGreenup_1
    )
  ) +
  geom_vline(
    data = phen,
    aes(
      xintercept = MidGreendown_1
    )
  ) +
  theme_minimal() +
  facet_grid(site ~ .)

print(p)
