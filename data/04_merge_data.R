library(tidyverse)
library(patchwork)
library(zoo)

# read in raw data, remove some
# unrequired columns and convert
# a date to date type to allow
# for a left join
evi <- readRDS("data/EVI.rds") %>%
  dplyr::select(-id, -veg_type)
era5 <- readRDS("data/ERA5.rds") %>%
  mutate(
    date = as.Date(date)
  ) %>%
  dplyr::select(-veg_type)
trmm <- readRDS("data/TRMM.rds")
phen <- readRDS("data/phenology.rds")

# create a big dataframe of daily values
df <- left_join(trmm, era5, by = c("site", "date", "latitude", "longitude"))
df <- left_join(df, evi, by = c("site", "date", "latitude", "longitude"))

# do some linear interpolation for the
# evi values (this is by site)
df <- df %>%
  dplyr::select(-starts_with("product")) %>%
  group_by(site) %>%
  mutate(
    #evi = ifelse(evi < quantile(evi, 0.1, na.rm = TRUE), NA, evi),
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

# save data
saveRDS(df, file = "data/master_data.rds", compress = "xz")
