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
  rename(
    'precipitation_era' = 'total_precipitation'
  ) %>%
  dplyr::select(-veg_type)
trmm <- readRDS("data/TRMM.rds")
phen <- readRDS("data/phenology.rds")

# create a big dataframe of daily values
df <- left_join(
  trmm,
  era5,
  by = c(
    "site",
    "date",
    "latitude",
    "longitude"
    )
  )

df <- left_join(
  df,
  evi,
  by = c(
    "site",
    "date",
    "latitude",
    "longitude"
    )
  )

# do some linear interpolation for the
# evi values where missing (QA/QC of GEE)
# define the shifted years for easier
# aggregation / grouping
df <- df %>%
  dplyr::select(-starts_with("product")) %>%
  group_by(site) %>%
  mutate(
    evi = zoo::na.approx(evi, na.rm = TRUE),
    evi_mov = zoo::rollmean(
      evi,
      k = 60,
      na.pad = TRUE,
      align = "right"
    ),
    date_shifted = date - 180,
    year_shifted = format(date_shifted, "%Y"),
    precip_norm = scales::rescale(precipitation, c(0.1,0.6))
  ) %>%
  ungroup() %>%
  group_by(site, year_shifted) %>%
  mutate(
    precip_cum = cumsum(precipitation)
  ) %>%
  ungroup()

# get MAP and other components
# needed for precip based analysis
df <- df %>%
  group_by(site, year_shifted) %>%
  mutate(
    ap = sum(precipitation)
  ) %>%
  group_by(site) %>%
  mutate(
    map = mean(ap)
  )

# save data
saveRDS(df, file = "data/master_data.rds", compress = "xz")
