library(synchrony)
library(tidyverse)

df <- readRDS("data/master_data.rds")

sync <- df %>%
  #group_by(site, year_shifted) %>%
  #group_by(site) %>%
  do({

    sync <- phase.sync(
      .$precipitation,
      .$evi,
      mod = 2,
      mins = TRUE,
      nrands = 1000
      )
    sync$deltaphase

    break
  })

