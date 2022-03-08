library(synchrony)
library(tidyverse)

df <- readRDS("data/master_data.rds")

# EVI threshold (% of yearly amplitude)
threshold = 0.25

# get phenology dates
# based upon the moving window approach
# use shifted window to get to a DOY which
# is not affected by end of year date issues
# when calculating DOY values
phen <- df %>%
  group_by(site, year_shifted) %>%
  summarize(
   date_evi = date[which(evi > evi_mov)[1]],
   date_evi_thresh = date[
     which(
       evi > (min(evi) + ((max(evi) - min(evi)) * threshold)) &
       evi > evi_mov
           )[1]
     ],
   date_prec = date[which(precip_cum > (map * 0.05))[1]],
   date_shifted_evi = date_shifted[which(evi > evi_mov)[1]],
   date_shifted_evi_thresh = date_shifted[
     which(
       evi > (min(evi) + ((max(evi) - min(evi)) * threshold)) &
         evi > evi_mov
     )[1]
   ],
   date_shifted_prec = date_shifted[which(precip_cum > (map * 0.05))[1]]
  ) %>%
  mutate(
    doy_shifted_evi = as.numeric(format(date_shifted_evi, "%j")),
    doy_shifted_evi_thresh = as.numeric(format(date_shifted_evi_thresh, "%j")),
    doy_shifted_prec  = as.numeric(format(date_shifted_prec, "%j"))
  ) %>%
  # truncate to data coverage
  filter(
    as.numeric(year_shifted) > 2000,
    as.numeric(year_shifted) < 2019
  )

# plot results
p <- ggplot(df) +
  geom_line(aes(date, precip_norm), colour = "lightblue") +
  geom_line(aes(date, evi)) +
  geom_line(aes(date, evi_mov), colour = "red") +
  geom_vline(
    data = phen,
    aes(xintercept = date_evi_thresh)
  ) +
  geom_vline(
    data = phen,
    aes(xintercept = date_evi),
    lty = 2
  ) +
  geom_vline(
    data = phen,
    aes(xintercept = date_prec),
    colour = "blue"
  ) +
  coord_cartesian(
    ylim = c(0.1,0.6)
    ) +
  facet_wrap(~site, nrow = 8)
plot(p)

# here the paper does not provide enough information
# to reproduce the results despite making use of an
# off the shelve package (or so it seems)
# don't see how the default synchrony package
# accomodates for the use of externally calculated
# minima - i.e. custom adaptation should have
# been made but are not reported minima are set in
# synchrony:::phase.sync.aux as are the interpolations etc
synchrony <- df %>%
  group_by(site) %>%
  do({

    sync <- phase.sync(
      as.numeric(.$evi),
      as.numeric(.$precipitation),
      mod = 2,
      mins = TRUE # no custom option
      )

    #hist(sync$deltaphase$mod_phase_diff_pi)
    #print(sync)

  })

