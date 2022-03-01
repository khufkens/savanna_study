library(tidyverse)
library(patchwork)
library(ggpubr)
library(lsmeans)

df <- readRDS("data/master_data.rds")

df_ann <- df %>%
  group_by(site, year_shifted) %>%
  summarize(
    ap = sum(precipitation),
    evi_auc = sum(evi, na.rm = TRUE),
    evi_max = max(evi, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    site = as.factor(site)
  )

p1 <- ggscatter(
    df_ann,
    x = "ap",
    y = "evi_auc",
    color = "site",
    add = "reg.line"
  ) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = site),
    label.x = 1100
  ) +
  labs(
    x = "Annual precipitation (mm yr-1)",
    y = "EVI (total AUC)"
  ) +
  xlim(0, 1500) +
  theme_minimal()

p2 <- ggscatter(
  df_ann,
  x = "ap",
  y = "evi_max",
  color = "site",
  add = "reg.line"
  ) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = site),
    label.x = 1100
  ) +
  labs(
    x = "Annual precipitation (mm yr-1)",
    y = "EVI (seasonal max)"
  ) +
  xlim(0, 1500) +
  theme_minimal()

p <- (p1 / p2) + plot_layout(
  guides = "collect"
)

plot(p)

#----- compare slopes ----

# this method uses the lsmeans package
# and least-squares means routine
fit <- lm(evi_auc ~ ap*site, data = df_ann)
anova(fit)

fit$coefficients
slopes <- lstrends(fit, "site", var="ap")

# Compare slopes
pairs(slopes)

# same can be done with an ANCOVA and model selection
# on the interaction term (if you want to write it
# out in full)

# long short, no significant differences are found
# between sites in terms of the slope between
# annual precip and EVI (max or area under the curve)
# -- so no difference in sensitivity
