library(excessmort)
library(tidyverse)

weekly_counts <- puerto_rico_counts |>
  filter(between(year(date), 2002, 2018)) |>
  mutate(date = floor_date(date, unit = "week", week_start = 3)) |>
  group_by(date, sex, agegroup) |>
  summarize(outcome = sum(outcome), population = mean(population), 
            n = n(), .groups = "drop") |>
  filter(n == 7) |>
  select(-n) |>
  mutate(week = epiweek(date)) |>
  mutate(rate = outcome/population,
         sex = as.factor(sex),
         day = difftime(date, min(date), units = "day"),
         week = as.factor(week))

save(weekly_counts, file = "data/excessmort_clean.RData")
