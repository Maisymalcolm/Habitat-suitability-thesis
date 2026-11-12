title: "test scrip 2 Lake Rotorua Buoy data"

## This script builds the wq_daily df by assigning a cast ID to each cast
## and 1 mean DO and mean temp value per depth band (m), so there is 20 readings total per cast per variable
## This will become integrated with the HSI

library(tidyverse)  # gives you dplyr, readr, ggplot2 etc. in one go

glimpse(wq)  # check column names, types
# This gives me a list of all wq variables and I can see what type of values they're listed as

library(lubridate)  # part of tidyverse, handles dates/times

wq <- wq %>%
  mutate(DateTime = ymd_hms(DateTime))  # adjust format function to match your data

glimpse(wq) # Checking that DateTime is a datetime object. 

wq %>% count(DateTime) %>% head()  # see how many rows per timestamp

## Since each cast has about 38 rows per variable, and therefore a different DateTime, there isn't a shared timestamp per individual cast. Therefore, need to build a cast_id that groups all rows within a cast together

## Using the depth reversals between the end of one cast and the beginning of the next to sort cast IDs. It will read a new cast ID when a depth reading reverses more than 15m (i.e from 19m back to 1)
min_drop <- 15   # <-- CHANGE THIS: smallest upward jump (in metres) that counts as a new cast

wq <- wq %>%
  arrange(DateTime) %>%
  mutate(
    depth_change = DptSns - lag(DptSns),                    # negative = instrument came back up
    new_cast     = is.na(depth_change) | depth_change < -min_drop,
    cast_id      = cumsum(new_cast)
  )

## Dropping all readings from the wq dataset that are <1m 
wq_sub <- dplyr::filter(wq, DptSns >= 1)
sum(is.na(wq_sub$DOconc)) # checking how many DOconc's are still NA


## Now tidying up the full wq df so that it keeps the single row per cast/depth combo whose raw depth was nearest to the rounded metre mark
wq_tidy <- wq_sub %>%
  mutate(depth_m = round(DptSns)) %>%
  filter(depth_m >= 1, depth_m <= 20) %>%
  mutate(depth_diff = abs(DptSns - depth_m)) %>%
  group_by(cast_id, depth_m) %>%
  slice_min(depth_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-depth_diff) 

## Creating a new object wq_daily with daily avaergages by depth for Do and temp
wq_daily <- wq_tidy %>%
  mutate(date = as.Date(DateTime)) %>%
  group_by(date, depth_m) %>%
  summarise(
    mean_do = mean(DOconc, na.rm = TRUE),
    mean_temp = mean(TmpWtr, na.rm = TRUE),
    .groups = "drop"
  )

glimpse(wq_daily)
saveRDS(wq_daily, "wq_daily.rds") 

# Attempting to plot a heatmap of temperature
ggplot(wq_daily, aes(x = date, y = depth_m, fill = mean_temp)) +
  geom_tile() +
  scale_y_reverse() +  # so 0m (surface) is at the top, deeper is downward
  scale_fill_viridis_c(option = "inferno", name = "Temp (°C)") +
  labs(
    title = "Water temperature through time and depth",
    x = "Date",
    y = "Depth (m)"
  ) +
  theme_minimal()

# Heatmap of DO
ggplot(wq_daily, aes(x = date, y = depth_m, fill = mean_do)) +
  geom_tile() +
  scale_y_reverse() +  # so 0m (surface) is at the top, deeper is downward
  scale_fill_viridis_c(option = "inferno", name = "DO (mg/L") +
  labs(
    title = "DO through time and depth",
    x = "Date",
    y = "Depth (m)"
  ) +
  theme_minimal()

##------------------ GAP AUDIT--------------------------------------

# First understand the pattern of missing values at 1m depth (are they all in one specific year?)
wq_daily %>%
  filter(depth_m == 1) %>%
  summarise(n_rows = n(), n_missing_DO = sum(is.na(mean_do)), pct_missing = mean(is.na(mean_do)) * 100) 

# Is it random, or concentrated in certain periods/years?
wq_daily %>%
  filter(depth_m == 1) %>%
  group_by(Year = lubridate::year(date)) %>%
  summarise(pct_missing = mean(is.na(mean_do)) * 100)

## Gap audit/exploration on wq_daily df

# Adding missing dates/depth as NA rows in the wq_daily df
library(dplyr)
library(tidyr)

wq_daily <- wq_daily %>%
  mutate(date = as.Date(date)) %>%
  complete(depth_m, date = seq(min(date), max(date), by = "day")) %>%
  arrange(depth_m, date) # NOTE this changes the layout of the wq_daily df so its now ordered by depth_m (i.e all of the 1m readings first for whole dataset)

nrow(wq_daily)   # should equal (number of days) x 20
wq_daily %>% count(depth_m)   # every depth should have the same count

# ---- PART 1: Count the gaps ----------------------------------------------
library(ggplot2)
library(lubridate)

# Assumes wq_daily has already been completed so every depth_m has a row
# for every day (missing days are NA rows). Columns: date, depth_m, mean_do, mean_temp.

# Find each run of consecutive missing days, per depth and variable
gap_runs <- wq_daily %>%
  arrange(depth_m, date) %>%
  pivot_longer(c(mean_temp, mean_do), names_to = "variable", values_to = "value") %>%
  group_by(depth_m, variable) %>%
  mutate(run_id = cumsum(c(1, diff(is.na(value)) != 0))) %>%   # new id each time NA/not-NA switches
  group_by(depth_m, variable, run_id) %>%
  filter(all(is.na(value))) %>%                                # keep only the all-NA runs
  summarise(start = min(date), end = max(date), gap_days = n(), .groups = "drop") %>%
  # drop NAs at the very start or end of the record (not real gaps)
  filter(start > min(wq_daily$date), end < max(wq_daily$date)) %>%
  mutate(
    month = month(start, label = TRUE),   # a gap is assigned to the month it starts in
    length_bin = cut(
      gap_days,
      breaks = c(0, 1, 3, 7, 14, Inf),
      labels = c("1 day", "2-3 days", "4-7 days", "8-14 days", ">14 days")
    )
  )

# Summary by variable x month x gap length
gap_by_month <- gap_runs %>%
  count(variable, month, length_bin) %>%
  pivot_wider(names_from = length_bin, values_from = n, values_fill = 0)

# Summary by variable x depth x gap length
gap_by_depth <- gap_runs %>%
  count(variable, depth_m, length_bin) %>%
  pivot_wider(names_from = length_bin, values_from = n, values_fill = 0)

# Simple per-day view: % of days missing in each month
missing_by_month <- wq_daily %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(month) %>%
  summarise(
    pct_temp_missing = round(mean(is.na(mean_temp)) * 100, 1),
    pct_do_missing   = round(mean(is.na(mean_do)) * 100, 1),
    .groups = "drop"
  )

print(gap_by_month, n = Inf)
print(gap_by_depth, n = Inf)
print(missing_by_month)

# Save the tables
write.csv(gap_runs, "gap_runs.csv", row.names = FALSE)
write.csv(gap_by_month, "gap_summary_by_month.csv", row.names = FALSE)
write.csv(gap_by_depth, "gap_summary_by_depth.csv", row.names = FALSE)
write.csv(missing_by_month, "missing_by_month.csv", row.names = FALSE)

# ---- PART 2: Plot where the gaps fall (date x depth) ---------------------

wq_daily %>%
  pivot_longer(c(mean_temp, mean_do), names_to = "variable", values_to = "value") %>%
  mutate(
    missing = is.na(value),
    variable = recode(variable, mean_temp = "Temperature", mean_do = "DO")
  ) %>%
  ggplot(aes(date, depth_m, fill = missing)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_manual(
    values = c("FALSE" = "grey90", "TRUE" = "firebrick"),
    labels = c("Present", "Missing"),
    name = NULL
  ) +
  facet_wrap(~variable, ncol = 1) +
  labs(x = 'date', y = "Depth (m)", title = NULL ) +
  theme_minimal()

# ---- Do long gaps cluster in summer? --------------------------------------
# Long gaps (more than 7 days) counted by the month they start in.
# Summer in NZ = Dec, Jan, Feb.
gap_runs %>%
  filter(gap_days > 7) %>%
  count(variable, month) %>%
  print(n = Inf)

## Numbers for reporting on in methods
# 1. Overall missingness (per depth-day)
wq_daily %>%
  summarise(
    pct_temp_missing = round(mean(is.na(mean_temp)) * 100, 1),
    pct_do_missing   = round(mean(is.na(mean_do)) * 100, 1),
    pct_either       = round(mean(is.na(mean_temp) | is.na(mean_do)) * 100, 1)
  )

# 2 and 3. Gap structure and where the missing days sit
gap_runs %>%
  group_by(variable) %>%
  summarise(
    n_gaps = n(),
    pct_gaps_over7 = round(mean(gap_days > 7) * 100, 1),
    missing_days = sum(gap_days),
    pct_days_in_gaps_over7 = round(sum(gap_days[gap_days > 7]) / sum(gap_days) * 100, 1),
    pct_days_in_gaps_over14 = round(sum(gap_days[gap_days > 14]) / sum(gap_days) * 100, 1),
    pct_days_recoverable_3 = round(sum(gap_days[gap_days <= 3]) / sum(gap_days) * 100, 1),
    pct_days_recoverable_7 = round(sum(gap_days[gap_days <= 7]) / sum(gap_days) * 100, 1)
  )
