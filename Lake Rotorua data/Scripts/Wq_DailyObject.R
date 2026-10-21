title: "test scrip 2 Lake Rotorua Buoy data"

## This script builds the wq_daily df by assigning a cast ID to each cast
## and 1 mean DO and mean temp value per depth band (m), so there is 20 readings total per cast per variable
## This will become integrated with the HSI
## May need to add additional cast data preceding 2022?

library(tidyverse)  # gives you dplyr, readr, ggplot2 etc. in one go

glimpse(wq)  # check column names, types
# This gives me a list of all wq variables and I can see what type of values they're listed as

library(lubridate)  # part of tidyverse, handles dates/times

wq <- wq %>%
  mutate(DateTime = ymd_hms(DateTime))  # adjust format function to match your data

glimpse(wq) # Checking that DateTime is a datetime object. 

wq %>% count(DateTime) %>% head()  # see how many rows per timestamp

## Since each cast has about 38 rows per variable, and therefore a different DateTime, there isn't a shared timestamp per individual cast.
## Need to build a cast_id that groups all rows within a cast together


## Using the time gap between the end of one cast and the beginning of the next to sort casts
wq <- wq %>%
  arrange(DateTime) %>%
  mutate(
    time_gap = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    new_cast = is.na(time_gap) | time_gap > 15,  # adjust threshold: bigger than your normal within-cast interval
    cast_id = cumsum(new_cast)
  )

## Now tidying up the full wq df ------------------------------
wq_tidy <- wq %>%
  mutate(depth_m = round(DptSns)) %>%
  filter(depth_m >= 1, depth_m <= 20) %>%
  mutate(depth_diff = abs(DptSns - depth_m)) %>%
  group_by(cast_id, depth_m) %>%
  slice_min(depth_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-depth_diff) # keeps the single row per cast/depth combo whose raw depth was nearest to the rounded metre mark — so you're not just grabbing an arbitrary duplicate.
## the wq_tidy object now has our tidied data by cast

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

## Notice that there are quite a few gaps or NAs in the data, so may need to interpolate the missing values?