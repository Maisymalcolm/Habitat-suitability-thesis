title: "test scrip 2 Lake Rotorua Buoy data"


## Step 1 (Claude prompt): I have a large set of buoy data with 3hrly cast data that goes to 20m depth. 
# I first want to plot maybe 10-12 casts using species-specific threshold values for 
# freshwater crayfish so that I can see what their habitable range looks like in the profiles.

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

wq %>%
  arrange(DateTime) %>%
  mutate(gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins"))) %>%
  count(gap_mins) %>%
  arrange(desc(n))

wq %>%
  arrange(DateTime) %>%
  mutate(gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins"))) %>%
  count(gap_mins) %>%
  arrange(desc(gap_mins)) %>%
  print(n = 20)

## Using the time gap between the end of one cast and the beginning of the next to sort casts
wq <- wq %>%
  arrange(DateTime) %>%
  mutate(
    time_gap = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    new_cast = is.na(time_gap) | time_gap > 15,  # adjust threshold: bigger than your normal within-cast interval
    cast_id = cumsum(new_cast)
  )

first_12_casts <- wq %>%
  distinct(cast_id) %>%
  arrange(cast_id) %>%
  slice_head(n = 12) %>%
  pull(cast_id)

wq_subset <- wq %>%
  filter(cast_id %in% first_12_casts)

wq_subset %>%
  distinct(cast_id) %>%
  arrange(cast_id) ## Have set up my subsets now for 12 individual casts

## Now I'll set the koura-specific thresholds
threshold_do <- 5      # mg/L minimum
threshold_temp <- 24   # °C maximum

wq_profiled <- wq_subset %>%
  mutate(
    do_ok = DOconc >= threshold_do,       
    temp_ok = TmpWtr <= threshold_temp,  
    suitable = do_ok & temp_ok
  )

## Summarise per cast
cast_summary <- wq_profiled %>%
  group_by(cast_id) %>%
  summarise(
    n_readings = n(),
    n_suitable = sum(suitable, na.rm = TRUE),
    pct_suitable = n_suitable / n_readings * 100,
    min_do = min(DOconc, na.rm = TRUE),
    max_temp = max(TmpWtr, na.rm = TRUE)
  )

cast_summary

## Sanity check with a plot
## These plots are by cast for DO
ggplot(wq_profiled, aes(x = DptSns, y = DOconc, colour = suitable)) +
  geom_point() +
  geom_hline(yintercept = threshold_do, linetype = "dashed") +
  coord_flip() +
  scale_x_reverse() +
  facet_wrap(~cast_id) +
  labs(title = "DO profile by cast", y = "DO (mg/L)", x = "Depth (m)")

## Same check for the temp variable
ggplot(wq_profiled, aes(x = DptSns, y = TmpWtr, colour = suitable)) +
  geom_point() +
  geom_hline(yintercept = threshold_temp, linetype = "dashed") +
  coord_flip() +
  scale_x_reverse() +
  facet_wrap(~cast_id) +
  labs(title = "Temperature profile by cast", y = "Temp (°C)", x = "Depth (m)")

##Combining both plots to show temperature and DO for each of the 12 casts
# Work out a scaling factor so both variables are visually comparable
# (adjust these based on your actual data ranges)
scale_factor <- max(wq_profiled$DOconc, na.rm = TRUE) / max(wq_profiled$TmpWtr, na.rm = TRUE)

ggplot(wq_profiled, aes(x = DptSns)) +
  geom_point(aes(y = DOconc, colour = "DO (mg/L)")) +
  geom_point(aes(y = TmpWtr * scale_factor, colour = "Temp (°C)")) +
  geom_hline(yintercept = threshold_do, linetype = "dashed", colour = "steelblue") +
  geom_hline(yintercept = threshold_temp * scale_factor, linetype = "dashed", colour = "tomato") +
  coord_flip() +
  scale_x_reverse() +
  scale_y_continuous(
    name = "DO (mg/L)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Temp (°C)")
  ) +
  facet_wrap(~cast_id) +
  labs(x = "Depth (m)", colour = "Variable") +
  theme_minimal()


# Combining both variables on the same plot
scale_factor <- max(wq_profiled$DOconc, na.rm = TRUE) / max(wq_profiled$TmpWtr, na.rm = TRUE)

ggplot() +
  geom_rect(
    data = habitable_ranges,
    aes(xmin = depth_min, xmax = depth_max, ymin = -Inf, ymax = Inf),
    fill = "forestgreen", alpha = 0.15
  ) +
  geom_line(data = wq_profiled, aes(x = DptSns, y = DOconc, colour = "DO (mg/L)")) +
  geom_line(data = wq_profiled, aes(x = DptSns, y = TmpWtr * scale_factor, colour = "Temp (°C)")) +
  geom_hline(yintercept = threshold_do, linetype = "dashed", colour = "steelblue") +
  geom_hline(yintercept = threshold_temp * scale_factor, linetype = "dashed", colour = "tomato") +
  coord_flip() +
  scale_x_reverse() +
  scale_y_continuous(
    name = "DO (mg/L)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Temp (°C)")
  ) +
  facet_wrap(~cast_id) +
  labs(x = "Depth (m)", colour = "Variable", title = "DO and temperature overlay with habitable band shaded") +
  theme_minimal()

## (25.09.26) I want to tidy/simplify my subset data by producing only one row of data per m of lake depth ------------------------------------------------------------------------------------------------------
## So I should have 20 rows per cast, by taking only the variable that is cloest to the true meter depth (i.e measurements taken at 0.75 will be disregarded if there's a sample taken at 1.10)

wq_subset <- wq_subset %>%
  mutate(depth_m = round(DptSns)) # Round depth to the nearest meter (if you open the wq_subset df you can see its added a column for depth_m)

wq_subset <- wq_subset %>%
  filter(depth_m >= 1, depth_m <= 20) # Filter to keep only 1-20m depth

wq_tidy <- wq_subset %>%
  mutate(depth_diff = abs(DptSns - depth_m)) %>%
  group_by(cast_id, depth_m) %>%
  slice_min(depth_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-depth_diff) # keeps the single row per cast/depth combo whose raw depth was nearest to the rounded metre mark — so you're not just grabbing an arbitrary duplicate.

wq_tidy %>%
  count(cast_id) %>%
  print(n = 20) # check by cast ID how many rows each has (should equal 20 unless missing data) 

ggplot(wq_tidy, aes(x = DateTime, y = DptSns, colour = TmpWtr)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient2(
    midpoint = 15, 
    high = scales::muted("red"), 
    low = scales::muted("blue")
  )

## Now tidying up the full wq df ------------------------------
wq_tidy <- wq %>%
  mutate(depth_m = round(DptSns)) %>%
  filter(depth_m >= 1, depth_m <= 20) %>%
  mutate(depth_diff = abs(DptSns - depth_m)) %>%
  group_by(cast_id, depth_m) %>%
  slice_min(depth_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-depth_diff) ## the wq_tidy object now has our tidied data by cast

## Creating a new object wq_daily with daily avaergages by depth for Do and temp
wq_daily <- wq_tidy %>%
  mutate(date = as.Date(DateTime)) %>%
  group_by(date, depth_m) %>%
  summarise(
    mean_do = mean(DOconc, na.rm = TRUE),
    mean_temp = mean(TmpWtr, na.rm = TRUE),
    .groups = "drop"
  )

# Attempting to plot a heatmap
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