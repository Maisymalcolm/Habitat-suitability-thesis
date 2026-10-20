title: Habitat Suitability Calculations

## (07.10.26) This script is to calculate livable habitat for kōura in Lake Rotorua using the df wq_daily which is already condensed to having daily averages at depth for temp and DO

## NEED TO AMEND ALL MISSING NA DATA POINTS IN wq_daily

# First understand the pattern of missing values at 1m depth (are they all in one specific year?)
wq_daily %>%
  filter(depth_m == 1) %>%
  summarise(n_rows = n(), n_missing_DO = sum(is.na(mean_do)), pct_missing = mean(is.na(mean_do)) * 100) # 17.2% of the mean_do values are missing, with 54% occuring in 2022 and 28% in 2023.

# Is it random, or concentrated in certain periods/years?
wq_daily %>%
  filter(depth_m == 1) %>%
  group_by(Year = lubridate::year(date)) %>%
  summarise(pct_missing = mean(is.na(mean_do)) * 100)

## (09.10.26) Using short-gap interpolation ??? Check what to do with these missing values with Deniz.
library(zoo)

wq_daily <- wq_daily %>%
  arrange(Lake, Depth, Date) %>%
  group_by(Lake, Depth) %>%
  mutate(
    DO_raw          = if_else(is.nan(DO), NA_real_, DO),
    DO              = na.approx(DO_raw, x = Date, maxgap = 3, na.rm = FALSE),
    DO_interpolated = is.na(DO_raw) & !is.na(DO)
  ) %>%
  ungroup()

#----------------------------------------------------------------------------------------
## Since each depth will receive a suitability score, I need to first define suitability curves for each variable

# 1) Define suitability curves
# This step is creating a separate ramp or buffer on the side of the thresholds. This will influence the assigned scores from 0-1 depending on the 'ramp' from these buffer values
library(dplyr)

si_do_ramp <- function(do, buffer = 1) {
  case_when(
    do >= 5             ~ 1,
    do >= (5 - buffer)  ~ (do - (5 - buffer)) / buffer,   # ramps 0 to 1
    TRUE                ~ 0
  )
}

si_temp_ramp <- function(temp, buffer = 2) {
  case_when(
    temp <= 24             ~ 1,
    temp <= (24 + buffer)  ~ 1 - ((temp - 24) / buffer),  # ramps 1 to 0
    TRUE                   ~ 0
  )
}

#2) Apply the suitability curves to the wq_daily df
wq_scored <- wq_daily %>%
  select(date, depth_m, mean_temp, mean_do) %>%          # simplify first, per Step 1
  mutate(
    SI_DO   = si_do_ramp(mean_do),      
    SI_Temp = si_temp_ramp(mean_temp)   
  )

#3) Combine both thresholds into a composite HSI score
wq_scored <- wq_scored %>%
  mutate(
    HSI = pmin(SI_DO, SI_Temp)     # limiting-factor approach: worst variable controls habitat quality
    
  ) 

## (08.10.26)
# 4) Join to hypsograph's Area_at_Depth and pecentage of total lake area (at depth)
wq_joined <- wq_scored %>%
  left_join(hyps_clean %>% select(depth, Area_at_Depth, Pct_of_surface_area),
            by = c("depth_m" = "depth"))

# 5) Add date and season related columns to wq_joined df
wq_joined <- wq_joined %>%
  mutate(date = as.Date(date),
         Day = day(date),
         Month = month(date, label = TRUE, abbr = TRUE),
         Year = year(date),
         Season = case_when(
           Month %in% c("Dec", "Jan", "Feb") ~ "Summer",
           Month %in% c("Mar", "Apr", "May") ~ "Autumn",
           Month %in% c("Jun", "Jul", "Aug") ~ "Winter",
           Month %in% c("Sep", "Oct", "Nov") ~ "Spring"
         ))

# ----------------------------------------------------------------------------------------------------------------------
# 6) (12.10.26) Creating daily habitat availability, including weighted usable area 
# (partial credit to depth bands included i.e weighs differently if score was HS 0.25 vs 0.75) 
# and area above cut-off (every depth band HS >0.5)

total_area <- sum(hyps_clean$Area_at_Depth, na.rm = TRUE)

daily_habitat <- wq_joined %>%
  group_by(date) %>%
  summarise(
    wua       = sum(HSI * Area_at_Depth, na.rm = TRUE),        # weighted usable area
    suit_area = sum(Area_at_Depth[HSI >= 0.5], na.rm = TRUE),  # area above a suitability cut-off
    pct_wua   = 100 * wua / total_area,
    pct_suit  = 100 * suit_area / total_area,
    .groups = "drop"
  )

# Plot Depth-time heatmap
ggplot(wq_joined, aes(date, depth_m, fill = HSI)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_viridis_c(limits = c(0, 1), name = "HSI") +
  labs(x = NULL, y = "Depth (m)")

# Big purple block in 09/2022 is missing values for a couple months. Need to fix
#Missing blanks in heat map? Fix
# Purple blocks at the 1m in 2022/2023 from missing 1m values. Need to tidy