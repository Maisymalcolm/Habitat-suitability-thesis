HSI calculations with interpolation

## This script is to calculate livable habitat for kōura in Lake Rotorua using the df wq_daily and short-term linear interpolation
## <3, and <7 days, HS raw. 

## Using short-gap interpolation for water temperature. This adds two columns to wq_daily; <3 days interpolated and <7 days (by depth band)
install.packages("zoo")  
library(zoo)

wq_daily <- wq_daily %>%
  arrange(depth_m, date) %>%
  group_by(depth_m) %>%
  mutate(
    temp_i3 = na.approx(mean_temp, maxgap = 3, na.rm = FALSE),
    temp_i7 = na.approx(mean_temp, maxgap = 7, na.rm = FALSE)
  ) %>%
  ungroup()

#----------------------------------------------------------------------------------------
## Since each depth will receive a suitability score, I need to first define suitability curves for each variable

# Define suitability curves
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

# Apply the suitability curves to new wq_scored df (assigns scores to all three temp columns and keeps NAs as NAs )
wq_scored <- wq_daily %>%
select(date, depth_m, mean_temp, temp_i3, temp_i7, mean_do) %>%
  mutate(
    SI_DO      = if_else(is.na(mean_do),   NA_real_, si_do_ramp(mean_do)),
    SI_Temp    = if_else(is.na(mean_temp), NA_real_, si_temp_ramp(mean_temp)),
    SI_Temp_i3 = if_else(is.na(temp_i3),   NA_real_, si_temp_ramp(temp_i3)),
    SI_Temp_i7 = if_else(is.na(temp_i7),   NA_real_, si_temp_ramp(temp_i7))
  )

#Combine both thresholds into a composite HSI score
composite <- function(a, b) {
  case_when(
    !is.na(a) & !is.na(b) ~ pmin(a, b),                       # both present: the minimum
    (!is.na(a) & a == 0) | (!is.na(b) & b == 0) ~ 0,          # one missing, the other is 0: known unsuitable
    TRUE ~ NA_real_                                           # both missing, or the present one isn't limiting
  )
}

wq_scored <- wq_scored %>%
  mutate(
    HSI_raw = composite(SI_Temp,    SI_DO),
    HSI_i3  = composite(SI_Temp_i3, SI_DO),
    HSI_i7  = composite(SI_Temp_i7, SI_DO)
  )

# Save wq_scored df as an object
saveRDS(wq_scored, 'wq_scored.rds')





## ---------------------------------------------------------------------------------------------------------------
# Join to hypsograph's Area_at_Depth and pecentage of total lake area (at depth)
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
saveRDS(wq_joined, 'wq_joined.rds')
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

