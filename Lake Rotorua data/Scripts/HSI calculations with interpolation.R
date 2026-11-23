HSI calculations with interpolation

## This script is to calculate livable habitat for kōura in Lake Rotorua using the df wq_daily and short-term linear interpolation
## <3, and <7 days, HS raw. 

## Using short-gap interpolation for water temperature. Adds two columns to wq_daily: <3 days interpolated and <7 days (by depth band)
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


# Join to hypsograph's Area_at_Depth and pecentage of total lake area (at depth)
wq_joined <- wq_scored %>%
  left_join(hyps_clean %>% select(depth, Area_at_Depth, Pct_of_surface_area),
            by = c("depth_m" = "depth"))

#Add date and season columns to wq_joined
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


# Calculate habitat suitability using HSI_raw, HSI_i3 & HSI_I7

library(tidyr)
library(ggplot2)
library(lubridate)

# ---- 0. Area denominators -------------------------------------------------
band_area <- wq_joined %>% distinct(depth_m, Area_at_Depth)
nrow(band_area)                                  # should equal the number of depth bands (20)

assessed_area <- sum(band_area$Area_at_Depth)     # area of the bands that have HSI scores
total_area    <- sum(hyps_clean$Area_at_Depth, na.rm = TRUE)   # whole lake per the hypsograph
c(assessed_area = assessed_area, total_area = total_area)    

lake_area     <- 80000000                        # known whole-lake area (m²), cite your source

denominator  <- assessed_area
coverage_pct <- 100 * assessed_area / lake_area  # report this in the methods -> coverage % = 90.14177 of total lake area
coverage_pct
# Choose the denominator for "% of lake area":
#   assessed_area = % of the scored depth range (max 100%)
#   total_area    = % of the whole lake (unscored depths count as no habitat)
denominator <- assessed_area

# Since interpolation didn't add any days, we'll stick to the raw version as the main version
main_version <- "HSI_raw"

# ---- 1. Daily habitat availability, for all three HSI versions -------------
daily_habitat <- wq_joined %>%
  select(date, depth_m, Area_at_Depth, HSI_raw, HSI_i3, HSI_i7) %>%
  pivot_longer(starts_with("HSI_"), names_to = "version", values_to = "HSI") %>%
  group_by(version, date) %>%
  summarise(
    all_valid = all(!is.na(HSI)),    # a day is only scored if every depth band has an HSI
    wua       = if_else(all_valid, sum(HSI * Area_at_Depth), NA_real_),            # weighted usable area
    suit_area = if_else(all_valid, sum(Area_at_Depth[HSI >= 0.5]), NA_real_),      # area with HSI >= 0.5
    .groups = "drop"
  ) %>%
  mutate(
    pct_wua  = 100 * wua / denominator,
    pct_suit = 100 * suit_area / denominator,
    Year  = year(date),
    Month = month(date, label = TRUE, abbr = TRUE),
    Season = case_when(
      Month %in% c("Dec", "Jan", "Feb") ~ "Summer",
      Month %in% c("Mar", "Apr", "May") ~ "Autumn",
      Month %in% c("Jun", "Jul", "Aug") ~ "Winter",
      Month %in% c("Sep", "Oct", "Nov") ~ "Spring"
    ),
    Season     = factor(Season, levels = c("Summer", "Autumn", "Winter", "Spring")),
    SeasonYear = if_else(Month == "Dec", Year + 1, Year)   # December belongs with the following Jan/Feb
  )

# Days that are partly scored (some bands NA) are set to NA, so check how many there are
daily_habitat %>% count(version, all_valid)

# ---- 2. Do the three versions differ? -------------------------------------
daily_habitat %>%
  select(date, version, pct_wua) %>%
  pivot_wider(names_from = version, values_from = pct_wua) %>%
  summarise(
    valid_days_raw = sum(!is.na(HSI_raw)),
    valid_days_i3  = sum(!is.na(HSI_i3)),
    valid_days_i7  = sum(!is.na(HSI_i7)),
    extra_days_i3  = valid_days_i3 - valid_days_raw,
    extra_days_i7  = valid_days_i7 - valid_days_raw,
    max_diff_i3    = max(abs(HSI_i3 - HSI_raw), na.rm = TRUE),   # on days valid in both
    max_diff_i7    = max(abs(HSI_i7 - HSI_raw), na.rm = TRUE)
  )

main <- daily_habitat %>% filter(version == main_version)


# HSI through time
# Uses `main` (daily_habitat filtered to your chosen HSI version).
# pct_wua / 100 is the area-weighted mean HSI across depth bands, a 0-1 score for the whole lake.

# ---- Plot 1: one line through time, summers shaded 
summer_bands <- tibble(SeasonYear = sort(unique(main$SeasonYear))) %>%
  mutate(
    xmin = pmax(as.Date(paste0(SeasonYear - 1, "-12-01")), min(main$date)),
    xmax = pmin(as.Date(paste0(SeasonYear, "-03-01")), max(main$date))
  ) %>%
  filter(xmin < xmax)

ggplot(main, aes(date, pct_wua / 100)) +
  geom_rect(
    data = summer_bands,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE, fill = "orange", alpha = 0.15
  ) +
  geom_line(colour = "steelblue", linewidth = 0.5) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +
  labs(
    x = NULL,
    y = "Daily habitat suitability index",
  ) +
    theme_minimal() +
  theme(panel.grid = element_blank())

# Plotting daily HSI, one panel per year, coloured by season 
ggplot(main, aes(date, pct_wua / 100, colour = Season)) +
  geom_point(size = 0.7) +
  facet_wrap(~Year, ncol = 1, scales = "free_x") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = NULL, y = "Daily HSI", colour = NULL) +
  theme_minimal()


## plotting weighted usable area through time (same as plot one but changed to WUA)
ggplot(main, aes(date, pct_wua)) +
  geom_rect(
    data = summer_bands,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE, fill = "orange", alpha = 0.15
  ) +
  geom_line(colour = "steelblue", linewidth = 0.5) +
  scale_y_continuous(limits = c(0, 100), expand = c(0.01, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +
  labs(
    x = NULL,
    y = "Weighted usable lake area (%)",
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())


# Calculate valid days by season and year
coverage_table <- main %>%
  group_by(SeasonYear, Season) %>%
  summarise(
    days_in_record = n(),
    valid_days     = sum(!is.na(pct_wua)),
    pct_valid      = round(100 * valid_days / days_in_record, 1),
    .groups = "drop"
  )
print(coverage_table, n = Inf)
write.csv(coverage_table, "valid_days_by_season.csv", row.names = FALSE)

read.csv(valid_days_by_season.csv)
# NEED TO DOUBLE CHECK THAT VALID DAYS ARE THE SAME AS DAY WHERE ALL BAND READINGS ARE AVAILABLE
# Getting numbers of missing valid days by year/season
# Overall coverage
coverage_table %>%
  summarise(pct_valid_overall = round(100 * sum(valid_days) / sum(days_in_record), 1))

# Coverage by season, pooled across years
coverage_table %>%
  group_by(Season) %>%
  summarise(pct_valid = round(100 * sum(valid_days) / sum(days_in_record), 1))

# The five lowest-coverage season-years
coverage_table %>% arrange(pct_valid) %>% slice_head(n = 5)




