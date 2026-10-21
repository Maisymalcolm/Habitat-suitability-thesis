## This script is to open the Lake Rotorua data needed to plot a hypsographic curve
## I've attemped to clean/simplify the data to fit my study needs by making max lake depth 25m, and calculating area (m^2) at the bottom of each metre band.

## Loading in Lake Rotorua bathymetric data from LERNZmp

list.files("lernzmp")


metadata <- read.csv("Lake Rotorua data/lernzmp/LERNZmp_lake_metadata.csv")
head(metadata)

dplyr::glimpse(metadata) ## This seems to just have a lot of general data about all the lakes in the LERNZmp platform. 
## Think I need to install the aeme package and use that to open the lake ID and specific bathymetric data from there...

library(AEME)
help(package = "AEME") 
data(package = "AEME")

## (30.09.2026) Now I'll load the AEME object (which contains lake metadata, model output and model controls). Then i'll be able to use the lake section to see the lake metadata
#| label: lernzmp-load-aeme
aeme <- readRDS("Lake Rotorua data/lernzmp/LID11133.rds")
class(aeme)
aeme # By running this alone you can see the contents of the object.

plot(aeme, "lake") # This can be used just to plot a visual of the lake location

## Access AEME input data - includes hypsograph data (lake bathymetry etc)
inp <- input(aeme)
names(inp)

## Hypsograph data
hyps <- inp$hypsograph
head(hyps)
str(hyps)

# Plot hypsograph
library(ggplot2)
ggplot(hyps, aes(x = area, y = depth)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  labs(y = "Depth (m)", x = "Area (m^2)") +
  theme_bw()

hypsographdata <- hyps

# Clean summarise hypsograph data to have max depth of 25m, and one row with total area per metre bandwidth
library(dplyr)

hyps_clean <- hypsographdata %>%
  filter(depth <= 0) %>%                        # keep only true below-surface points
  mutate(depth = abs(depth)) %>%                 # now convert to positive depth
  filter(depth <= 30) %>%                        # This line sets the max depth to 30m
  mutate(depth = round(depth)) %>%
  group_by(depth) %>%
  summarise(area = mean(area), .groups = "drop") %>%
  arrange(depth) %>%
  mutate(Area_at_Depth = abs(area - lag(area, default = first(area)))) %>%
  mutate(Area_at_Depth = if_else(depth == 0, 0, Area_at_Depth))

# check hyps_clean
hyps_clean

## (01.10.2026) 
head(hyps_clean)

# Save hyps_clean data as an RDS (a saved df)
saveRDS(hyps_clean, "hyps_clean.rds") 

# Plotting the hyps_clean hypsographic curve (converted negative values to positve; remove postive values above 0; produces area at depth values)
ggplot(hyps_clean, aes(x = area, y = depth)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  scale_y_reverse() +
  labs(y = "Depth (m)", x = "Area (m^2)") +
  theme_bw()

hyps_clean

## Now visualise as a table in the console
print(hyps_clean, n = Inf)

library(knitr)

# Adding a column to represent percentage of total lake area that each band makes up
hyps_clean <- hyps_clean %>%  
  mutate(Pct_of_surface_area = round(Area_at_Depth / max(area) * 100, 1))

names(hyps_clean)

hyps_clean %>%
  kable(col.names = c("Depth (m)", "Area (m²)", "Area at Depth (m²)", "% of Surface Area"),
        digits = 0,
        caption = "Hypsographic table for Lake Rotorua")
 
## Format for outputting table
library(gt)

hyps_clean %>%
  gt() %>%
  cols_label(
    depth = "Depth (m)",
    area = "Area (m²)",
    Area_at_Depth = "Area at Depth (m²)",
    Pct_of_surface_area = "% of Surface Area" 
  ) %>%
  fmt_number(columns = everything(), decimals = 0) %>%
  tab_header(title = "Hypsographic table for Lake Rotorua") %>%
  gtsave("hypsographic_table.png")   # saves directly to your working directory

##--------------------ignore this section for now------------------------------------
library(ltapi)

lt_tables()

lt_schema("lernzmp_lakes")

## Fetching spatial data
contours <- lt_fetch(
  table  = "lake_contours",
  filter = lt_filter(lernzmp_id == "LID11133")
)

class(contours) 

metadata_lakeRotorua <- metadata |> 
  dplyr::filter(aeme_file %in% c("LID11133"))
metadata

utils::data("aeme_parameters")
aeme_parameters
##-----------------------------------------------------------------------------------



