# Self-contained script to generate the `buoy_wq` table (formerly the
# `old_buoy_wq` target): daily-mean DO, pH and water temperature by depth from
# the QC'd old Rotorua buoy data.
#
# Requires: readr, dplyr, tidyr, lubridate, tidyselect
# Output:   `buoy_wq` (Date, depth, var_aeme, value), also saved to `out_file`

url <- "https://github.com/limnotrack/f_rotorua/releases/download/v0.0.1/rotorua_data_qc.zip"
zip_file <- "rotorua_data_qc.zip"

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# Functions ----

generate_var_ref <- function(var, z_relative, reference = "d") {
  ref_str <- paste0(reference, z_relative * 100)  # Convert meters to cm
  paste(var, ref_str, sep = "_")
}

map_sensors <- function(site_devices, device_var, device_position) {
  site_devices |>
    dplyr::select(site, device_id, date_from, date_to) |>
    dplyr::left_join(device_var, by = "device_id") |>
    dplyr::left_join(device_position, by = c("site", "device_id")) |>
    dplyr::mutate(var_ref_id = generate_var_ref(var_abbr, z_relative, reference)) |>
    dplyr::arrange(var_ref_id, date_from)
}

map_data_to_devices <- function(data, site_devices, device_var,
                                device_position, variables = NULL) {
  sensor_map <- map_sensors(site_devices = site_devices,
                            device_var = device_var,
                            device_position = device_position)

  if (!is.null(variables)) {
    variables <- variables |>
      dplyr::select(abbr, label)
    sensor_map <- sensor_map |>
      dplyr::left_join(variables, by = c("var_abbr" = "abbr"))
  }

  sensor_map |>
    dplyr::group_by(device_id) |>
    dplyr::left_join(data, by = c("var_ref_id", "site"),
                     relationship = "many-to-many") |>
    dplyr::filter(datetime >= date_from & datetime <= date_to) |>
    dplyr::ungroup() |>
    dplyr::arrange(var_ref_id, datetime) |>
    dplyr::select(-c(date_from, date_to, reference, z_relative))
}

load_buoy_data <- function(zip_file) {

  # Unzip into a fresh temp folder
  target_dir <- tempfile("buoy_")
  dir.create(target_dir)
  unzip(zip_file, exdir = target_dir)
  dir <- file.path(target_dir, tools::file_path_sans_ext(basename(zip_file)))

  read <- function(f) readr::read_csv(file.path(dir, f), col_types = readr::cols())

  site_devices <- read("site_devices.csv")
  device_var <- read("device_variable.csv")
  device_position <- read("device_position.csv")
  variable_ref <- read("variables.csv")

  data_wide <- readr::read_csv(file.path(dir, "rotorua_qc.csv"),
                               show_col_types = FALSE) |>
    dplyr::mutate(
      datetime = lubridate::force_tz(datetime, tzone = "Etc/GMT-12")
    )

  data_wide |>
    tidyr::pivot_longer(
      cols = tidyselect::matches("^(qc_value|qc_code|qc_flag)_"),
      names_to = c(".value", "var_ref_id"),
      names_pattern = "^(qc_value|qc_code|qc_flag)_(.+)$"
    ) |>
    map_data_to_devices(site_devices = site_devices,
                        device_var = device_var,
                        device_position = device_position,
                        variables = variable_ref)
}

buoy_wq_data <- function(data, wq_vars = c("c_do", "t_wtr")) {
  data |>
    dplyr::filter(var_abbr %in% wq_vars, !is.na(qc_value)) |>
    dplyr::mutate(
      depth = as.numeric(gsub("\\D", "", var_ref_id)) / 100,
      var_aeme = dplyr::case_when(
        var_abbr == "c_do" ~ "CHM_oxy",
        var_abbr == "c_ph" ~ "CHM_ph",
        var_abbr == "t_wtr" ~ "HYD_temp"
      ),
      value = qc_value,
      Date = lubridate::ceiling_date(datetime, unit = "day")
    ) |>
    dplyr::group_by(Date, depth, var_aeme) |>
    dplyr::summarise(
      value = mean(value, na.rm = TRUE),
      .groups = "drop"
    )
}

# Run ----

if (!file.exists(zip_file)) {
  dir.create(dirname(zip_file), recursive = TRUE, showWarnings = FALSE)
  download.file(url, destfile = zip_file, mode = "wb")
}

# Load all the data (takes some time)
buoy_data <- load_buoy_data(zip_file)
unique(buoy_data$var_abbr) # All variable names

# Subset to WQ parameters
wq_vars <- c(
  "t_wtr", # Water temperature (degC)
  "c_do" # Dissolved oxygen (mg/L)
)
buoy_wq <- buoy_wq_data(buoy_data, wq_vars = wq_vars)
head(buoy_wq)


# ----------- Okataina data -------------------------------------------------------------------------------------------------------
# R packages for accessing LERNZmp data - https://limnotrack.shinyapps.io/LERNZmp/
remotes::install_github("limnotrack/AEME")
remotes::install_github("limnotrack/aemetools")
lake_id <- "LID54731"
aeme <- aemetools::get_aeme(id = lake_id, api_key = "lernzmp_lakes")

# Extract observations
obs <- aeme |>
  AEME::upgrade_aeme() |> 
  AEME::get_obs()

head(obs)
# Summarise temperature and oxygen
obs |> 
  dplyr::filter(
    var_aeme %in% c("HYD_temp", "CHM_oxy")
  ) |> 
  dplyr::group_by(var_aeme) |> 
  dplyr::summarise(
    start = min(Date),
    end = max(Date),
    n = dplyr::n(),
    min = min(value),
    mean = mean(value),
    median = median(value),
    max = max(value),
    sd = sd(value),
    min_depth = min(depth),
    max_depth = max(depth)
  ) 

