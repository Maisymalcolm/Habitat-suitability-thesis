
#install.packages("pak")

# Trying to fix error messages ----------
system("xcode-select -p")
system("xcode-select --install")
pkgbuild::check_build_tools(debug = TRUE)
R.version.string
system("which gfortran")
# ---------------------------------------

pak::pak("limnotrack/ltapi", ask = FALSE)

#Installed R package 'ltapi' 
#pak::pak("limnotrack/ltapi")

# Package Install
 #install.packages("remotes")
remotes::install_github("limnotrack/ltapi")

library(ltapi)

# Download all the met/profiler data which is on the Shiny app: limnotrack.shinyapps.io/wqprofiler_bop
wq <- lt_get_wq(
  lake = "rotorua", type = "pro", api_key = "lernzmp_lakes"
)
summary(wq)

met <- lt_get_wq(
  lake = "rotorua", type = "met", api_key = "lernzmp_lakes"
)

# -------------------------------------------------------------------------------------------------------------------
# Experimental - Database queries - same data as above but using the PostgREST API instead of the dedicated endpoint.
met <- lt_fetch_all(
  table  = "met_data",
 filter = lt_filter(site_id %in% c("Rotorua"),
                     parameter %in% c("TmpAir", "WndSpd"),
                    qc_level == 400L, # Quality control level 400 (Automated validated data)
                    datetime >= "2025-01-01"
                   ),
  order  = "datetime.asc"
)

# Exploring data frames
 head(met) # Shows the first 6 rows of df
tail(met) # Shows the last 6 rows of df
str(met) # Shows the structure of an object
summary(met)

str(wq)
