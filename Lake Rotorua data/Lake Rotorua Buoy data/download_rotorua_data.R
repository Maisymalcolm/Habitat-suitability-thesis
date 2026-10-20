# Package Install
# install.packages("remotes")
# remotes::install_github("limnotrack/ltapi")

library(ltapi)

# Download all the met/profiler data which is on the Shiny app: limnotrack.shinyapps.io/wqprofiler_bop
wq <- lt_get_wq(
  lake = "rotorua", type = "pro", api_key = "lernzmp_lakes"
)
summary(wq)

met <- lt_get_wq(
  lake = "rotorua", type = "met", api_key = "lernzmp_lakes"
)


# Experimental - Database queries - same data as above but using the PostgREST API instead of the dedicated endpoint.
# met <- lt_fetch_all(
#   table  = "met_data",
#   filter = lt_filter(site_id %in% c("Rotorua"),
#                      parameter %in% c("TmpAir", "WndSpd"),
#                      qc_level == 400L, # Quality control level 400 (Automated validated data)
#                      datetime >= "2025-01-01"
#                      ),
#   order  = "datetime.asc"
# )
#
# head(met)
# tail(met)
