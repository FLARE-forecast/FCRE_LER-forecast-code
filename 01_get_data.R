#Note: lake_directory need to be set prior to running this script

lake_directory <- getwd() # Presuming you are using an an Rproject

if(!exists("lake_directory")){
  stop("Missing lake_directory variable")
}

config <- yaml::read_yaml(file.path(lake_directory,"configuration", "observation_processing", "observation_processing.yml"))

# Set working directories relative to this project
# Don't forget to pull in the git submodules
# In the terminal run:
# `git submodule init`
# `git submodule update`

config$data_location <- file.path(lake_directory, "data_raw")



if(!file.exists(file.path(config$data_location, config$realtime_insitu_location))){
  stop("Missing temperature data GitHub repo")
}
if(!file.exists(file.path(config$data_location, config$realtime_met_station_location))){
  stop("Missing met station data GitHub repo")
}
if(!file.exists(file.path(lake_directory, config$noaa_location))){
  stop("Missing NOAA forecast GitHub repo")
}
if(!file.exists(file.path(config$data_location, config$manual_data_location))){
  stop("Missing Manual data GitHub repo")
}

if(!file.exists(file.path(config$data_location, config$realtime_inflow_data_location))){
  stop("Missing Inflow data GitHub repo")
}
