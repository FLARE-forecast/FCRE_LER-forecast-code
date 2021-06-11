#Note: lake_directory need to be set prior to running this script

if(!exists("lake_directory")){
  stop("Missing lake_directory variable")
}

config <- yaml::read_yaml(file.path(lake_directory,"configuration","flarer","configure_flare.yml"))
run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","flarer","configure_run.yml"))


# Set working directories for your system
config$file_path$noaa_directory <- file.path(lake_directory, "forecasted_drivers", config$met$forecast_met_model)
config$file_path$inflow_directory <- file.path(lake_directory, "forecasted_drivers", config$inflow$forecast_inflow_model)
config$file_path$configuration_directory <- file.path(lake_directory, "configuration")
config$file_path$execute_directory <- file.path(lake_directory, "flare_tempdir")
config$file_path$forecast_output_directory <- file.path(lake_directory, "forecast_output")
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$run_config <- file.path(lake_directory, "configuration", "flarer/configure_run.yml")

if(!exists("saved_file")){
  fils <- list.files(config$file_path$forecast_output_directory, pattern = ".nc",
                     full.names = TRUE)
  if(length(fils) > 0) {
    saved_file <- fils[1]
  } else {
    stop("Need to run '04_run_flarer_forecast.R'")
  }
}


if(!is.na(run_config$restart_file)){
  restart_file <- run_config$restart_file
}else{
  restart_file <- saved_file #From 04_run_flarer_forecast
}

FLAREr::plotting_general(file_name = restart_file,
                         qaqc_data_directory = config$file_path$qaqc_data_directory)

source(file.path(lake_directory, "R","manager_plot.R"))

if(run_config$forecast_horizon == 16){
  manager_plot(file_name = restart_file,
               qaqc_data_directory = config$file_path$qaqc_data_directory,
               focal_depths = c(1, 5, 8))
}

