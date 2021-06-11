#Note: lake_directory need to be set prior to running this script

if(!exists("lake_directory")){
  stop("Missing lake_directory variable")
}

config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr",configuration_file))
run_config <- yaml::read_yaml(config$file_path$run_config)

if(!is.na(run_config$restart_file)){
  restart_file <- run_config$restart_file
}else{
  restart_file <- saved_file #From 04_run_flarer_forecast
}

FLAREr::plotting_general(file_name = restart_file,
                         qaqc_data_directory = config$file_path$qaqc_data_directory)

source(file.path(lake_directory, "R","manager_plot.R"))

if(run_config$forecast_horizon == 16){
  manager_plot(file_name = run_config$restart_file,
               qaqc_data_directory = config$file_path$qaqc_data_directory,
               focal_depths = c(1, 5, 8))
}

