lake_directory <- "/Users/quinn/Downloads/FCRE-forecast-code"
update_run_config <- TRUE #TRUE is used for an iterative workflow
configuration_file <- "configure_flare.yml"
#source(file.path(lake_directory, "01_get_data.R"))

source(file.path(lake_directory, "02_process_data.R"))

source(file.path(lake_directory, "03_run_inflow_forecast.R"))

source(file.path(lake_directory, "04_run_flarer_forecast.R"))

source(file.path(lake_directory, "05_visualize.R"))
