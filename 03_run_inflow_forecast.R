config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_flare.yml"))
run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))

# Set working directories for your system
config$file_path$noaa_directory <- file.path(lake_directory, "forecasted_drivers", config$met$forecast_met_model)
config$file_path$inflow_directory <- file.path(lake_directory, "forecasted_drivers", config$inflow$forecast_inflow_model)
config$file_path$configuration_directory <- file.path(lake_directory, "configuration")
config$file_path$execute_directory <- file.path(lake_directory, "flare_tempdir")
config$file_path$forecast_output_directory <- file.path(lake_directory, "forecast_output")
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$run_config <- file.path(lake_directory, "configuration", "flarer/configure_run.yml")


config$run_config <- run_config
# Set up timings
#Weather Drivers
start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
if(is.na(config$run_config$forecast_start_datetime)){
  end_datetime <- lubridate::as_datetime(config$run_config$end_datetime)
  forecast_start_datetime <- end_datetime
}else{
  forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  end_datetime <- forecast_start_datetime + lubridate::days(config$run_config$forecast_horizon)
}
forecast_hour <- lubridate::hour(forecast_start_datetime)
if(forecast_hour < 10){forecast_hour <- paste0("0",forecast_hour)}
forecast_path <- file.path(config$file_path$noaa_directory,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)


#Weather Drivers

noaa_forecast_path <- file.path(config$file_path$noaa_directory, config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)

message("Forecasting inflow and outflows")
source(paste0(lake_directory, "/R/forecast_inflow_outflows.R"))
# Forecast Inflows

config$future_inflow_flow_coeff <- c(0.0010803, 0.9478724, 0.3478991)
config$future_inflow_flow_error <- 0.00965
config$future_inflow_temp_coeff <- c(0.20291, 0.94214, 0.04278)
config$future_inflow_temp_error <- 0.943

forecast_files <- list.files(noaa_forecast_path, full.names = TRUE)
forecast_inflows_outflows(inflow_obs = file.path(config$file_path$qaqc_data_directory, "inflow_postQAQC.csv"),
                          forecast_files = forecast_files,
                          obs_met_file = file.path(config$file_path$qaqc_data_directory,"observed-met_fcre.nc"),
                          output_dir = config$file_path$inflow_directory,
                          inflow_model = config$inflow$forecast_inflow_model,
                          inflow_process_uncertainty = FALSE,
                          forecast_location = config$file_path$forecast_output_directory,
                          config = config)
