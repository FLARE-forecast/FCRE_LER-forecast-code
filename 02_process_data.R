#Note: lake_directory need to be set prior to running this script
lake_directory <- getwd()

config_obs <- yaml::read_yaml(file.path(lake_directory,"configuration","observation_processing","observation_processing.yml"))
config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_flare.yml"))

# Set working directories for your system
config$file_path$noaa_directory <- file.path(lake_directory, "forecasted_drivers", config$met$forecast_met_model)
config$file_path$inflow_directory <- file.path(lake_directory, "forecasted_drivers", config$inflow$forecast_inflow_model)
config$file_path$configuration_directory <- file.path(lake_directory, "configuration")
config$file_path$execute_directory <- file.path(lake_directory, "flare_tempdir")
config$file_path$forecast_output_directory <- file.path(lake_directory, "forecast_output")
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$run_config <- file.path(lake_directory, "configuration", "flarer/configure_run.yml")

config_obs$data_location <- file.path(lake_directory, "data_raw")

library(tidyverse)
library(lubridate)

files.sources <- list.files(file.path(lake_directory, "R"), full.names = TRUE)
files.sources <- files.sources[-grep("noaaGEFSpoint", files.sources)]
sapply(files.sources, source)

if(is.null(config_obs$met_file)){
  met_qaqc(realtime_file = file.path(config_obs$data_location, config_obs$met_raw_obs_fname[1]),
           qaqc_file = file.path(config_obs$data_location, config_obs$met_raw_obs_fname[2]),
           cleaned_met_file_dir = config$file_path$qaqc_data_directory,
           input_file_tz = "EST",
           nldas = file.path(config_obs$data_location, config_obs$nldas))
}else{
  file.copy(file.path(config_obs$data_location,config_obs$met_file), cleaned_met_file, overwrite = TRUE)
}

cleaned_inflow_file <- file.path(config$file_path$qaqc_data_directory, "inflow_postQAQC.csv")

if(is.null(config_obs$inflow1_file)){
  inflow_qaqc(realtime_file = file.path(config_obs$data_location, config_obs$inflow_raw_file1[1]),
              qaqc_file = file.path(config_obs$data_location, config_obs$inflow_raw_file1[2]),
              nutrients_file = file.path(config_obs$data_location, config_obs$nutrients_fname),
              cleaned_inflow_file,
              input_file_tz = "EST")
}else{
  file.copy(file.path(config_obs$data_location,config_obs$inflow1_file), cleaned_inflow_file, overwrite = TRUE)
}


cleaned_observations_file_long <- paste0(config$file_path$qaqc_data_directory,
                                         "/observations_postQAQC_long.csv")
if(is.null(config_obs$combined_obs_file)){
  in_situ_qaqc(insitu_obs_fname = file.path(config_obs$data_location,config_obs$insitu_obs_fname),
               data_location = config_obs$data_location,
               maintenance_file = file.path(config_obs$data_location,config_obs$maintenance_file),
               ctd_fname = file.path(config_obs$data_location,config_obs$ctd_fname),
               nutrients_fname =  file.path(config_obs$data_location, config_obs$nutrients_fname),
               secchi_fname = file.path(config_obs$data_location, config_obs$secchi_fname),
               cleaned_observations_file_long = cleaned_observations_file_long,
               lake_name_code = config$location$lake_name_code,
               config = config_obs)
}else{
  file.copy(file.path(config_obs$data_location,config_obs$combined_obs_file), cleaned_observations_file_long, overwrite = TRUE)
}

file.copy(file.path(config_obs$data_location,config_obs$sss_fname),
          file.path(config$file_path$qaqc_data_directory, basename(config_obs$sss_fname)))

if(!is.null(config_obs$specified_sss_inflow_file)){
  file.copy(file.path(config_obs$data_location,config_obs$specified_sss_inflow_file), file.path(config$file_path$qaqc_data_directory,basename(config_obs$specified_sss_inflow_file)))
}
if(!is.null(config_obs$specified_sss_outflow_file)){
  file.copy(file.path(config_obs$data_location,config_obs$specified_sss_outflow_file), file.path(config$file_path$qaqc_data_directory,basename(config_obs$specified_sss_outflow_file)))
}
if(!is.null(config_obs$specified_metfile)){
  file.copy(file.path(config_obs$data_location,config_obs$specified_metfile), file.path(config$file_path$qaqc_data_directory,basename(config_obs$specified_metfile)))
}
if(!is.null(config_obs$specified_inflow1)){
  file.copy(file.path(config_obs$data_location, config_obs$specified_inflow1), file.path(config$file_path$qaqc_data_directory,basename(config_obs$specified_inflow1)))
}

if(!is.null(config_obs$specified_inflow2)){
  file.copy(file.path(config_obs$data_location,config_obs$specified_inflow2), file.path(config$file_path$qaqc_data_directory,basename(config_obs$specified_inflow2)))
}
if(!is.null(config_obs$specified_outflow1)){
  file.copy(file.path(config_obs$data_location,config_obs$specified_outflow1), file.path(config$file_path$qaqc_data_directory,basename(config_obs$specified_outflow1)))
}

