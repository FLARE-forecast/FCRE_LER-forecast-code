lake_directory <- getwd()

update_run_config <- FALSE
if(!exists("lake_directory")){
  stop("Missing lake_directory variable")
}

if(!exists("update_run_config")){
  stop("Missing update_run_config variable")
}

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

# Configure run settings ----
run_config$forecast_start_datetime <- format(Sys.Date(), "%Y-%m-%d %H:%M:%S")
run_config$start_datetime <- format((Sys.Date() - lubridate::days(7)), "%Y-%m-%d %H:%M:%S")
run_config$forecast_horizon

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
noaa_forecast_path <- file.path(config$file_path$noaa_directory,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)


forecast_files <- list.files(noaa_forecast_path, full.names = TRUE)

if(length(forecast_files) > 0){



  if(!dir.exists(config$file_path$execute_directory)){
    dir.create(config$file_path$execute_directory)
  }

  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, "FLAREr", config$model_settings$par_config_file), col_types = readr::cols())
  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, "FLAREr", config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, "FLAREr", config$model_settings$states_config_file), col_types = readr::cols())

  cleaned_observations_file_long <- file.path(config$file_path$qaqc_data_directory,"observations_postQAQC_long.csv")
  cleaned_inflow_file <- file.path(config$file_path$qaqc_data_directory, "/inflow_postQAQC.csv")
  observed_met_file <- file.path(config$file_path$qaqc_data_directory,"observed-met_fcre.nc")

  # Met files for running GLM

  met_out <- FLAREr::generate_glm_met_files(obs_met_file = observed_met_file,
                                            out_dir = config$file_path$execute_directory,
                                            forecast_dir = noaa_forecast_path,
                                            config = config)

  # Inflow Drivers for GLM
  if(config$model_settings$model_name == "glm"){

  inflow_forecast_path <- file.path(config$file_path$inflow_directory, config$inflow$forecast_inflow_model,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)

  inflow_outflow_files <- FLAREr::create_glm_inflow_outflow_files(inflow_file_dir = inflow_forecast_path,
                                                                  inflow_obs = cleaned_inflow_file,
                                                                  working_directory = config$file_path$execute_directory,
                                                                  config = config,
                                                                  state_names = states_config$state_names)

  inflow_file_names <- inflow_outflow_files$inflow_file_name
  outflow_file_names <- inflow_outflow_files$outflow_file_name

  management <- NULL

  }else if(config$model_settings$model_name == "glm_aed"){
    file.copy(file.path(config_obs$data_location, "manual-data/FCR_weir_inflow_2013_2019_20200828_allfractions_2poolsDOC.csv"),
              file.path(config$file_path$execute_directory, "FCR_weir_inflow_2013_2019_20200624_allfractions_2poolsDOC.csv"))

    file.copy(file.path(config_obs$data_location, "manual-data/FCR_wetland_inflow_2013_2019_20200828_allfractions_2DOCpools.csv"),
              file.path(config$file_path$execute_directory, "FCR_wetland_inflow_2013_2019_20200713_allfractions_2DOCpools.csv"))

    file.copy(file.path(config_obs$data_location, "manual-data/FCR_SSS_inflow_2013_2019_20200701_allfractions_2DOCpools.csv"),
              file.path(config$file_path$execute_directory, "FCR_SSS_inflow_2013_2019_20200701_allfractions_2DOCpools.csv"))

    file.copy(file.path(config_obs$data_location, "manual-data/FCR_spillway_outflow_SUMMED_WeirWetland_2013_2019_20200615.csv"),
              file.path(config$file_path$execute_directory, "FCR_spillway_outflow_SUMMED_WeirWetland_2013_2019_20200615.csv"))

    file1 <- file.path(config$file_path$execute_directory, "FCR_weir_inflow_2013_2019_20200624_allfractions_2poolsDOC.csv")
    file2 <- file.path(config$file_path$execute_directory, "FCR_wetland_inflow_2013_2019_20200713_allfractions_2DOCpools.csv")
    inflow_file_names <- tibble(file1 = file1,
                                file2 = file2,
                                file3 = "sss_inflow.csv")
    outflow_file_names <- tibble(file_1 = file.path(config$file_path$execute_directory, "FCR_spillway_outflow_SUMMED_WeirWetland_2013_2019_20200615.csv"),
                                 file_2 = "sss_outflow.csv")

    management <- FLAREr::generate_oxygen_management(config = config)
  }

  #Create observation matrix
  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                   obs_config,
                                   config)

  states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)

  model_sd <- FLAREr::initiate_model_error(config, states_config)

  init <- FLAREr::generate_initial_conditions(states_config,
                                              obs_config,
                                              pars_config,
                                              obs,
                                              config,
                                              restart_file = config$run_config$restart_file,
                                              historical_met_error = met_out$historical_met_error)
  #Run EnKF
  da_forecast_output <- FLAREr::run_da_forecast(states_init = init$states,
                                       pars_init = init$pars,
                                       aux_states_init = init$aux_states_init,
                                       obs = obs,
                                       obs_sd = obs_config$obs_sd,
                                       model_sd = model_sd,
                                       working_directory = config$file_path$execute_directory,
                                       met_file_names = met_out$filenames,
                                       inflow_file_names = inflow_outflow_files$inflow_file_names,
                                       outflow_file_names = inflow_outflow_files$outflow_file_names,
                                       config = config,
                                       pars_config = pars_config,
                                       states_config = states_config,
                                       obs_config = obs_config,
                                       management,
                                       da_method = config$da_setup$da_method,
                                       par_fit_method = config$da_setup$par_fit_method)

  # Save forecast
  saved_file <- FLAREr::write_forecast_netcdf(da_forecast_output = da_forecast_output,
                                              forecast_output_directory = config$file_path$forecast_output_directory)

  #Create EML Metadata
  FLAREr::create_flare_metadata(file_name = saved_file,
                                da_forecast_output = da_forecast_output)

  #Clean up temp files and large objects in memory
  unlist(config$file_path$execute_directory, recursive = TRUE)

  rm(da_forecast_output)
  gc()

  if(update_run_config){
    run_config$start_datetime <- run_config$forecast_start_datetime
    run_config$forecast_start_datetime <- as.character(lubridate::as_datetime(run_config$forecast_start_datetime) + lubridate::days(1))
    if(lubridate::hour(run_config$forecast_start_datetime) == 0){
      run_config$forecast_start_datetime <- paste(run_config$forecast_start_datetime, "00:00:00")
    }
    run_config$restart_file <- saved_file
    yaml::write_yaml(run_config, file = file.path(config$file_path$run_config))
  }
}else{
  if(update_run_config){
    run_config$forecast_start_datetime <- as.character(lubridate::as_date(run_config$forecast_start_datetime) + lubridate::days(1))
    yaml::write_yaml(run_config, file = file.path(config$file_path$run_config,"run_configuration.yml"))
  }
}

