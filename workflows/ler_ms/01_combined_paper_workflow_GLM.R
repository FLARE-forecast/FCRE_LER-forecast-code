#remotes::install_github("FLARE-forecast/GLM3r", ref = "FLARErLERv1")
#remotes::install_github("FLARE-forecast/FLARErLER", ref = "v2.2.0")

##'
# Load in the required functions for processing the data
#renv::restore()
library(magrittr)
library(dplyr)
library(lubridate)
set.seed(100)
config_set_name <- "ler_ms"
run_ler_flare <- TRUE
run_clim_null <- FALSE
run_persistence_null <- FALSE
start_from_scratch <- TRUE
time_start_index <- 1
#Set use_archive = FALSE unless you have read/write credentials for the remote
#s3 bucket that is set up for running FLARE.
use_archive <- FALSE
ensemble_size <- 200
model <- "GLM"
ncore <- parallel::detectCores() - 1
lake_directory <- here::here()
source(file.path(lake_directory, "R","forecast_inflow_outflows.R"))


if(use_archive){
  use_s3 <- FALSE
}else{
  Sys.setenv('AWS_DEFAULT_REGION' = 's3',
             'AWS_S3_ENDPOINT' = 'flare-forecast.org',
             'USE_HTTPS' = TRUE)
  use_s3 <- FALSE
}


sim_names <- paste0("ms1_ler_flare_", model)
config_files <- paste0("configure_flare.yml")

#num_forecasts <- 20
num_forecasts <- 34 * 7 - 1
days_between_forecasts <- 1
forecast_horizon <- 35 #32
starting_date <- as_date("2021-03-01")
second_date <- starting_date + months(1) - days(days_between_forecasts)

start_dates <- rep(NA, num_forecasts)
start_dates[1:2] <- c(starting_date, second_date)
for(i in 3:num_forecasts){
  start_dates[i] <- as_date(start_dates[i-1]) + days(days_between_forecasts)
}

sites <- "FCRE"

start_dates <- as_date(start_dates)
forecast_start_dates <- start_dates + days(days_between_forecasts)
forecast_start_dates <- as_date(c(NA, forecast_start_dates[-1]))

configure_run_file <- "configure_run.yml"

for(j in 1:length(sites)){

  message(paste0("Running site: ", sites[j]))

  run_config <- yaml::read_yaml(file.path(lake_directory, "configuration", config_set_name, configure_run_file))
  run_config$configure_flare <- config_files[j]
  run_config$sim_name <- sim_names
  run_config$use_s3 <- use_s3
  yaml::write_yaml(run_config, file = file.path(lake_directory, "configuration", config_set_name, configure_run_file))

  if(start_from_scratch){
    if(use_s3){
      FLARErLER::delete_restart(sites[j], sim_names)
    }
    if(file.exists(file.path(lake_directory, "restart", sites[j], sim_names, configure_run_file))){
      unlink(file.path(lake_directory, "restart", sites[j], sim_names, configure_run_file))
    }
    config <- FLARErLER::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)

    unlink(config$file_path$execute_directory, recursive = TRUE)
    config <- FLARErLER::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)


    config$run_config$start_datetime <- as.character(paste0(start_dates[1], " 00:00:00"))
    config$run_config$forecast_start_datetime <- as.character(paste0(start_dates[2], " 00:00:00"))
    config$run_config$forecast_horizon <- 0
    config$run_config$restart_file <- NA
    run_config <- config$run_config
    yaml::write_yaml(run_config, file = file.path(config$file_path$configuration_directory, configure_run_file))
  } else {
    config <- FLARErLER::set_configuration(configure_run_file, lake_directory, config_set_name = config_set_name)
    config$file_path$forecast_output_directory <- file.path(lake_directory, "forecasts", config$location$site_id, config$run_config$sim_name)

    restart_files <- list.files(config$file_path$forecast_output_directory, "*.nc", full.names = FALSE)
    restart_files <- restart_files[nchar(restart_files) > 40]
    n_let <- nchar(paste0(config$run_config$sim_name, "_H_"))
    dates <- substr(restart_files, n_let+1, n_let+10)
    dates <- gsub("_", "-", dates)
    config$run_config <- yaml::read_yaml(file.path(config$file_path$restart_directory, paste0("configure_run_", dates[length(dates)], ".yml")))
    time_start_index <- grep(as.Date(config$run_config$forecast_start_datetime), forecast_start_dates)
  }

  config$da_setup$ensemble_size <- ensemble_size
  config$model_settings$model <- model
  config$model_settings$ncore <- ncore
  config$run_config$use_s3 <- FALSE


  depth_bins <- config$model_settings$modeled_depths

  # if(!use_archive){
  #   message("    Downloading NEON data")
  #   neonstore::neon_download(product = "DP1.20264.001", site = sites[j], start_date = NA)
  # }

  # neonstore_dir <-  file.path(lake_directory, "data_raw","neonstore")
  #
  # dir.create(neonstore_dir,showWarnings = FALSE)
  # Sys.setenv("NEONSTORE_DB" = neonstore_dir)
  # Sys.setenv("NEONSTORE_HOME" = neonstore_dir)

  ##'
  # Process the NEON data for the site selected in the original .yml file

  # message("    Processing NEON data")
  #
  # edi_file <- site_edi_profile[str_detect(site_edi_profile, sites[j])]
  #
  # FLARErLER::get_edi_file(edi_https = edi_url[str_detect(site_edi_profile, sites[j])],
  #                      file = edi_file,
  #                      lake_directory)
  #
  # profiler_data <- readr::read_csv(file.path(lake_directory,"data_raw",edi_file))
  #
  # cleaned_insitu_file <- buoy_qaqc(forecast_site = config$location$site_id,
  #                                  processed_filename = file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-insitu.csv")),
  #                                  depth_bins,
  #                                  profiler_data = profiler_data,
  #                                  release = NA)
  #
  # FLARErLER::put_targets(sites[j],
  #                     cleaned_insitu_file = cleaned_insitu_file, use_s3 = use_s3)

  ##` Download NOAA forecasts`

  message("    Downloading NOAA data")

  cycle <- "00"

  if(!use_archive){
    FLARErLER::get_stacked_noaa(lake_directory, config, averaged = TRUE)
  }

  for(i in time_start_index:length(forecast_start_dates)){

    # config <- FLARErLER::set_configuration(configure_run_file, lake_directory,
    #                                     config_set_name = config_set_name)

    # config <- FLARErLER::get_restart_file(config, lake_directory)
    if(!is.na(config$run_config$restart_file)) {
      config$run_config$restart_file <- basename(config$run_config$restart_file)
    }

    message(paste0("     Running forecast that starts on: ", config$run_config$start_datetime, " with index: ",i))

    if(config$run_config$forecast_horizon > 0){
      noaa_forecast_path <- FLARErLER::get_driver_forecast_path(config,
                                                             forecast_model = config$met$forecast_met_model)
      if(!use_archive){
        FLARErLER::get_driver_forecast(lake_directory, forecast_path = noaa_forecast_path)
      }
      forecast_dir <- file.path(config$file_path$noaa_directory, noaa_forecast_path)
    }else{
      forecast_dir <- NULL
    }

    # mCatch for missing files
    if(i > 1) {
      file_chk <- length(list.files(forecast_dir)) > 0
    } else {
      file_chk <- FALSE
    }

    # If no NOAA files - skips to next day
    if(!file_chk & i > 1) {
      message("No NOAA files for ", forecast_start_dates[i])
      config$run_config$forecast_start_datetime <- paste0(forecast_start_dates[i+1], " 00:00:00")
      next
    }

    noaa_forecast_path <- FLARErLER::get_driver_forecast_path(config,
                                                           forecast_model = config$met$forecast_met_model)


    dir.create(file.path(lake_directory, "flare_tempdir", config$location$site_id,
                         config$run_config$sim_name), recursive = TRUE, showWarnings = FALSE)

    met_out <- FLARErLER::generate_met_files(obs_met_file = file.path(config$file_path$qaqc_data_directory, paste0("observed-met_",config$location$site_id,".nc")),
                                          out_dir = config$file_path$execute_directory,
                                          forecast_dir = forecast_dir,
                                          config = config)
    met <- read.csv(met_out$filenames[1])
    tail(met$datetime)

    source(file.path(lake_directory, "workflows", config_set_name, "forecast_inflows.R"))

    if(config$run_config$forecast_horizon > 0) {
      inflow_forecast_path <- file.path(config$inflow$forecast_inflow_model, config$location$site_id,
                                        lubridate::as_date(config$run_config$forecast_start_datetime), paste0("0", lubridate::hour(config$run_config$forecast_start_datetime)))
    } else {
      inflow_forecast_path <- NULL
    }

    # inflow_forecast_path <- FLARErLER::get_driver_forecast_path(config,
    #                                                          forecast_model = config$inflow$forecast_inflow_model)

    if(!is.null(inflow_forecast_path)){
      # FLARErLER::get_driver_forecast(lake_directory, forecast_path = inflow_forecast_path)
      inflow_file_dir <- file.path(config$file_path$noaa_directory,inflow_forecast_path)
    }else{
      inflow_file_dir <- NULL
    }

    inflow_outflow_files <- FLARErLER::create_inflow_outflow_files(inflow_file_dir = inflow_file_dir,
                                                                inflow_obs = file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-inflow.csv")),
                                                                working_directory = config$file_path$execute_directory,
                                                                config = config,
                                                                state_names = states_config$state_names)

    inf <- read.csv(inflow_outflow_files$inflow_file_names[1])
    tail(inf)


    #Need to remove the 00 ensemble member because it only goes 16-days in the future
    met_out$filenames <- met_out$filenames[!stringr::str_detect(met_out$filenames, "ens00")]

    ##' Create observation matrix
    cleaned_observations_file_long <- file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv"))
    obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
    obs <- FLARErLER::create_obs_matrix(cleaned_observations_file_long,
                                     obs_config,
                                     config)

    states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())
    states_config <- FLARErLER::generate_states_to_obs_mapping(states_config, obs_config)

    model_sd <- FLARErLER::initiate_model_error(config = config, states_config = states_config)


    ##' Generate initial conditions
    pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())

    # Set fc output dire
    config$file_path$forecast_output_directory <- file.path(lake_directory, "forecasts", config$location$site_id, config$run_config$sim_name)


    init <- FLARErLER::generate_initial_conditions(states_config,
                                                obs_config,
                                                pars_config,
                                                obs,
                                                config,
                                                historical_met_error = met_out$historical_met_error)

    # states_init = init$states
    # pars_init = init$pars
    # aux_states_init = init$aux_states_init
    # obs = obs
    # obs_sd = obs_config$obs_sd
    # model_sd = model_sd
    # working_directory = config$file_path$execute_directory
    # met_file_names = met_out$filenames
    # inflow_file_names = inflow_outflow_files$inflow_file_name
    # outflow_file_names = inflow_outflow_files$outflow_file_name
    # config = config
    # pars_config = pars_config
    # states_config = states_config
    # obs_config = obs_config
    # management = NULL
    # da_method = config$da_setup$da_method
    # par_fit_method = config$da_setup$par_fit_method
    # debug = TRUE
    config$output_settings$diagnostics_names <- NULL

    ##' Run the forecasts
    message("Starting Data Assimilation and Forecasting for ", config$model_settings$model)
    da_forecast_output <- FLARErLER::run_da_forecast_all(states_init = init$states,
                                                      pars_init = init$pars,
                                                      aux_states_init = init$aux_states_init,
                                                      obs = obs,
                                                      obs_sd = obs_config$obs_sd,
                                                      model_sd = model_sd,
                                                      working_directory = config$file_path$execute_directory,
                                                      met_file_names = met_out$filenames,
                                                      inflow_file_names = inflow_outflow_files$inflow_file_name,
                                                      outflow_file_names = inflow_outflow_files$outflow_file_name,
                                                      config = config,
                                                      pars_config = pars_config,
                                                      states_config = states_config,
                                                      obs_config = obs_config,
                                                      management = NULL,
                                                      da_method = config$da_setup$da_method,
                                                      par_fit_method = config$da_setup$par_fit_method,
                                                      debug = TRUE)

    # Set fc output dire
    config$file_path$forecast_output_directory <- file.path(lake_directory, "forecasts", config$location$site_id, config$run_config$sim_name)
    dir.create(config$file_path$forecast_output_directory, recursive = TRUE, showWarnings = FALSE)

    saved_file <- FLARErLER::write_forecast_netcdf(da_forecast_output = da_forecast_output,
                                                forecast_output_directory = config$file_path$forecast_output_directory,
                                                use_short_filename = FALSE)

    #Create EML Metadata
    eml_file_name <- FLARErLER::create_flare_metadata(file_name = saved_file,
                                                   da_forecast_output = da_forecast_output)

    rm(da_forecast_output)
    gc()
    message("Generating plot")
    pdf_file <- FLARErLER::plotting_general_2(file_name = saved_file,
                                           target_file = file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-insitu.csv")),
                                           ncore = 2,
                                           obs_csv = FALSE)

    FLARErLER::put_forecast(saved_file, eml_file_name, config)

    if(config$run_config$use_s3){
      success <- aws.s3::put_object(file = pdf_file, object = file.path(config$location$site_id, basename(pdf_file)), bucket = "analysis")
      if(success){
        unlink(pdf_file)
      }
    }

    restart_date <- as.character(lubridate::as_datetime(config$run_config$forecast_start_datetime) + lubridate::days(1))
    config <- FLARErLER::update_run_config(config, lake_directory, configure_run_file, saved_file, new_horizon = forecast_horizon, day_advance = days_between_forecasts)
    file.copy(from = file.path(config$file_path$restart_directory, configure_run_file),
              to = file.path(config$file_path$restart_directory, paste0("configure_run_", restart_date, ".yml")), overwrite = TRUE)

    # unlink(config$run_config$restart_file)
    unlink(forecast_dir, recursive = TRUE)
    setwd(lake_directory)
    unlink(file.path(lake_directory, "flare_tempdir", config$location$site_id, run_config$sim_name), recursive = TRUE)
    if (config$run_config$use_s3) {
      success <- aws.s3::put_object(file = saved_file, object = file.path(config$location$site_id,
                                                                          basename(saved_file)), bucket = "forecasts")
      if (success) {
        unlink(saved_file)
      }
    }
  }
}

