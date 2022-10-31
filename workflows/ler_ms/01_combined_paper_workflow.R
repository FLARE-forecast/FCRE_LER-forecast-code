library(tidyverse)
library(lubridate)
set.seed(100)

readRenviron("~/.Renviron") # compatible with littler

Sys.setenv('AWS_DEFAULT_REGION' = 's3',
           'AWS_S3_ENDPOINT' = 'flare-forecast.org',
           'USE_HTTPS' = TRUE,
           'TZ' = 'UTC')

use_s3 <- TRUE

lake_directory <- here::here()

starting_index <- 1
#Pick up on 25

files.sources <- list.files(file.path(lake_directory, "R"), full.names = TRUE)
sapply(files.sources, source)

models <- c("GLM", "GOTM","Simstrat")
#models <- c("GOTM","Simstrat")
models <- c("GLM")
config_files <- "configure_flare.yml"
configure_run_file <- "configure_run.yml"
config_set_name <- "ler_ms"

num_forecasts <- 52*2 #* 3 - 3
#num_forecasts <- 1#19 * 7 + 1
days_between_forecasts <- 7
forecast_horizon <- 14
starting_date <- as_date("2020-09-25")
#second_date <- as_date("2020-12-01") - days(days_between_forecasts)
#starting_date <- as_date("2018-07-20")
second_date <- as_date("2020-11-01") - days(days_between_forecasts)

start_dates <- as_date(rep(NA, num_forecasts + 1))
end_dates <- as_date(rep(NA, num_forecasts + 1))
start_dates[1] <- starting_date
end_dates[1] <- second_date
for(i in 2:(num_forecasts+1)){
  start_dates[i] <- as_date(end_dates[i-1])
  end_dates[i] <- start_dates[i] + days(days_between_forecasts)
}

j = 1
sites <- "fcre"

#function(i, sites, lake_directory, sim_names, config_files, )

message(paste0("Running site: ", sites[j]))

##'
# Set up configurations for the data processing
config_obs <- FLAREr::initialize_obs_processing(lake_directory, observation_yml = "observation_processing.yml", config_set_name = config_set_name)

#' Clone or pull from data repositories

FLAREr::get_git_repo(lake_directory,
                     directory = config_obs$realtime_insitu_location,
                     git_repo = "https://github.com/FLARE-forecast/FCRE-data.git")

FLAREr::get_git_repo(lake_directory,
                     directory = config_obs$realtime_met_station_location,
                     git_repo = "https://github.com/FLARE-forecast/FCRE-data.git")

#FLAREr::get_git_repo(lake_directory,
#                     directory = config_obs$realtime_inflow_data_location,
#                     git_repo = "https://github.com/FLARE-forecast/FCRE-data.git")

#get_git_repo(lake_directory,
#             directory = config_obs$manual_data_location,
#             git_repo = "https://github.com/FLARE-forecast/FCRE-data.git")

#' Download files from EDI

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/389/6/a5524c686e2154ec0fd0459d46a7d1eb",
                     file = config_obs$met_raw_obs_fname[2],
                     lake_directory)

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/271/6/23a191c1870a5b18cbc17f2779f719cf",
                     file = config_obs$insitu_obs_fname[2],
                     lake_directory)

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/198/8/336d0a27c4ae396a75f4c07c01652985",
                     file = config_obs$secchi_fname,
                     lake_directory)


FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/202/7/f5fa5de4b49bae8373f6e7c1773b026e",
                     file = config_obs$inflow_raw_file1[2],
                     lake_directory)

#' Clean up observed meterology

cleaned_met_file <- met_qaqc(realtime_file = file.path(config_obs$file_path$data_directory, config_obs$met_raw_obs_fname[1]),
                             qaqc_file = file.path(config_obs$file_path$data_directory, config_obs$met_raw_obs_fname[2]),
                             cleaned_met_file = file.path(config_obs$file_path$targets_directory, config_obs$site_id,paste0("observed-met_",config_obs$site_id,".nc")),
                             input_file_tz = "EST",
                             nldas = NULL)

#' Clean up observed inflow

#cleaned_inflow_file <- inflow_qaqc(realtime_file = file.path(config_obs$file_path$data_directory, config_obs$inflow_raw_file1[1]),
#                                   qaqc_file = file.path(config_obs$file_path$data_directory, config_obs$inflow_raw_file1[2]),
#                                   nutrients_file = NA,
#                                   silica_file = NA,
#                                   co2_ch4 = NA,
#                                   cleaned_inflow_file = file.path(config_obs$file_path$targets_directory, config_obs$site_id, paste0(config_obs$site_id,"-targets-inflow.csv")),
#                                   input_file_tz = 'EST')

#' Clean up observed insitu measurements

cleaned_insitu_file <- in_situ_qaqc(insitu_obs_fname = file.path(config_obs$file_path$data_directory,config_obs$insitu_obs_fname),
                                    data_location = config_obs$file_path$data_directory,
                                    maintenance_file = file.path(config_obs$file_path$data_directory,config_obs$maintenance_file),
                                    ctd_fname = NA,
                                    nutrients_fname = NA,
                                    secchi_fname = file.path(config_obs$file_path$data_directory, config_obs$secchi_fname),
                                    ch4_fname = NA,
                                    cleaned_insitu_file = file.path(config_obs$file_path$targets_directory, config_obs$site_id, paste0(config_obs$site_id,"-targets-insitu.csv")),
                                    lake_name_code = config_obs$site_id,
                                    config = config_obs)

##` Download NOAA forecasts`
config <- FLAREr::set_configuration(configure_run_file = configure_run_file, lake_directory = lake_directory, config_set_name = config_set_name, sim_name = models[1])

FLAREr::put_targets(site_id = config_obs$site_id,
                    cleaned_insitu_file,
                    cleaned_met_file,
                    cleaned_inflow_file = NA,
                    use_s3 = config$run_config$use_s3,
                    config)

missing_noaa <- rep(0, length(end_dates))
for(i in 1:length(end_dates)){
  noaa_forecast_path <- file.path(config$met$forecast_met_model, config$location$site_id, end_dates[i], "00")
  if(length(list.files(file.path(lake_directory,"drivers", noaa_forecast_path))) == 0){
    test <- FLAREr::get_driver_forecast(lake_directory, forecast_path = noaa_forecast_path, config)
    if(length(list.files(file.path(lake_directory,"drivers", noaa_forecast_path))) == 0){
      missing_noaa[i] <- 1
    }
  }
}

horizon <- ifelse(missing_noaa == 0, forecast_horizon, 0)

available_dates <- list.files(file.path(lake_directory,"drivers","noaa","NOAAGEFS_1hr","fcre"))

sims <- expand.grid(paste0(start_dates,"_",end_dates,"_", horizon), models)

names(sims) <- c("date","model")

sims$start_dates <- stringr::str_split_fixed(sims$date, "_", 3)[,1]
sims$end_dates <- stringr::str_split_fixed(sims$date, "_", 3)[,2]
sims$horizon <- stringr::str_split_fixed(sims$date, "_", 3)[,3]


sims <- sims |>
  mutate(model = as.character(model)) |>
  select(-date) |>
  distinct_all() |>
  arrange(start_dates)

sims$horizon[19:21] <- 0


for(i in starting_index:nrow(sims)){


  message(paste0("index: ", i))
  message(paste0("     Running model: ", sims$model[i]))

  model <- sims$model[i]
  sim_names <- paste0(model,"_2")

  config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name, sim_name = sim_names)

  cycle <- "00"

    if(file.exists(file.path(lake_directory, "restart", sites[j], sim_names, configure_run_file))){
      unlink(file.path(lake_directory, "restart", sites[j], sim_names, configure_run_file))
      if(use_s3){
        FLAREr::delete_restart(site_id = sites[j],
                               sim_name = sim_names,
                               bucket = config$s3$warm_start$bucket,
                               endpoint = config$s3$warm_start$endpoint)
      }
    }
    run_config <- yaml::read_yaml(file.path(lake_directory, "configuration", config_set_name, configure_run_file))
    run_config$configure_flare <- config_files[j]
    run_config$sim_name <- sim_names
    yaml::write_yaml(run_config, file = file.path(lake_directory, "configuration", config_set_name, configure_run_file))
    config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)
    config$run_config$start_datetime <- as.character(paste0(sims$start_dates[i], " 00:00:00"))
    config$run_config$forecast_start_datetime <- as.character(paste0(sims$end_dates[i], " 00:00:00"))
    config$run_config$forecast_horizon <- sims$horizon[i]
    if(i < 4){
      config$run_config$restart_file <- NA
    }else{
      config$run_config$restart_file <- paste0(config$location$site_id, "-", lubridate::as_date(config$run_config$start_datetime), "-", sim_names, ".nc")
      if(!file.exists(config$run_config$restart_file )){
        warning(paste0("restart file: ", config$run_config$restart_file, " doesn't exist"))
      }
    }
    run_config <- config$run_config
    yaml::write_yaml(run_config, file = file.path(config$file_path$configuration_directory, configure_run_file))

  config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name, sim_name = sim_names)
  config$model_settings$model <- model
  config$run_config$sim_name <- sim_names
  config <- FLAREr::get_restart_file(config, lake_directory)


  message(paste0("     Running forecast that starts on: ", config$run_config$start_datetime))

  if(config$run_config$forecast_horizon > 0){
    noaa_forecast_path <- FLAREr::get_driver_forecast_path(config,
                                                           forecast_model = config$met$forecast_met_model)
    forecast_dir <- file.path(config$file_path$noaa_directory, noaa_forecast_path)
  }else{
    forecast_dir <- NULL
  }

  #inflow_forecast_path <- FLAREr::get_driver_forecast_path(config,
  #                                                         forecast_model = config$inflow$forecast_inflow_model)
  #

  inflow_forecast_path <- NULL
  if(!is.null(inflow_forecast_path)){
    FLAREr::get_driver_forecast(lake_directory, forecast_path = inflow_forecast_path, config)
    inflow_file_dir <- file.path(config$file_path$noaa_directory,inflow_forecast_path)
  }else{
    inflow_file_dir <- NULL
  }


  config$future_inflow_flow_coeff <- c(0.0010803, 0.9478724, 0.3478991)
  config$future_inflow_flow_error <- 0.00965
  config$future_inflow_temp_coeff <- c(0.20291, 0.94214, 0.04278)
  config$future_inflow_temp_error <- 0.943

  #if(!is.null(forecast_dir) > 0){
  #  forecast_files <- list.files(file.path(lake_directory, "drivers", noaa_forecast_path), full.names = TRUE)
  #  temp_flow_forecast <- forecast_inflows_outflows(inflow_obs = file.path(config$file_path$qaqc_data_directory, "fcre-targets-inflow.csv"),
  #                                                  forecast_files = forecast_files,
  #                                                  obs_met_file = file.path(config$file_path$qaqc_data_directory,"observed-met_fcre.nc"),
  #                                                  output_dir = config$file_path$inflow_directory,
  #                                                  inflow_model = config$inflow$forecast_inflow_model,
  #                                                  inflow_process_uncertainty = FALSE,
  #                                                  forecast_location = config$file_path$forecast_output_directory,
  #                                                  config = config,
  #                                                  use_s3 = config$run_config$use_s3,
  #                                                  bucket = "drivers",
  #                                                  model_name = "glm")
  #}

  #Need to remove the 00 ensemble member because it only goes 16-days in the future

  #pars_config <- NULL #readr::read_csv(file.path(config$file_path$configuration_directory, "FLAREr", config$model_settings$par_config_file), col_types = readr::cols())
  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

  FLAREr::get_stacked_noaa(lake_directory, config, averaged = TRUE)

  met_out <- FLARErLER::generate_met_files_ler(obs_met_file = file.path(config$file_path$noaa_directory, "noaa", "NOAAGEFS_1hr_stacked_average", config$location$site_id, paste0("observed-met-noaa_",config$location$site_id,".nc")),
                                               out_dir = config$file_path$execute_directory,
                                               forecast_dir = forecast_dir,
                                               config = config)

  #Download and process observations (already done)

  #met_out <- FLARErLER::generate_met_files_ler(obs_met_file = file.path(config$file_path$qaqc_data_directory, paste0("observed-met_",config$location$site_id,".nc")),
  #                                         out_dir = config$file_path$execute_directory,
  #                                         forecast_dir = forecast_dir,
  #                                         config = config)

  met_out$filenames <- met_out$filenames[!stringr::str_detect(met_out$filenames, "ens00")]

  #inflow_outflow_files <- FLARErLER::create_inflow_outflow_files_ler(inflow_file_dir = inflow_file_dir,
  #                                                                   inflow_obs = file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-inflow.csv")),
  #                                                                   working_directory = config$file_path$execute_directory,
  #                                                                   config = config,
  #                                                                   state_names = states_config$state_names)

  #Create observation matrix
  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long = file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),
                                   obs_config = obs_config,
                                   config)

  #obs[ ,2:dim(obs)[2], ] <- NA

  states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)

  model_sd <- FLAREr::initiate_model_error(config, states_config)

  init <- FLARErLER::generate_initial_conditions_ler(states_config,
                                                     obs_config,
                                                     pars_config,
                                                     obs,
                                                     config,
                                                     historical_met_error = met_out$historical_met_error)
  if(model != "GLM"){ #GOTM and Simstrat have different diagnostics
    config$output_settings$diagnostics_names <- NULL
  }
  # if(model == "Simstrat"){  #Inflows doesn't work for Simstrat but inflows are not turned off for GLM with setting NULL
  inflow_file_names <- NULL
  outflow_file_names <- NULL
  # }else{
  #   inflow_file_names <- inflow_outflow_files$inflow_file_name
  #   outflow_file_names <- inflow_outflow_files$outflow_file_name
  # }
  #Run EnKF
  da_forecast_output <- FLARErLER::run_da_forecast_ler(states_init = init$states,
                                                       pars_init = init$pars,
                                                       aux_states_init = init$aux_states_init,
                                                       obs = obs,
                                                       obs_sd = obs_config$obs_sd,
                                                       model_sd = model_sd,
                                                       working_directory = config$file_path$execute_directory,
                                                       met_file_names = met_out$filenames,
                                                       inflow_file_names = inflow_file_names,
                                                       outflow_file_names = outflow_file_names,
                                                       config = config,
                                                       pars_config = pars_config,
                                                       states_config = states_config,
                                                       obs_config = obs_config,
                                                       management = NULL,
                                                       da_method = config$da_setup$da_method,
                                                       par_fit_method = config$da_setup$par_fit_method,
                                                       debug = TRUE)

  # Save forecast

  #saved_file <- FLAREr::write_forecast_netcdf(da_forecast_output = da_forecast_output,
  saved_file <- FLARErLER::write_forecast_netcdf_ler(da_forecast_output = da_forecast_output,
                                                     forecast_output_directory = config$file_path$forecast_output_directory,
                                                     use_short_filename = TRUE)

  forecast_file <- FLARErLER::write_forecast_csv_ler(da_forecast_output = da_forecast_output,
                                                     forecast_output_directory = config$file_path$forecast_output_directory,
                                                     use_short_filename = TRUE)

  message("writing score file")
  if(config$run_config$forecast_horizon > 0){
    dir.create(file.path(lake_directory, "scores", config$location$site_id, config$run_config$sim_name), recursive = TRUE, showWarnings = FALSE)
    score_file <- FLAREr::generate_forecast_score(targets_file = file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),
                                                  forecast_file = forecast_file,
                                                  output_directory = file.path(lake_directory, "scores", config$location$site_id, config$run_config$sim_name))
    FLAREr::put_score(saved_file = score_file, config)
  }

  FLAREr::put_forecast_csv(saved_file = forecast_file, config)
  #Create EML Metadata
  #eml_file_name <- FLAREr::create_flare_metadata(file_name = saved_file,
  #                                               da_forecast_output = da_forecast_output)


  message("Generating plot")
  FLAREr::plotting_general_2(file_name = saved_file,
                             target_file = file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-insitu.csv")),
                             ncore = 2,
                             obs_csv = FALSE)

  FLAREr::put_forecast(saved_file, eml_file_name = NULL, config)

  unlink(saved_file)

  unlink(config$run_config$restart_file)

  rm(da_forecast_output)
  gc()

}
