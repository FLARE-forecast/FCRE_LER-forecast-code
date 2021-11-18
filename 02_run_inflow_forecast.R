renv::restore()

library(tidyverse)
library(lubridate)

lake_directory <- here::here()

files.sources <- list.files(file.path(lake_directory, "R"), full.names = TRUE)
sapply(files.sources, source)

configure_run_file <- "configure_run.yml"
config <- FLAREr::set_configuration(configure_run_file,lake_directory)

FLAREr::get_targets(lake_directory, config)

noaa_forecast_path <- FLAREr::get_driver_forecast_path(config,
                                             forecast_model = config$met$forecast_met_model)


if(!is.null(noaa_forecast_path)){

  FLAREr::get_driver_forecast(lake_directory, forecast_path = noaa_forecast_path)

  message("Forecasting inflow and outflows")
  # Forecast Inflows

  config$future_inflow_flow_coeff <- c(0.0010803, 0.9478724, 0.3478991)
  config$future_inflow_flow_error <- 0.00965
  config$future_inflow_temp_coeff <- c(0.20291, 0.94214, 0.04278)
  config$future_inflow_temp_error <- 0.943

  forecast_files <- list.files(file.path(lake_directory, "drivers", noaa_forecast_path), full.names = TRUE)
  if(length(forecast_files) == 0){
    stop(paste0("missing forecast files at: ", noaa_forecast_path))
  }
  temp_flow_forecast <- forecast_inflows_outflows(inflow_obs = file.path(config$file_path$qaqc_data_directory, "fcre-targets-inflow.csv"),
                                                  forecast_files = forecast_files,
                                                  obs_met_file = file.path(config$file_path$qaqc_data_directory,"observed-met_fcre.nc"),
                                                  output_dir = config$file_path$inflow_directory,
                                                  inflow_model = config$inflow$forecast_inflow_model,
                                                  inflow_process_uncertainty = FALSE,
                                                  forecast_location = config$file_path$forecast_output_directory,
                                                  config = config,
                                                  use_s3 = config$run_config$use_s3,
                                                  bucket = "drivers")

  #NEED TO COPY TO BUCKET

  if(config$model_settings$model_name == "glm_aed"){

    run_dir <- str_replace_all(dirname(list.files(temp_flow_forecast[[1]], full.names = TRUE)[1]), "INFLOW-FLOWS-NOAAGEFS-AR1","INFLOW-FLOWS-NOAAGEFS-AR1-AED")

    if(!dir.exists(run_dir)){
      dir.create(run_dir, recursive = TRUE)
    }

    historical_chemistry_weir <- read_csv("/Users/quinn/Downloads/FCRE-forecast-code/targets/FCR_weir_inflow_2013_2019_20200624_allfractions_2poolsDOC.csv")

    for(i in 1:length(inflow_files)){
      inflow_files <- list.files(temp_flow_forecast[[1]], full.names = TRUE)
      inflow_forecast <- read_csv(inflow_files[i])

      historical_chemistry_weir_mean <- historical_chemistry_weir %>%
        mutate(doy = lubridate::yday(time)) %>%
        select(-c("FLOW", "TEMP", "SALT", "time")) %>%
        group_by(doy) %>%
        summarise(across(everything(), mean))

      file_name <- str_replace_all(list.files(temp_flow_forecast[[1]], full.names = TRUE)[i], "INFLOW-FLOWS-NOAAGEFS-AR1","INFLOW-FLOWS-NOAAGEFS-AR1-AED")

      inflow_forecast_aed <- inflow_forecast %>%
        mutate(doy = lubridate::yday(time)) %>%
        left_join(historical_chemistry_weir_mean, by = "doy") %>%
        select(-doy) %>%
        write_csv(file = file_name)

      #NEED TO COPY TO BUCKET
    }
  }else if(config$model_settings$model_name == "glm_oxy"){

    #NEED TO GET WORKING WITH S3

    run_dir <- str_replace_all(dirname(list.files(temp_flow_forecast, full.names = TRUE)[1]), "INFLOW-FLOWS-NOAAGEFS-AR1","INFLOW-FLOWS-NOAAGEFS-AR1-AED")

    if(!dir.exists(run_dir)){
      dir.create(run_dir, recursive = TRUE)
    }

    historical_chemistry_weir <- read_csv(file.path(config$file_path$qaqc_data_directory, "FCR_weir_inflow_2013_2019_20200624_allfractions_2poolsDOC.csv"))

    for(i in 1:length(inflow_files)){
      inflow_files <- list.files(temp_flow_forecast, full.names = TRUE)
      inflow_forecast <- read_csv(inflow_files[i])

      historical_chemistry_weir_mean <- historical_chemistry_weir %>%
        mutate(doy = lubridate::yday(time)) %>%
        select(-c("FLOW", "TEMP", "SALT", "time")) %>%
        select("OXY_oxy") %>%
        group_by(doy) %>%
        summarise(across(everything(), mean))

      file_name <- str_replace_all(list.files(temp_flow_forecast, full.names = TRUE)[i], "INFLOW-FLOWS-NOAAGEFS-AR1","INFLOW-FLOWS-NOAAGEFS-AR1-AEDOXY")

      inflow_forecast_aed <- inflow_forecast %>%
        mutate(doy = lubridate::yday(time)) %>%
        left_join(historical_chemistry_weir_mean, by = "doy") %>%
        select(-doy) %>%
        write_csv(file = file_name)

      #NEED TO COPY TO BUCKET
    }

    unlink(noaa_forecast_path, recursive = TRUE)
    if(use_s3){
      unlink(dirname((temp_flow_forecast[[1]])[1]), recursive = TRUE)
    }
  }

  message(paste0("successfully generated inflow forecats for: ", file.path(config$met$forecast_met_model,config$location$site_id,lubridate::as_date(config$run_config$forecast_start_datetime))))

}else{

  message(paste0("no forecast days in configuration file: ", file.path(config$met$forecast_met_model,config$location$site_id,lubridate::as_date(config$run_config$forecast_start_datetime))))

}


