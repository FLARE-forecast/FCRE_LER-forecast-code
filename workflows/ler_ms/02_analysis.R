#' FLARE-LER-MS Analysis

library(magrittr)
library(ggplot2)

lake_directory <- here::here()


models <- c("GLM", "GOTM", "Simstrat")
config_set_name <- "ler_ms"
sim_names <- paste0("ms1_ler_flare_", models)
configure_run_file <- "configure_run.yml"

source(file.path(lake_directory, "R", "read_flare_params.R"))
source(file.path(lake_directory, "R", "read_flare_temp.R"))
source(file.path(lake_directory, "R", "combine_forecast_observation.R"))

# Set Color schemes
cols <- as.vector(palette.colors(palette = "Okabe-Ito"))
col_scale <- scale_color_manual(values = c("GLM-LER" = cols[5], "GOTM-LER" = cols[3], "Simstrat-LER" = cols[4],
                                           "LER mean" = cols[1]))
fill_scale <- scale_fill_manual(values = c("GLM-LER" = cols[5], "GOTM-LER" = cols[3], "Simstrat-LER" = cols[4],
                                           "LER mean" = cols[1]))
mod_leg <- guides(color = guide_legend(override.aes = list(shape = NA, size = 4))) # Plot Legend


config <- FLAREr::set_configuration(configure_run_file, lake_directory,
                                    config_set_name = config_set_name)


# Extract parameter sets ----
par_list <- lapply(1:length(models), function(m) {

  fc_dir <- file.path(config$file_path$forecast_output_directory,
                      paste0("ms1_ler_flare_", models[m]))
  fc_files <- list.files(fc_dir, pattern = "*.nc", full.names = TRUE)
  fc_files_short <- basename(fc_files)
  fc_files <- fc_files[nchar(fc_files_short) > 42]

  pars <- read_flare_params(files = fc_files, type = "forecast", summary = TRUE)
  pars$model <- models[m]
  return(pars)
})

params <- do.call(rbind, par_list)

write.csv(params, file.path(config$file_path$analysis_directory,
                          paste0(config_set_name, "_forecast_parameter_summary.csv")),
          quote = FALSE, row.names = FALSE)


params$mod_name <- paste0(params$model, "-LER")

idx <- which(params$datetime <= "2021-11-15")
p1 <- ggplot(params[idx, ]) +
  geom_ribbon(aes(datetime, ymin = mean - sd, ymax = mean + sd, fill = mod_name), alpha = 0.3) +
  geom_line(aes(datetime, mean, color = mod_name)) +
  ylab("Value") +
  facet_wrap(model~parameter, scales = "free_y", nrow = 3) +
  xlab("Forecast Date") +
  col_scale +
  fill_scale +
  mod_leg +
  labs(fill = "Sim Name", color = "Sim Name") +
  theme_classic(base_size = 16)
p1
ggsave(file.path(config$file_path$analysis_directory,
                 paste0(config_set_name, "_forecast_parameter_summary.png")), p1, device = "png", width = 900, height = 900, units = "mm")


target_file <- file.path(config$file_path$qaqc_data_directory,
                         "fcre-targets-insitu.csv")

obs <- read.csv(target_file)
wtemp <- obs[obs$variable == "temperature", ]

sub <- wtemp[wtemp$date > "2021-04-01" & wtemp$date < "2021-12-31", ]
sub$Date <- as.Date(sub$date)
ggplot(sub) +
  geom_line(aes(Date, value, color = factor(depth)))


sub$dens <- rLakeAnalyzer::water.density(sub$value)
plyr::ddply(sub, "date", function(x) {
  dif <- abs(x$dens[x$depth == max(x$depth)] - x$dens[x$depth == min(x$depth)])
  if(dif > 0.1) {
    return("Stratified")
  } else {
    return("Isothermal")
  }
})


depths <- unique(sub$depth)


# Analyze each individual model
for(m in seq_len(length(models))) {

  fc_dir <- file.path(config$file_path$forecast_output_directory,
                      paste0("ms1_ler_flare_", models[m]))
  fc_files <- list.files(fc_dir, pattern = "*.nc", full.names = TRUE)
  fc_files_short <- basename(fc_files)
  fc_files <- fc_files[nchar(fc_files_short) > 42]

  for(f in fc_files) {

    tmp <- read_flare_temp(f)


    if(tmp[[1]] == "No forecast") {
      message("No forecast in ", basename(f))
      next
    }
    if(length(dim(tmp[[1]]$temp)) < 3) {
      message("Not enough dimensions in ", basename(f))
      next
    }

    res <- combine_forecast_observation(temp = tmp, obs = sub, min_dep = 1, max_dep = 8)

    dir.create(file.path(config$file_path$analysis_directory, models[m]), showWarnings = FALSE)
    write.csv(res, file.path(config$file_path$analysis_directory, models[m], paste0("forecast_summary_", res$forecast_date[1], ".csv")),
              row.names = FALSE, quote = FALSE)
    message(Sys.time())
  }
}


# Analyze the Ensemble Mean ----
fc_dir <- file.path(config$file_path$forecast_output_directory,
                    paste0("ms1_ler_flare_", models[1]))
fc_files <- list.files(fc_dir, pattern = "*.nc", full.names = TRUE)
fc_files_short <- basename(fc_files)
fc_files <- fc_files[nchar(fc_files_short) > 42]

for(f in fc_files) {

  nam <- strsplit(basename(f), "_H_")[[1]][2]
  date_tgt <- substr(nam, 1, 21)

  fils <- list.files(file.path(config$file_path$forecast_output_directory,
                               paste0("ms1_ler_flare_", models[2])), pattern = date_tgt)
  f2 <- file.path(config$file_path$forecast_output_directory,
                  paste0("ms1_ler_flare_", models[2]), fils[grep("*.nc", fils)])
  fils <- list.files(file.path(config$file_path$forecast_output_directory,
                               paste0("ms1_ler_flare_", models[3])), pattern = date_tgt)
  f3 <- file.path(config$file_path$forecast_output_directory,
                  paste0("ms1_ler_flare_", models[3]), fils[grep("*.nc", fils)[1]])


  # Read each netCDF for each model
  tmp <- read_flare_temp(f)
  tmp2 <- read_flare_temp(f2)
  tmp3 <- read_flare_temp(f3)

  tmp_list <- list(tmp[[1]]$temp[-35, , ], tmp2[[1]]$temp, tmp3[[1]]$temp)


  # Check dimensions match
  if(all(dim(tmp_list[[1]]) == dim(tmp_list[[2]]) &
         all(dim(tmp_list[[1]]) == dim(tmp_list[[3]])) &
         all(dim(tmp_list[[2]]) == dim(tmp_list[[3]])))) {

    # Average across models for ensemble mean
    tmp[[1]]$temp <- Reduce("+", tmp_list) / length(tmp_list)


    if(tmp[[1]] == "No forecast") {
      message("No forecast in ", basename(f))
      next
    }
    if(length(dim(tmp[[1]]$temp)) < 3) {
      message("Not enough dimensions in ", basename(f))
      next
    }

    res <- combine_forecast_observation(temp = tmp, obs = sub, min_dep = 1, max_dep = 8)

    dir.create(file.path(config$file_path$analysis_directory, "ens_mean"), showWarnings = FALSE)
    write.csv(res, file.path(config$file_path$analysis_directory, "ens_mean", paste0("forecast_summary_", res$forecast_date[1], ".csv")),
              row.names = FALSE, quote = FALSE)
    message("[", Sys.time(), "] Finished ", basename(f))

  } else {
    message("Dimensions for ", date_tgt, " are not equal")
  }
}

# end
