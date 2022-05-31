library(magrittr)
library(ggplot2)

# Set colors & legends
cols <- as.vector(palette.colors(palette = "Okabe-Ito"))
col_scale <- scale_color_manual(values = c("GLM-LER" = cols[5], "GOTM-LER" = cols[3], "Simstrat-LER" = cols[4],
                                           # "GLM-Thomas" = cols[2],
                                           "LER mean" = cols[1]))
fill_scale <- scale_fill_manual(values = c("GLM-LER" = cols[5], "GOTM-LER" = cols[3], "Simstrat-LER" = cols[4],
                                           # "GLM-Thomas" = cols[2],
                                           "LER mean" = cols[1]))
size_scale <- scale_size_manual(values = c("GLM-LER" = 1, "GOTM-LER" = 1, "Simstrat-LER" = 1,
                                           # "GLM-Thomas" = 1.5,
                                           "LER mean" = 1.5))
mod_leg <- guides(color = guide_legend(override.aes = list(shape = NA, size = 4)))
ler_models <- c("GLM-LER", "GOTM-LER", "Simstrat-LER")

lake_directory <- here::here()


models <- c("GLM", "GOTM", "Simstrat", "ens_mean") #, "flare_realtime")
config_set_name <- "ler_ms"
sim_names <- paste0("ms1_ler_flare_", models)
configure_run_file <- "configure_run.yml"

config <- FLAREr::set_configuration(configure_run_file, lake_directory,
                                    config_set_name = config_set_name)
bathy <- read.csv(file.path(config$file_path$configuration_directory, config$model_settings$ler_bathymetry_file))

plot_dir <- file.path(config$file_path$analysis_directory, "plots")
dir.create(plot_dir, showWarnings = FALSE)

# Read in FLARE observations ----
target_file <- file.path(config$file_path$qaqc_data_directory,
                         "fcre-targets-insitu.csv")
obs <- read.csv(target_file)
wtemp <- obs[obs$variable == "temperature", ] # Subset to temperature

sub <- wtemp[wtemp$date > "2021-04-01" & wtemp$date < "2021-11-15", ] # Subset to target dates - need to change for your experiment
sub$Date <- as.Date(sub$date) # Line

meta <- plyr::ddply(sub, "Date", function(x) {
  res <- rLakeAnalyzer::meta.depths(x$value, x$depth, slope = 0.5, seasonal = FALSE)
  if(nrow(x) <= 1) {
    res <- c(NA, NA)
  }
  data.frame(epi = res[1], td = rLakeAnalyzer::thermo.depth(x$value, x$depth, seasonal = FALSE),  hyp = res[2])
})
p <- ggplot(meta) +
  geom_line(aes(Date, epi)) +
  geom_line(aes(Date, td), color = "red") +
  geom_line(aes(Date, hyp), linetype = "dashed") +
  scale_y_reverse()
p
plotly::ggplotly(p)

meta$epi_five <- c(rep(NA, 4), zoo::rollmeanr(meta$epi, 5))
meta$hyp_five <- c(rep(NA, 4), zoo::rollmeanr(meta$hyp, 5))
meta$td_five <- c(rep(NA, 4), zoo::rollmeanr(meta$td, 5))

p0 <- ggplot(sub) +
  geom_contour_filled(aes(Date, depth, z = value), binwidth = 2) +
  # geom_line(data = meta, aes(Date, epi_five), color = "black", size = 1.2) +
  # geom_line(data = meta, aes(Date, td_five), color = "red", size = 1.2) +
  # geom_line(data = meta, aes(Date, hyp_five), color = "black", size = 1.2, linetype = "dashed") +
  scale_y_continuous(breaks = seq(10, 0, -2), trans = "reverse") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_fill_gradientn(colours = c("#0A2864", "#CCD9FF", "#FFF9CF", "#FEBF00", "#E6281E", "#6C0000"),
                       super = metR::ScaleDiscretised) +
  labs(fill = "Temperature\n(\u00B0C)") +
  coord_cartesian(xlim = c(as.Date("2021-04-01"), as.Date("2021-11-15"))) +
  ylab("Depth (m)") +
  theme_classic(base_size = 18) +
  theme(legend.key.height = unit(1.5, "cm"))
p0
ggsave(file.path(plot_dir, paste0("obs_wtemp_heatmap.png")), p0, device = "png", units = "mm", width = 240, height = 120)

p1 <- ggplot(sub) +
  geom_contour_filled(aes(Date, depth, z = value), binwidth = 2) +
  # geom_line(data = meta, aes(Date, epi_five), color = "black", size = 1.2) +
  geom_line(data = meta, aes(Date, td_five), color = "black", size = 1.2) +
  # geom_line(data = meta, aes(Date, hyp_five), color = "black", size = 1.2, linetype = "dashed") +
  scale_y_continuous(breaks = seq(10, 0, -2), trans = "reverse") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_fill_gradientn(colours = c("#0A2864", "#CCD9FF", "#FFF9CF", "#FEBF00", "#E6281E", "#6C0000"),
                       super = metR::ScaleDiscretised) +
  labs(fill = "Temperature\n(\u00B0C)") +
  coord_cartesian(xlim = c(as.Date("2021-04-01"), as.Date("2021-11-15"))) +
  ylab("Depth (m)") +
  theme_classic(base_size = 18) +
  theme(legend.key.height = unit(1.5, "cm"))
p1
ggsave(file.path(plot_dir, paste0("obs_wtemp_heatmap_td.png")), p1, device = "png", units = "mm", width = 240, height = 120)


tgt_date <- as.Date("2021-06-01")

idx <- which(sub$Date == tgt_date)

p2 <- ggplot(sub[idx, ]) +
  geom_hline(yintercept =  meta$td[meta$Date == tgt_date],
             linetype = "dashed") +
  geom_path(aes(value, depth), size = 1.2) +
  scale_y_continuous(breaks = seq(0, 10, 2), trans = "reverse") +
  scale_x_continuous(breaks = seq(0, 32, 4), limits = c(4, 30)) +
  ylab("Depth (m)") +
  xlab("Temperature (\u00B0C)") +
  theme_classic(base_size = 12)
p2

p0a <- p0 +
  # geom_vline(xintercept = tgt_date, size = 1.2) +
  theme_classic(base_size = 12)  +
  theme(legend.key.height = unit(1.5, "cm"))
g <- ggpubr::ggarrange(p0a, p2, ncol = 2, widths = c(2, 1), align = "h")
g
ggsave(file.path(plot_dir, paste0("thermo_prof0.png")), g, device = "png", units = "mm", width = 240, height = 120)

p0a <- p0 +
  geom_vline(xintercept = tgt_date, size = 1.2) +
  theme_classic(base_size = 12)  +
  theme(legend.key.height = unit(1.5, "cm"))
g <- ggpubr::ggarrange(p0a, p2, ncol = 2, widths = c(2, 1), align = "h")
g
ggsave(file.path(plot_dir, paste0("thermo_prof1.png")), g, device = "png", units = "mm", width = 240, height = 120)

tgt_date <- as.Date("2021-09-15")

idx <- which(sub$Date == tgt_date)

p2 <- ggplot(sub[idx, ]) +
  geom_hline(yintercept =  meta$td[meta$Date == tgt_date],
             linetype = "dashed") +
  geom_path(aes(value, depth), size = 1.2) +
  scale_y_continuous(breaks = seq(0, 10, 2), trans = "reverse") +
  scale_x_continuous(breaks = seq(0, 32, 4), limits = c(4, 30)) +
  ylab("Depth (m)") +
  xlab("Temperature (\u00B0C)") +
  theme_classic(base_size = 12)
p2

p0a <- p0 +
  geom_vline(xintercept = tgt_date, size = 1.2) +
  theme_classic(base_size = 12)  +
  theme(legend.key.height = unit(1.5, "cm"))
g <- ggpubr::ggarrange(p0a, p2, ncol = 2, widths = c(2, 1), align = "h")
g
ggsave(file.path(plot_dir, paste0("thermo_prof2.png")), g, device = "png", units = "mm", width = 240, height = 120)


p2 <- ggplot(sub) +
  geom_contour_filled(aes(Date, depth, z = value), binwidth = 2) +
  geom_line(data = meta, aes(Date, epi_five), color = "black", size = 1.2, linetype = "dotted") +
  geom_line(data = meta, aes(Date, td_five), color = "black", size = 1.2) +
  geom_line(data = meta, aes(Date, hyp_five), color = "black", size = 1.2, linetype = "dashed") +
  scale_y_continuous(breaks = seq(10, 0, -2), trans = "reverse") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_fill_gradientn(colours = c("#0A2864", "#CCD9FF", "#FFF9CF", "#FEBF00", "#E6281E", "#6C0000"),
                       super = metR::ScaleDiscretised) +
  labs(fill = "Temperature\n(\u00B0C)") +
  coord_cartesian(xlim = c(as.Date("2021-04-01"), as.Date("2021-11-15"))) +
  ylab("Depth (m)") +
  theme_classic(base_size = 18) +
  theme(legend.key.height = unit(1.5, "cm"))
p2
ggsave(file.path(plot_dir, paste0("obs_wtemp_heatmap_td_epi_hyp.png")), p2, device = "png", units = "mm", width = 240, height = 120)


sub$dens <- rLakeAnalyzer::water.density(sub$value)
phen <- plyr::ddply(sub, "Date", function(x) {
  surf <- x$dens[x$depth == min(x$depth)]
  bott <- x$dens[x$depth == max(x$depth)]
  if(nrow(x) == 1) return(data.frame(status = NA))
  status <- ifelse((abs(surf - bott) < 0.1), "Mixed", "Stratified")
  data.frame(status = status, surf = surf, bott = bott)
})
phen$type <- "Density"
phen2 <- plyr::ddply(sub, "Date", function(x) {
  surf <- x$value[x$depth == 1]
  bott <- x$value[x$depth == 8]
  if(nrow(x) == 1) return(data.frame(status = NA))
  status <- ifelse((abs(surf - bott) < 1), "Mixed", "Stratified")
  data.frame(status = status, surf = surf, bott = bott)
})
phen2$type <- "Temperature"

ggplot() +
  geom_raster(data = phen, aes(Date, type, fill = status)) +
  geom_raster(data = phen2, aes(Date, type, fill = status))


# Read in all the forecast_summary csv files
out <- lapply(models, function(m) {
  fils <- list.files(file.path(config$file_path$analysis_directory, m),
                     pattern = "forecast_summary_", full.names = TRUE)
  readr::read_csv(fils)
})

names(out) <- models

# melt the list into a long dataframe
mlt <- reshape2::melt(out, id.vars = colnames(out$GLM))
mlt$date <- mlt$forecast_date + mlt$horizon
mlt <- mlt[mlt$horizon <= 34, ]
colnames(mlt)[11] <- "model"
mlt$model[mlt$model == "ens_mean"] <- "Ensemble mean"
# mlt$model[mlt$model == "flare_realtime"] <- "GLM-Thomas"
mlt$model <- factor(mlt$model, levels = c("GLM", "GOTM", "Simstrat",
                                          # "GLM-Thomas",
                                          "Ensemble mean"),
                    labels = c("GLM-LER", "GOTM-LER", "Simstrat-LER",
                               # "GLM-Thomas",
                               "LER mean"))
mlt$lsiz <- 1
mlt$lsiz[mlt$model == "Ensemble mean"] <- 1.2


# FC ----
fc_dir <- file.path(config$file_path$forecast_output_directory,
                    "ms1_ler_flare_Simstrat")
                    # "flare_realtime")
fc_files <- list.files(fc_dir, pattern = "*.nc", full.names = TRUE)

tmp <- read_flare_temp(fc_files[188])
out <- combine_forecast_observation(temp = tmp, obs = sub, return = "ensemble")
idx <- which(out$horizon <= 7 & out$depth %in% c(2))
out$dep <- paste0(out$depth, "m")
p <- ggplot(out[idx, ]) +
  geom_line(aes(horizon, value, group = ens), alpha = 0.4) +
  geom_point(aes(horizon, obs, color = "Obs"), size = 3) +
  # facet_wrap(~dep, ncol = 1, scales = "free_y") +
  ylab("Temperature (\u00B0C)") +
  # xlab("Date") +
  xlab("Horizon (Days)") +
  labs(color = "Data") +
  scale_color_manual(values = c("Obs" = "red")) +
  scale_y_continuous(limits = c(15, 26)) +
  # scale_x_date(date_breaks = "4 days", date_labels = "%m/%d") +
  scale_x_continuous(breaks = seq(0, 16, 1)) +
  theme_classic(base_size = 18)
p
ggsave(file.path(plot_dir, paste0("example_forecast_ensemble_v2.png")), p, device = "png", units = "mm", width = 180, height = 120)

out <- combine_forecast_observation(temp = tmp, obs = sub, return = "summary", min_dep = 0.1, max_dep = 8)
out$date <- as.Date(out$forecast_date) + out$horizon
idx <- which(out$horizon <= 7 & out$depth %in% c(2))
out$dep <- paste0(out$depth, "m")
p <- ggplot(out[idx, ]) +
  geom_ribbon(aes(horizon, ymin = mean - sd, ymax = mean + sd), alpha = 0.3) +
  geom_ribbon(aes(horizon, ymin = mean - 2*sd, ymax = mean + 2*sd), alpha = 0.2) +
  geom_line(aes(horizon, mean, color = "Mean")) +
  geom_point(aes(horizon, obs, color = "Obs"), size = 3) +
  # facet_wrap(~dep, ncol = 1, scales = "free_y") +
  ylab("Temperature (\u00B0C)") +
  # xlab("Date") +
  xlab("Horizon (Days)") +
  labs(color = "Data") +
  scale_color_manual(values = c("Obs" = "red", "Mean" = "black")) +
  mod_leg +
  scale_x_continuous(breaks = seq(0, 16, 1)) +
  scale_y_continuous(limits = c(15, 26)) +
  theme_classic(base_size = 18)
p
ggsave(file.path(plot_dir, paste0("example_forecast_summary_v2.png")), p, device = "png", units = "mm", width = 180, height = 120)

ggplot(out) +
  geom_line(aes(horizon, sd)) +
  facet_wrap(~depth)

# Calculate skill for each forecast date, model & depth ----
idx <- which(mlt$horizon <= 16)
res <- plyr::ddply(mlt[idx,], c("forecast_date", "model"), function(x) {
  error <- x$mean - x$obs
  data.frame(
    RMSE = sqrt(mean((error)^2, na.rm = TRUE)),
    MAE = mean(abs(error), na.rm = TRUE),
    SDAE = sd(abs(error), na.rm = TRUE),
    pbias = 100 * (sum(error, na.rm = TRUE) / sum(x$obs, na.rm = TRUE)),
    CRPS = verification::crps(x$obs, as.matrix(x[, 4:5]))$CRPS,
    Bias = mean(error)
  )
}, .progress = plyr::progress_text(), .parallel = FALSE) # Can switch on parallel if you wanna speed things up

res2 <- plyr::ddply(res, "model", function(x) {
  data.frame(
    forecast_date = x$forecast_date,
    five_day = zoo::rollmeanr(x$RMSE, 5, na.pad = TRUE),
    ten_day = zoo::rollmeanr(x$RMSE, 10, na.pad = TRUE),
    fif_day = zoo::rollmeanr(x$RMSE, 15, na.pad = TRUE)
  )
})

idx <- which(res2$forecast_date > "2021-04-15" & res2$forecast_date < "2021-11-15" & res2$model %in% ler_models)

p1 <- ggplot(res2[idx, ]) +
  geom_line(aes(forecast_date, five_day, color = model, size = model)) +
  # geom_smooth(aes(forecast_date, RMSE, color = model, size = model), method = loess, se = FALSE) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  col_scale +
  mod_leg +
  guides(ma_fun = "none") +
  size_scale +
  labs(color = "Model", size = "Model") +
  coord_cartesian(ylim = c(0, 4)) +
  xlab("Forecast Date") +
  ylab("RMSE (\u00B0C)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_skill_day_ler.png")), p1, device = "png", units = "mm", width = 240, height = 120)

idx <- which(res2$forecast_date > "2021-04-15" & res2$forecast_date < "2021-11-15")

p1 <- ggplot(res2[idx, ]) +
  geom_line(aes(forecast_date, five_day, color = model, size = model)) +
  # geom_smooth(aes(forecast_date, RMSE, color = model, size = model), method = loess, se = FALSE) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  col_scale +
  mod_leg +
  guides(ma_fun = "none") +
  size_scale +
  labs(color = "Model", size = "Model") +
  coord_cartesian(ylim = c(0, 4)) +
  xlab("Forecast Date") +
  ylab("RMSE (\u00B0C)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_skill_day_all.png")), p1, device = "png", units = "mm", width = 240, height = 120)

# Calculate skill for each forecast date, model & depth ----
idx <- which(mlt$horizon %in% c(1, 7, 16))
res <- plyr::ddply(mlt[idx,], c("forecast_date", "model"), function(x) {
  error <- x$mean - x$obs
  data.frame(
    RMSE = sqrt(mean((error)^2, na.rm = TRUE)),
    MAE = mean(abs(error), na.rm = TRUE),
    SDAE = sd(abs(error), na.rm = TRUE),
    pbias = 100 * (sum(error, na.rm = TRUE) / sum(x$obs, na.rm = TRUE)),
    CRPS = verification::crps(x$obs, as.matrix(x[, 4:5]))$CRPS,
    Bias = mean(error)
  )
}, .progress = plyr::progress_text(), .parallel = FALSE) # Can switch on parallel if you wanna speed things up
res$hday <- paste0(res$horizon, "-day")
res$hday <- factor(res$hday, levels = paste0(c(1, 7, 16), "-day"))

res2 <- plyr::ddply(res, c("model", "hday"), function(x) {
  data.frame(
    forecast_date = x$forecast_date,
    five_day = zoo::rollmeanr(x$RMSE, 5, na.pad = TRUE),
    ten_day = zoo::rollmeanr(x$RMSE, 10, na.pad = TRUE),
    fif_day = zoo::rollmeanr(x$RMSE, 15, na.pad = TRUE)
  )
})

start <- as.Date("2021-04-15")
end <- as.Date("2021-11-15")

idx <- which(res2$forecast_date > "2021-04-15" & res2$forecast_date < "2021-11-15" & res2$model %in% ler_models)

p1 <- ggplot(res2[idx, ]) +
  geom_line(aes(forecast_date, five_day, color = model, size = model)) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  col_scale +
  mod_leg +
  size_scale +
  # facet_wrap(~hday, nrow = 1) +
  labs(color = "Model", size = "Model") +
  xlab("Forecast Date") +
  ylab("RMSE (\u00B0C)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(start, end)) +
  scale_y_continuous(breaks = seq(0, 6, 1), limits = c(0, 6)) +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_skill_day_ler.png")), p1, device = "png", units = "mm", width = 240, height = 120)

idx <- which(res2$forecast_date > "2021-04-15" & res2$forecast_date < "2021-11-15")

p1 <- ggplot(res2[idx, ]) +
  geom_line(aes(forecast_date, five_day, color = model, size = model)) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  col_scale +
  mod_leg +
  size_scale +
  labs(color = "Model", size = "Model") +
  xlab("Forecast Date") +
  ylab("RMSE (\u00B0C)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(start, end)) +
  scale_y_continuous(breaks = seq(0, 6, 1), limits = c(0, 6)) +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_skill_day_all.png")), p1, device = "png", units = "mm", width = 240, height = 120)

res3 <- plyr::ddply(res2, "forecast_date", function(x) {
  data.frame(model = x$model[which.min(x$five_day)])
})
res3$y <- 0.2

p1 <- ggplot(res2[idx, ]) +
  geom_tile(data = res3, aes(forecast_date, y, fill = model), height = 0.2) +
  geom_line(aes(forecast_date, five_day, color = model, size = model)) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  col_scale +
  fill_scale +
  mod_leg +
  size_scale +
  labs(color = "Model", size = "Model") +
  xlab("Forecast Date") +
  ylab("RMSE (\u00B0C)") +
  guides(fill = "none") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(start, end)) +
  scale_y_continuous(breaks = seq(0, 6, 1), limits = c(0, 6)) +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_skill_day_all_tile.png")), p1, device = "png", units = "mm", width = 240, height = 120)

pct <- data.frame(LER_mean = 100 *sum(res3$model == "LER mean")/nrow(res3),
                  GLM = 100 *sum(res3$model == "GLM-LER")/nrow(res3),
                  GOTM = 100 *sum(res3$model == "GOTM-LER")/nrow(res3),
                  Simstrat = 100 *sum(res3$model == "Simstrat-LER")/nrow(res3))
pct

# Calculate skill for each forecast date, model & depth ----
idx <- which(mlt$horizon <= 1)
res <- plyr::ddply(mlt[idx,], c("forecast_date", "model", "depth"), function(x) {
  error <- x$mean - x$obs
  data.frame(
    RMSE = sqrt(mean((error)^2, na.rm = TRUE)),
    MAE = mean(abs(error), na.rm = TRUE),
    SDAE = sd(abs(error), na.rm = TRUE),
    pbias = 100 * (sum(error, na.rm = TRUE) / sum(x$obs, na.rm = TRUE)),
    CRPS = verification::crps(x$obs, as.matrix(x[, 4:5]))$CRPS,
    Bias = error
  )
}, .progress = plyr::progress_text(), .parallel = FALSE) # Can switch on parallel if you wanna speed things up

ggplot(res) +
  geom_line(aes(forecast_date, RMSE, color = model)) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  # coord_cartesian(ylim = c(0, 3)) +
  col_scale +
  facet_wrap(~depth) +
  theme_classic(base_size = 18)

res[which(res$RMSE < 2 & res$depth <= 1),]

mlt$date <- mlt$forecast_date +mlt$horizon
sub <- mlt[mlt$forecast_date == "2021-06-21" & mlt$depth %in% c(1, 4, 9) & mlt$horizon <= 16, ]
ggplot(sub) +
  geom_ribbon(aes(date, ymin = mean -sd, ymax = mean+sd, fill = model), alpha = 0.1) +
  geom_line(aes(date, mean, color = model)) +
  ylab("Temperature (\u00B0C)") +
  xlab("") +
  col_scale +
  fill_scale +
  geom_point(aes(date, obs)) +
  facet_wrap(~depth, nrow = 3) +
  theme_classic(base_size = 18)

# Forecast skill depth profiles ----
xlim = c(0, 16)
res <- plyr::ddply(mlt, c("model", "depth", "horizon"), function(x) {
  error <- x$mean - x$obs
  data.frame(
    RMSE = sqrt(mean((error)^2, na.rm = TRUE)),
    MAE = mean(abs(error), na.rm = TRUE),
    SDAE = sd(abs(error), na.rm = TRUE),
    pbias = 100 * (sum(error, na.rm = TRUE) / sum(x$obs, na.rm = TRUE)),
    CRPS = verification::crps(x$obs, as.matrix(x[, 4:5]))$CRPS,
    Bias = mean(error),
    SD = mean(x$sd)
  )
}, .progress = plyr::progress_text(), .parallel = FALSE)

hzns <- c(0, 1, 5, 7, 10, 16)
idx <- which(res$horizon %in% hzns)
res$h_day <- paste0(res$horizon, "-day")
res$h_day <- factor(res$h_day, levels = paste0(1:34, "-day"))

# levels(res$h_day) <- 1:34
ggplot(res[idx, ]) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = c(-2, 2), linetype = "dashed") +
  geom_path(aes(Bias, depth, color = model, size = model)) +
  geom_point(aes(Bias, depth, color = model)) +
  scale_x_continuous(breaks = seq(-6, 6, 2)) +
  scale_y_continuous(breaks = seq(10, 0, -2), trans = "reverse") +
  col_scale +
  size_scale +
  mod_leg +
  ylab("Depth (m)") +
  xlab("Bias (\u00B0C)") +
  labs(color = "Model", size = "Model") +
  facet_wrap(~h_day, nrow = 1) +
  theme_classic(base_size = 15)



hzns <- c(0, 1, 2, 5, 7, 10, 16)
sub <- res[res$horizon <= 16, ]
sub$disc <- cut(sub$Bias, c(-Inf, -1.75, -1.25, -0.75, -0.25, 0.25, 0.75, 1.25, 1.75, Inf))
sub$disc <- factor(sub$disc, labels = c("[<-1.75]", "[-1.75, -1.25]", "[-1.25, -0.75]", "[-0.75, -0.25]", "[-0.25, 0.25]", "[0.25, 0.75]", "[0.75, 1.25]", "[1.25, 1.75]", "[>1.75]"))

div_cols <- RColorBrewer::brewer.pal(length(unique(sub$disc)), "RdYlBu")
disc_col_scale <- scale_color_manual(breaks = levels(sub$disc), values = rev(div_cols), na.value = "transparent", drop = FALSE)


for(h in hzns) {

  sub2 <- sub

  if(h == 0) {
    sub2[sub2$horizon > 1, c("Bias", "SD", "disc")] <- NA
    alpha <- 0
  } else {
    sub2[sub2$horizon > h, c("Bias", "SD", "disc")] <- NA
    alpha <- 1
  }
  sub2 <- na.exclude(sub2)

  p <- ggplot(sub2) +
    geom_point(aes(horizon, depth, color = disc), size = 5.8, shape = 15, alpha = alpha) +
    facet_wrap(~model) +
    xlab("Horizon (days)") +
    ylab("Depth (m)") +
    scale_x_continuous(breaks = seq(1, 16, 2)) +
    scale_y_continuous(breaks = seq(10, 0, -2), trans = "reverse") +
    labs(size = "Std. Dev", color = "Bias (\u00B0C)") +
    theme_classic(base_size = 18) +
    guides(alpha = "none") +
    disc_col_scale +
    coord_cartesian(xlim = c(0, 16)) +
    scale_size_continuous(breaks = seq(0, 3, 0.5), limits = c(0, 3)) +
    theme(
      panel.background = element_rect(fill = "transparent"), # bg of the panel
      strip.background = element_rect(fill = "transparent", color = "white"),
      axis.title = element_text(color = "white"),
      axis.text = element_text(color = "white"),
      axis.ticks = element_line(color = "white"),
      axis.line = element_line(color = "white"),
      strip.text = element_text(color = "white"),
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      legend.background = element_rect(fill = "gray"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    )
  p

  ggsave(file.path(plot_dir, paste0("forecast_depth_bias_uncertainty_h", h, ".png")), p, device = "png", units = "mm", width = 240, height = 120, bg = "transparent")
}






# Calculate skill for each horizon, model & depth ----
res2 <- plyr::ddply(mlt, c("horizon", "model", "depth"), function(x) {
  error <- x$mean - x$obs
  data.frame(
    RMSE = sqrt(mean((error)^2, na.rm = TRUE)),
    MAE = mean(abs(error), na.rm = TRUE),
    SDAE = sd(abs(error), na.rm = TRUE),
    pbias = 100 * (sum(error, na.rm = TRUE) / sum(x$obs, na.rm = TRUE)),
    CRPS = verification::crps(x$obs, as.matrix(x[, 4:5]))$CRPS,
    sd = mean(x$sd)
  )
}, .progress = plyr::progress_text(), .parallel = FALSE)
ggplot(res2[res2$horizon <= 16, ]) +
  geom_ribbon(aes(horizon, ymin = MAE-SDAE, ymax = MAE+SDAE, fill = model), alpha = 0.2) +
  geom_line(aes(horizon, MAE, color = model)) +
  facet_wrap(~depth) +
  coord_cartesian(xlim = xlim) +
  theme_classic(base_size = 18)


hzns <- c(0, 1, 2, 5, 7, 10, 16)
idx <- which(res2$horizon %in% hzns & res2$depth %in% c(1, 4, 9))
res2$depth_c <- paste0(res2$depth, "m")

for(h in hzns) {

  sub2 <- res2[idx, ]
  if(h == 0) {
    # sub2$RMSE[sub2$horizon > 1] <- NA
    alpha <- 0
  } else {
    sub2$RMSE[sub2$horizon > h] <- NA
    alpha <- 1
  }


  p <- ggplot(sub2) +
    geom_hline(yintercept = 2, linetype = "dashed") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(aes(sd, RMSE, color = model), alpha = alpha) +
    geom_point(aes(sd, RMSE, color = model, shape = factor(horizon)), size = 3, alpha = alpha) +
    labs(color = "Model", shape = "Horizon (days)") +
    col_scale +
    mod_leg +
    size_scale +
    guides(size = "none", shape = guide_legend(override.aes = list(alpha = 1))) +
    # geom_point(aes(sd, RMSE, color = horizon, shape = model)) +
    facet_wrap(~depth_c, nrow = 1) +
    coord_cartesian(xlim = c(0, 2.5), ylim = c(0, 5.5)) +
    scale_y_continuous(breaks = seq(0, 5, 1), expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(0, 2.0, 0.5), expand = c(0, 0)) +
    xlab("Std. dev. (\u00B0C)") +
    ylab("RMSE (\u00B0C)") +
    theme_classic(base_size = 16)
  p

  ggsave(file.path(plot_dir, paste0("forecast_error_uncertainty_h", h, ".png")), p, device = "png", units = "mm", width = 240, height = 120)
}




ggplot(res2[idx, ]) +
  geom_hline(yintercept = 0) +
  # geom_line(aes(sd, RMSE, group = model)) +
  geom_point(aes(sd, RMSE, color = depth, shape = model)) +
  facet_wrap(~horizon) +
  xlab("Std. dev. (\u00B0C)") +
  ylab("RMSE (\u00B0C)") +
  theme_classic(base_size = 18)

mlt2 <- reshape2::melt(res2[, c(1:4, 9)], id =1:3)
idx <- which(mlt2$depth %in% c(1, 4, 9))
ggplot(mlt2[idx, ]) +
  geom_hline(yintercept = 0) +
  geom_line(aes(horizon, value, color = model)) +
  facet_grid(depth~variable) +
  ylab("\u00B0C") +
  theme_classic(base_size = 18)



# Calculate skill for each horizon, model & depth ----
res2 <- plyr::ddply(mlt, c("horizon", "model", "depth"), function(x) {
  data.frame(
    RMSE = sqrt(mean((x$mean - x$obs)^2, na.rm = TRUE)),
    MAE = mean(abs(x$mean - x$obs), na.rm = TRUE),
    pbias = 100 * (sum(x$mean - x$obs, na.rm = TRUE) / sum(x$obs, na.rm = TRUE)),
    CRPS = verification::crps(x$obs, as.matrix(x[, 4:5]))$CRPS
  )
}, .progress = plyr::progress_text(), .parallel = FALSE)

idx <- which(res2$depth %in% c(1, 4, 9) & res2$horizon <= 16)
res2$dep <- paste0(res2$depth, "m")
ggplot(res2[idx, ]) +
  geom_line(aes(horizon, RMSE, color = model, size = model)) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  col_scale +
  size_scale +
  mod_leg +
  labs(color = "Model") +
  guides(size = "none") +
  xlab("Horizon (days)") +
  ylab("RMSE (\u00B0C)") +
  facet_wrap(~dep) +
  theme_classic(base_size = 18)

ggplot(res2[idx, ]) +
  geom_line(aes(horizon, CRPS, color = model)) +
  geom_point(aes(horizon, CRPS, color = model)) +
  # geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  labs(color = "Model") +
  # coord_cartesian(ylim = c(0, 3)) +
  facet_wrap(~depth, nrow = 3) +
  theme_classic(base_size = 18)

# Thermocline depth ----
#* Calculate thermocline for each forecast date, horizon & depth ----
# res3 <- plyr::ddply(mlt, c("forecast_date", "horizon", "model"), function(x) {
#
#   td.mod <- x$td_mean[1]
#   td.obs <- rLakeAnalyzer::thermo.depth(x$obs, x$depth)
#
#   data.frame(
#     RMSE = sqrt(mean((td.mod - td.obs)^2, na.rm = TRUE)),
#     MAE = mean(abs(td.mod - td.obs), na.rm = TRUE),
#     pbias = 100 * (sum(td.mod - td.obs, na.rm = TRUE) / sum(td.obs, na.rm = TRUE)),
#     Bias = mean(td.mod - td.obs, na.rm = TRUE)
#   )
# }, .progress = plyr::progress_text())

res3 <- plyr::ddply(mlt, c("forecast_date", "horizon", "model"), function(x) {

  td.mod <- rLakeAnalyzer::thermo.depth(x$mean, x$depth, seasonal = FALSE)
  td.obs <- rLakeAnalyzer::thermo.depth(x$obs, x$depth, seasonal = FALSE)
  meta.mod <- rLakeAnalyzer::meta.depths(x$mean, x$depth, slope = 0.5, seasonal = FALSE)
  meta.obs <- rLakeAnalyzer::meta.depths(x$obs, x$depth, slope = 0.5, seasonal = FALSE)

  data.frame(
    RMSE = sqrt(mean((td.mod - td.obs)^2, na.rm = TRUE)),
    MAE = mean(abs(td.mod - td.obs), na.rm = TRUE),
    pbias = 100 * (sum(td.mod - td.obs, na.rm = TRUE) / sum(td.obs, na.rm = TRUE)),
    Bias = mean(td.mod - td.obs, na.rm = TRUE),
    epi_Bias = mean(meta.mod[1] - meta.obs[1], na.rm = TRUE),
    hyp_Bias = mean(meta.mod[2] - meta.obs[2], na.rm = TRUE)
  )
}, .progress = plyr::progress_text())

#* THEN calculate mean Bias for ech horizon & model

res3$hday <- paste0(res3$horizon, "-day")
res3$hday <- factor(res3$hday, levels = paste0(1:34, "-day"))
res3$hgroup <- NA
res3$hgroup[res3$horizon <= 5] <- "1-5 day"
res3$hgroup[res3$horizon > 5 & res3$horizon <= 10] <- "6-10 day"
res3$hgroup[res3$horizon > 10 & res3$horizon <= 16] <- "11-16 day"
res3$hgroup[res3$horizon > 16] <- ">16 day"
res3$hgroup <- factor(res3$hgroup, levels = c("1-5 day", "6-10 day", "11-16 day", ">16 day"))
# levels(res3$hgroup) <- c("1-5 day", "6-10 day", "11-15 day", ">15 day")
# idx <- which(res3$forecast_date > "2021-04-01" & res3$forecast_date < "2021-11-01" & res3$horizon %in% c(1, 7, 16))
res4 <- plyr::ddply(res3, c("forecast_date", "model", "hgroup"), function(x) {
  data.frame(
    td_mean = mean(x$Bias, na.rm = TRUE),
    epi_mean = mean(x$epi_Bias, na.rm = TRUE),
    hyp_mean = mean(x$hyp_Bias, na.rm = TRUE),
    sd = sd(x$epi_Bias, na.rm = TRUE)
  )
}, .progress = plyr::progress_text())

res5 <- plyr::ddply(res4, c("model", "hgroup"), function(x) {
  data.frame(
    forecast_date = x$forecast_date,
    five_td = zoo::rollmeanr(x$td_mean, 5, na.pad = TRUE),
    five_epi = zoo::rollmeanr(x$epi_mean, 5, na.pad = TRUE),
    five_hyp = zoo::rollmeanr(x$hyp_mean, 5, na.pad = TRUE)
  )
})

mlt2 <- reshape::melt(res5, id.vars = c("model", "hgroup", "forecast_date"))
mlt2$variable <- factor(mlt2$variable)
mlt2$variable <- factor(mlt2$variable, labels = c("Thermocline", "Epilimnion", "Hypolimnion"))
levels(mlt2$variable) <- c("Epilimnion", "Thermocline", "Hypolimnion")

idx <- which(mlt2$hgroup != ">16 day")
p1 <- ggplot(mlt2[idx, ]) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1), linetype = "dashed") +
  geom_line(aes(forecast_date, value, color = model, size = model), alpha = 1)+
  scale_y_continuous(breaks = seq(-4, 4, 1), limits = c(-3, 4)) +
  facet_grid(variable~hgroup) +
  col_scale +
  mod_leg +
  fill_scale +
  size_scale +
  labs(color = "Model") +
  guides(size = "none") +
  xlab("Date") +
  ylab("Bias (m)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_meta_td_bias_none_vert.png")), p1, device = "png", units = "mm", width = 280, height = 240)

idx <- which(mlt2$hgroup != ">16 day" & mlt2$model != "LER mean")
p1 <- ggplot(mlt2[idx, ]) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1), linetype = "dashed") +
  geom_line(aes(forecast_date, value, color = model, size = model), alpha = 1)+
  scale_y_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4)) +
  facet_grid(hgroup~variable) +
  col_scale +
  mod_leg +
  fill_scale +
  size_scale +
  labs(color = "Model") +
  guides(size = "none") +
  xlab("Date") +
  ylab("Bias (m)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_metalimnion_bias_LER_vert.png")), p1, device = "png", units = "mm", width = 280, height = 240)

idx <- which(mlt2$hgroup != ">16 day")
p1 <- ggplot(mlt2[idx, ]) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1), linetype = "dashed") +
  geom_line(aes(forecast_date, value, color = model, size = model), alpha = 1)+
  scale_y_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4)) +
  facet_grid(hgroup~variable) +
  col_scale +
  mod_leg +
  fill_scale +
  size_scale +
  labs(color = "Model") +
  guides(size = "none") +
  xlab("Date") +
  ylab("Bias (m)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_metalimnion_bias_all_vert.png")), p1, device = "png", units = "mm", width = 280, height = 240)

# Back to thermocline ----

idx <- which(mlt2$hgroup == "1-5 day" & mlt2$variable == "Thermocline")

res6 <- plyr::ddply(mlt2[idx, ], "forecast_date", function(x) {
  data.frame(model = x$model[which.min(abs(x$value))])
})
res6$y <- -1.75


pct <- data.frame(LER_mean = 100 *sum(res6$model == "LER mean")/nrow(res6),
                  GLM = 100 *sum(res6$model == "GLM-LER")/nrow(res6),
                  GOTM = 100 *sum(res6$model == "GOTM-LER")/nrow(res6),
                  Simstrat = 100 *sum(res6$model == "Simstrat-LER")/nrow(res6))

idx <- which(mlt2$hgroup == "1-5 day" & mlt2$variable == "Thermocline" &mlt2$model %in% ler_models)
p1 <- ggplot(mlt2[idx, ]) +
  # geom_tile(data = res6, aes(forecast_date, y, fill = model), height = 0.2) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1), linetype = "dashed") +
  geom_line(aes(forecast_date, value, color = model, size = model), alpha = 0)+
  scale_y_continuous(breaks = seq(-4, 4, 1), limits = c(-2, 3)) +
  col_scale +
  mod_leg +
  fill_scale +
  size_scale +
  labs(color = "Model") +
  guides(size = "none", fill = "none") +
  xlab("Date") +
  ylab("Bias (m)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(start, end)) +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_td_bias_none_1-5day.png")), p1, device = "png", units = "mm", width = 240, height = 120)

idx <- which(mlt2$hgroup == "1-5 day" & mlt2$variable == "Thermocline" &mlt2$model %in% ler_models)
p1 <- ggplot(mlt2[idx, ]) +
  # geom_tile(data = res6, aes(forecast_date, y, fill = model), height = 0.2) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1), linetype = "dashed") +
  geom_line(aes(forecast_date, value, color = model, size = model), alpha = 1)+
  scale_y_continuous(breaks = seq(-4, 4, 1), limits = c(-2, 3)) +
  col_scale +
  mod_leg +
  fill_scale +
  size_scale +
  labs(color = "Model") +
  guides(size = "none", fill = "none") +
  xlab("Date") +
  ylab("Bias (m)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(start, end)) +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_td_bias_LER_1-5day.png")), p1, device = "png", units = "mm", width = 240, height = 120)

idx <- which(mlt2$hgroup == "1-5 day" & mlt2$variable == "Thermocline")
p1 <- ggplot(mlt2[idx, ]) +
  # geom_tile(data = res6, aes(forecast_date, y, fill = model), height = 0.2) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1), linetype = "dashed") +
  geom_line(aes(forecast_date, value, color = model, size = model), alpha = 1)+
  scale_y_continuous(breaks = seq(-4, 4, 1), limits = c(-2, 3)) +
  col_scale +
  mod_leg +
  fill_scale +
  size_scale +
  labs(color = "Model") +
  guides(size = "none", fill = "none") +
  xlab("Date") +
  ylab("Bias (m)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(start, end)) +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_td_bias_all_1-5day.png")), p1, device = "png", units = "mm", width = 240, height = 120)

p1 <- ggplot(mlt2[idx, ]) +
  geom_tile(data = res6, aes(forecast_date, y, fill = model), height = 0.2) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1), linetype = "dashed") +
  geom_line(aes(forecast_date, value, color = model, size = model), alpha = 1)+
  scale_y_continuous(breaks = seq(-4, 4, 1), limits = c(-2, 3)) +
  col_scale +
  mod_leg +
  fill_scale +
  size_scale +
  labs(color = "Model") +
  guides(size = "none", fill = "none") +
  xlab("Date") +
  ylab("Bias (m)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(start, end)) +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_td_bias_all_1-5day_tile.png")), p1, device = "png", units = "mm", width = 240, height = 120)

idx <- which(mlt2$hgroup != ">16 day" & mlt2$variable == "Thermocline")
p1 <- ggplot(mlt2[idx, ]) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1), linetype = "dashed") +
  geom_line(aes(forecast_date, value, color = model, size = model), alpha = 0)+
  scale_y_continuous(breaks = seq(-4, 4, 1), limits = c(-3, 4)) +
  facet_grid(hgroup~variable) +
  col_scale +
  mod_leg +
  fill_scale +
  size_scale +
  labs(color = "Model") +
  guides(size = "none") +
  xlab("Date") +
  ylab("Bias (m)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_td_bias_none_vert.png")), p1, device = "png", units = "mm", width = 280, height = 240)

idx <- which(mlt2$hgroup != ">16 day" & mlt2$model != "LER mean" & mlt2$variable == "Thermocline")
p1 <- ggplot(mlt2[idx, ]) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1), linetype = "dashed") +
  geom_line(aes(forecast_date, value, color = model, size = model), alpha = 1)+
  scale_y_continuous(breaks = seq(-4, 4, 1), limits = c(-3, 4)) +
  facet_grid(hgroup~variable) +
  col_scale +
  mod_leg +
  fill_scale +
  size_scale +
  labs(color = "Model") +
  guides(size = "none") +
  xlab("Date") +
  ylab("Bias (m)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_td_bias_LER_vert.png")), p1, device = "png", units = "mm", width = 280, height = 240)

idx <- which(mlt2$hgroup != ">16 day" & mlt2$variable == "Thermocline")
p1 <- ggplot(mlt2[idx, ]) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1), linetype = "dashed") +
  geom_line(aes(forecast_date, value, color = model, size = model), alpha = 1)+
  scale_y_continuous(breaks = seq(-4, 4, 1), limits = c(-3, 4)) +
  facet_grid(hgroup~variable) +
  col_scale +
  mod_leg +
  fill_scale +
  size_scale +
  labs(color = "Model") +
  guides(size = "none") +
  xlab("Date") +
  ylab("Bias (m)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_td_bias_all_vert.png")), p1, device = "png", units = "mm", width = 280, height = 240)


# Mixing event ----
tgt_date <- seq.Date(as.Date("2021-05-31"), as.Date("2021-06-04"), 1)
to_fc <- mlt[mlt$date %in% as.Date(tgt_date) & mlt$horizon <= 16,  ]
fc1 <- plyr::ddply(to_fc, c("forecast_date", "horizon", "model"), function(x) {
  data.frame(td_obs = rLakeAnalyzer::thermo.depth(x$obs, x$depth),
             td_fc = rLakeAnalyzer::thermo.depth(x$mean, x$depth))
})
fc1$Date <- fc1$forecast_date + fc1$horizon
ggplot(fc1) +
  geom_line(aes(Date, td_fc, color = model)) +
  geom_point(aes(Date, td_obs), color = "red") +
  scale_y_reverse() +
  facet_wrap(~forecast_date)



res5 <- plyr::ddply(res3[idx, ], c("model", "hday"), function(x) {
  data.frame(
    forecast_date = x$forecast_date,
    five_day = zoo::rollmeanr(x$Bias, 5, na.pad = TRUE),
    ten_day = zoo::rollmeanr(x$Bias, 10, na.pad = TRUE),
    fif_day = zoo::rollmeanr(x$Bias, 15, na.pad = TRUE)
  )
})


# LER plot
idx <- which(res5$model %in% ler_models)

p1 <- ggplot(res5[idx, ]) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1), linetype = "dashed") +
  geom_line(aes(forecast_date, five_day, color = model, size = model), alpha = 0)+
  scale_y_continuous(breaks = seq(-4, 8, 1)) +
  facet_wrap(~hday, nrow = 3) +
  col_scale +
  mod_leg +
  fill_scale +
  size_scale +
  labs(color = "Model") +
  guides(size = "none") +
  xlab("Date") +
  ylab("Bias (m)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_thermocline_bias_none.png")), p1, device = "png", units = "mm", width = 240, height = 360)

p1 <- ggplot(res5[idx, ]) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1), linetype = "dashed") +
  geom_line(aes(forecast_date, five_day, color = model, size = model))+
  scale_y_continuous(breaks = seq(-4, 8, 1)) +
  facet_wrap(~hday, nrow = 1) +
  col_scale +
  mod_leg +
  fill_scale +
  size_scale +
  labs(color = "Model") +
  guides(size = "none") +
  xlab("Date") +
  ylab("Bias (m)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_thermocline_bias_LER.png")), p1, device = "png", units = "mm", width = 240, height = 120)


p2 <- ggplot(res5) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1), linetype = "dashed") +
  geom_line(aes(forecast_date, five_day, color = model, size = model))+
  scale_y_continuous(breaks = seq(-4, 8, 0.5)) +
  facet_wrap(~hday, nrow = 3) +
  col_scale +
  mod_leg +
  fill_scale +
  size_scale +
  labs(color = "Model") +
  guides(size = "none") +
  xlab("Date") +
  ylab("Bias (m)") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_classic(base_size = 18)
p2
ggsave(file.path(plot_dir, paste0("forecast_thermocline_bias_all.png")), p2, device = "png", units = "mm", width = 240, height = 120)

#** Find peaks ----
peaks <- which(res5$five_day > 1)
res5[peaks, 1:2]
eve1 <- c("2021-05-19", "2021-06-09")

sub3 <- sub[sub$Date >= eve1[1] & sub$Date <= eve1[2], ]
idx <- which(td$Date >= eve1[1] & td$Date <= eve1[2])
p1 <- ggplot(sub3) +
  geom_contour_filled(aes(Date, depth, z = value), binwidth = 2) +
  geom_line(data = td[idx, ], aes(Date, td), color = "black", size = 1.2) +
  # geom_line(data = td, aes(Date, five_day), color = "black", size = 1.2) +
  scale_y_continuous(breaks = seq(10, 0, -2), trans = "reverse") +
  scale_x_date(date_breaks = "3 day", date_labels = "%d-%b") +
  scale_fill_gradientn(colours = c("#0A2864", "#CCD9FF", "#FFF9CF", "#FEBF00", "#E6281E", "#6C0000"),
                       super = metR::ScaleDiscretised) +
  labs(fill = "Temperature\n(\u00B0C)") +
  coord_cartesian(xlim = c(as.Date(eve1[1]), as.Date(eve1[2]))) +
  ylab("Depth (m)") +
  theme_classic(base_size = 18) +
  theme(legend.key.height = unit(1.5, "cm"))
ggsave(file.path(plot_dir, paste0("obs_thermocline_event1.png")), p1, device = "png", units = "mm", width = 240, height = 120)


wid <- tidyr::pivot_wider(res5, id_cols = "forecast_date",
                          names_from = model, values_from = five_day)
ggplot(wid) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(`GLM-Thomas`, `GLM-LER`, shape = "GLM")) +
  geom_point(aes(`GLM-Thomas`, `GOTM-LER`, shape = "GOTM")) +
  geom_point(aes(`GLM-Thomas`, `Simstrat-LER`, shape = "Simstrat")) +
  geom_point(aes(`GLM-Thomas`, `LER mean`, shape = "Ensemble mean")) +
  coord_cartesian(xlim = c(-1, 2.5), ylim = c(-1, 2.5))


# Schmidt stability ----

res3 <- plyr::ddply(mlt, c("forecast_date", "horizon", "model"), function(x) {

  td.mod <- rLakeAnalyzer::schmidt.stability(x$mean, x$depth, bthA = bathy$Area_meterSquared, bthD = bathy$Depth_meter)
  td.obs <- rLakeAnalyzer::schmidt.stability(x$obs, x$depth, bthA = bathy$Area_meterSquared, bthD = bathy$Depth_meter)

  data.frame(
    RMSE = sqrt(mean((td.mod - td.obs)^2, na.rm = TRUE)),
    MAE = mean(abs(td.mod - td.obs), na.rm = TRUE),
    pbias = 100 * (sum(td.mod - td.obs, na.rm = TRUE) / sum(td.obs, na.rm = TRUE))
  )
}, .progress = plyr::progress_text())

res4 <- plyr::ddply(res3, c("horizon", "model"), function(x) {
  data.frame(
    mean = mean(x$RMSE, na.rm = TRUE),
    sd = sd(x$RMSE, na.rm = TRUE)
  )
})

ggplot(res4) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(horizon, ymin = mean -sd, ymax = mean +sd, fill = model), alpha = 0.1) +
  geom_line(aes(horizon, mean, color = model))+
  geom_hline(yintercept = 2, linetype = "dashed") +
  ylab("RMSE (m)") +
  theme_classic(base_size = 18)


# Turnover forecast

# Read in all the forecast_summary csv files
out2 <- lapply(models, function(m) {
  fils <- list.files(file.path(config$file_path$analysis_directory, paste0(m, "_test")),
                     pattern = "forecast_summary_", full.names = TRUE)
  readr::read_csv(fils)
})

names(out2) <- models

# melt the list into a long dataframe
mlt2 <- reshape2::melt(out2, id.vars = colnames(out2$GLM))
mlt2$date <- mlt2$forecast_date + mlt2$horizon
mlt2 <- mlt2[mlt2$horizon <= 34, ]
colnames(mlt2)[9] <- "model"
mlt2$model[mlt2$model == "ens_mean"] <- "LER mean"
mlt2$model[mlt2$model == "flare_realtime"] <- "GLM-Thomas"
mlt2$model <- factor(mlt2$model, levels = c("GLM", "GOTM", "Simstrat", "GLM-Thomas", "LER mean"),
                     labels = c("GLM-LER", "GOTM-LER", "Simstrat-LER", "GLM-Thomas", "LER mean"))
mlt2$lsiz <- 1
mlt2$lsiz[mlt2$model == "LER mean"] <- 1.2

# Temp Date: 2021-11-03
tgt_date <- seq.Date(as.Date("2021-11-01"), as.Date("2021-11-07"), 1)
to_fc <- mlt2[mlt2$date %in% as.Date(tgt_date) & mlt2$depth == 1 & mlt2$horizon <= 16, ]
# to_fc <- mlt[mlt$date %in% as.Date(tgt_date) & mlt$depth == 1 & mlt$horizon <= 16, ]
# to_fc <- mlt2[mlt2$forecast_date >= "2021-10-15" & mlt2$forecast_date <= "2021-11-07" & mlt2$depth == 1, ]
to_fc$fdate <- factor(to_fc$date)
to_fc$hday <- paste0(to_fc$horizon, "-day")
to_fc$hday <- factor(to_fc$hday, levels = paste0(1:16, "-day"))
to_fc$status <- "Unlikely"

thresholds <- c(33.33, 66.66)
to_fc$status[to_fc$temp_turnover_pct <= thresholds[1]] <- "Unlikely"
to_fc$status[to_fc$temp_turnover_pct > thresholds[1] & to_fc$temp_turnover_pct <= thresholds[2]] <- "Likely"
to_fc$status[to_fc$temp_turnover_pct > thresholds[2]] <- "Highly likely"
to_fc$status <- factor(to_fc$status, levels = c("Unlikely", "Likely", "Highly likely"))


ggplot(to_fc) +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-11-07")) +
  geom_line(aes(forecast_date, turnover_pct, color = model)) +
  col_scale +
  coord_cartesian(ylim = c(0, 100)) +
  mod_leg +
  facet_wrap(~fdate) +
  theme_classic(base_size = 18)

temp_date <- as.Date("2021-11-06")
idx <- which(to_fc$date == temp_date & to_fc$model %in% ler_models)
to_fc$days_before <- as.numeric(temp_date - to_fc$forecast_date)
p1 <- ggplot(to_fc[idx, ]) +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_vline(xintercept = 0) +
  geom_line(aes(days_before, turnover_pct, color = model, size = model)) +
  scale_color_discrete() +
  col_scale +
  size_scale +
  labs(color = "Model") +
  ylab("Percentage chance of turnover (%)") +
  xlab(paste0("Days before turnover\n(", temp_date, ")")) +
  scale_x_continuous(breaks = seq(0, 16, 1), trans = "reverse") +
  guides(size = "none") +
  coord_cartesian(ylim = c(0, 100)) +
  mod_leg +
  # facet_wrap(~model) +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_turnover_density_pct_ler.png")), p1, device = "png", units = "mm", width = 240, height = 120)

idx <- which(to_fc$date == temp_date)
to_fc$days_before <- as.numeric(temp_date - to_fc$forecast_date)
p1 <- ggplot(to_fc[idx, ]) +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_vline(xintercept = 0) +
  geom_line(aes(days_before, turnover_pct, color = model, size = model)) +
  scale_color_discrete() +
  col_scale +
  size_scale +
  labs(color = "Model") +
  ylab("Percentage chance of turnover (%)") +
  xlab(paste0("Days before turnover\n(", temp_date, ")")) +
  scale_x_continuous(breaks = seq(0, 16, 1), trans = "reverse") +
  guides(size = "none") +
  coord_cartesian(ylim = c(0, 100)) +
  mod_leg +
  # facet_wrap(~model) +
  theme_classic(base_size = 18)
p1
ggsave(file.path(plot_dir, paste0("forecast_turnover_density_pct_all.png")), p1, device = "png", units = "mm", width = 240, height = 120)


sub3 <- sub[sub$Date >= temp_date-18 & sub$Date <= temp_date+4, ]
idx <- which(td$Date >= temp_date-18 & td$Date <= temp_date+4)
p1 <- ggplot(sub3) +
  geom_contour_filled(aes(Date, depth, z = value), binwidth = 1) +
  geom_vline(xintercept = temp_date, linetype = "dashed", size = 2) +
  geom_line(data = td[idx, ], aes(Date, td), color = "black", size = 1.2) +
  scale_y_continuous(breaks = seq(10, 0, -2), trans = "reverse") +
  scale_x_date(breaks = seq.Date(temp_date-15, temp_date+4, by = 3), date_labels = "%d-%b") +
  scale_fill_gradientn(colours = c("#0A2864", "#CCD9FF", "#FFF9CF", "#FEBF00", "#E6281E", "#6C0000"),
                       super = metR::ScaleDiscretised) +
  labs(fill = "Temperature\n(\u00B0C)") +
  # coord_cartesian(xlim = c(as.Date(eve1[1]), as.Date(eve1[2]))) +
  ylab("Depth (m)") +
  theme_classic(base_size = 18) +
  theme(legend.key.height = unit(1.5, "cm"))
p1
ggsave(file.path(plot_dir, paste0("obs_thermocline_turnover.png")), p1, device = "png", units = "mm", width = 240, height = 120)


ggplot(to_fc[idx, ]) +
  geom_raster(aes(days_before, model, fill = status)) +
  scale_x_continuous(breaks = seq(0, 16, 1), trans = "reverse") +
  ylab("Model") +
  xlab(paste0("Days before turnover\n(", temp_date, ")")) +
  labs(fill = "Likelihood of\nturnover") +
  theme_classic(base_size = 18)


obs <- mlt[mlt$date %in% as.Date(tgt_date) & mlt$model == "GLM" & mlt$horizon == 1, ]
idx <- which(obs$depth %in% c(0, 9))
ggplot(obs[idx, ]) +
  geom_line(aes(date, obs, color = factor(depth)))




# Index vi
sub <- mlt[mlt$forecast_date == "2021-06-21" & mlt$horizon <= 16, ]
sub$psiz <- sub$sd / max(sub$sd)
sub$err <- sub$mean - sub$obs
obs <- sub[sub$model == "GLM", ]
obs$mean <- obs$obs
obs$sd <- 3
obs$model <- "Obs"

dat <- rbind(sub, obs)

ggplot(dat) +
  geom_point(aes(horizon, depth, color = mean, size = sd)) +
  # geom_raster(aes(horizon, depth, fill = mean, alpha = psiz)) +
  facet_wrap(~model) +
  xlab("Horizon (days)") +
  ylab("Depth (m)") +
  scale_y_reverse() +
  # scale_alpha_continuous(range = c(1, 0.1)) +
  theme_classic(base_size = 18) +
  scale_colour_gradientn(limits = c(7, 32),
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral")))

ggplot(sub) +
  geom_point(aes(horizon, depth, color = err, size = sd)) +
  # geom_raster(aes(horizon, depth, fill = mean, alpha = psiz)) +
  facet_wrap(~model) +
  xlab("Horizon (days)") +
  ylab("Depth (m)") +
  scale_y_reverse() +
  # scale_alpha_continuous(range = c(1, 0.1)) +
  theme_classic(base_size = 18) +
  scale_colour_gradientn(limits = c(-2, 2),
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral")))



