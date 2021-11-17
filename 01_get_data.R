lake_directory <- getwd()
config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_flare.yml"))
config$file_path$noaa_directory <- file.path(lake_directory, "forecasted_drivers", config$met$forecast_met_model)


# Create directories
dir.create("data_raw/fcre-manual-data", showWarnings = FALSE)
dir.create("data_raw/fcre-weir-data", showWarnings = FALSE)
dir.create("data_raw/fcre-metstation-data", showWarnings = FALSE)
dir.create("data_raw/fcre-catwalk-data", showWarnings = FALSE)


#download CTD data from EDI
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.200.11&entityid=d771f5e9956304424c3bc0a39298a5ce",
              "data_raw/fcre-manual-data/CTD_final_2013_2020.csv")

#download various field_data files
download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-manual-data/FCR_SSS_inflow_2013_2020.csv",
              "data_raw/fcre-manual-data/FCR_SSS_inflow_2013_2020.csv")
download.file("https://github.com/FLARE-forecast/FCRE-data/blob/fcre-manual-data/FCR_GLM_NLDAS_010113_123119_GMTadjusted.csv?raw=true",
              "data_raw/fcre-manual-data/FCR_GLM_NLDAS_010113_123119_GMTadjusted.csv")
download.file("https://github.com/FLARE-forecast/FCRE-data/blob/fcre-weir-data/FCRweir.csv?raw=true",
              "data_raw/fcre-weir-data/FCRweir.csv")
download.file("https://github.com/FLARE-forecast/FCRE-data/blob/fcre-manual-data/inflow_for_EDI_2013_06Mar2020.csv?raw=true",
              "data_raw/fcre-manual-data/inflow_for_EDI_2013_06Mar2020.csv")
download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-manual-data/FCR_weir_inflow_2013_2019_20200624_allfractions_2poolsDOC.csv",
              "data_raw/fcre-manual-data/FCR_weir_inflow_2013_2019_20200624_allfractions_2poolsDOC.csv")
download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-manual-data/FCR_spillway_outflow_SUMMED_WeirWetland_2013_2019_20200615.csv",
              "data_raw/fcre-manual-data/FCR_spillway_outflow_SUMMED_WeirWetland_2013_2019_20200615.csv")
download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-manual-data/Catwalk_cleanedEDI.csv",
              "data_raw/fcre-manual-data/Catwalk_cleanedEDI.csv")
download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/Catwalk.csv",
              "data_raw/fcre-catwalk-data/Catwalk.csv")
download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data/CAT_MaintenanceLog.txt",
              "data_raw/fcre-catwalk-data/CAT_MaintenanceLog.txt")
download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-manual-data/Secchi_depth_2013-2019.csv",
              "data_raw/fcre-manual-data/Secchi_depth_2013-2019.csv")
download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-manual-data/chemistry.csv",
              "data_raw/fcre-manual-data/chemistry.csv")
download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-manual-data/CTD_final_2013_2019.csv",
              "data_raw/fcre-manual-data/CTD_final_2013_2019.csv")
download.file("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-manual-data/FCR_SSS_inflow_2013_2020.csv",
              "data_raw/fcre-manual-data/FCR_SSS_inflow_2013_2020.csv")


# download FCR met data
download.file("https://github.com/FLARE-forecast/FCRE-data/blob/fcre-metstation-data/FCRmet.csv?raw=true",
              "data_raw/fcre-metstation-data/FCRmet.csv")
download.file("https://github.com/FLARE-forecast/FCRE-data/blob/fcre-manual-data/met_full_postQAQC.csv?raw=true",
              "data_raw/fcre-manual-data/met_full_postQAQC.csv")



#download EDI fcr met file
# download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.389.5&entityid=3d1866fecfb8e17dc902c76436239431",
#               "data_raw/Met_final_2015_2020.csv")


#download BVR temp data from EDI
# inUrl1  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.725.1&entityid=9f4d77dc90db2d87e4cdec8b7584d504"
# infile1 <- paste0(config$data_location,"/BVR_EDI_2020.csv")
# download.file(inUrl1,infile1,method="curl")



# Download NOAA Data using noaaGEFSpoints ----
# remotes::install_github("rqthomas/noaaGEFSpoint")
job <- rstudioapi::jobRunScript(path = "R/noaaGEFSpoint_download.R",
                                name = "Download NOAA",
                                exportEnv = "R_GlobalEnv",
                                workingDir = lake_directory)

# Downloading from S3 Bucket ----
source(file.path(lake_directory, "R", "noaa_download_s3.R"))

# set a start and end date for NOAA forecasts and check which days are not available in local NOAA directory
dates <- seq.Date(as.Date('2021-07-01'), as.Date(Sys.Date()), by = 'day')
download_dates <- c()
for (i in 1:length(dates)) {
  fpath <- file.path(config$file_path$noaa_directory, "NOAAGEFS_1hr", "fcre", dates[i])
  if(dir.exists(fpath)){
    message(paste0(dates[i], ' already downloaded'))
  }else{
    download_dates <- c(download_dates, dates[i])
  }
}

download_dates <- na.omit(download_dates)
download_dates <- as.Date(download_dates, origin = '1970-01-01')


for (i in 1:length(download_dates)) {
  noaa_download_s3(siteID = config$location$site_id,
                   date = download_dates[i],
                   cycle = '00',
                   noaa_horizon = 16,
                   noaa_directory = config$file_path$noaa_directory)

}

# noaa-point is 16 day
fc_files <- list.files(file.path(config$file_path$noaa_directory, config$location$site_id,
                                 download_dates[1], "00"),
                       full.names = TRUE)

# Inspect netcdf files
fid <- ncdf4::nc_open(fc_files[1])
fid$dim$longitude$vals
ncdf4::nc_close(fid)

#####
