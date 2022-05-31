

read_flare_temp <- function(file) {

  out <- lapply(file, function(f) {
    if(!file.exists(f)) {
      stop("File ", f, " does not exist.")
    }

    nc <- ncdf4::nc_open(f)
    on.exit({
      ncdf4::nc_close(nc)
    })
    t <- ncdf4::ncvar_get(nc,'time')
    local_tzone <- ncdf4::ncatt_get(nc, 0)$local_time_zone_of_simulation
    full_time <- as.POSIXct(t,
                            origin = '1970-01-01 00:00.00 UTC',
                            tz = "UTC")
    full_time_day <- lubridate::as_date(full_time)
    nsteps <- length(full_time_day)
    forecast <- ncdf4::ncvar_get(nc, 'forecast')
    if(any(forecast == 1, na.rm = TRUE)) {
      depths <- round(ncdf4::ncvar_get(nc, 'depth'),2)

      temp <- ncdf4::ncvar_get(nc, "temp")

      idx <- which(forecast == 1 & !is.na(forecast))

      return(list(forecast_date = as.character((full_time_day[idx])[1]),
                  dates = full_time_day[idx],
                  depths = depths,
                  temp = temp[idx, , ]))
    } else {
      return(list(forecast_date = "No forecast"))
    }
  })
  names(out) <- as.character(sapply(out, "[", 1))
  return(out)
}
