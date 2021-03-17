#' Fix common HRM error of inflated heart rate in early part of activity.
#' 
fix_hr <- function(hr_base, hr_ceiling, max_minutes = NULL,
                   infile = NULL,
                   outfile = NULL,
                   return_output = FALSE) {

  lines <- readLines(infile)
  
  # Get start and end times
  time_idx <- which(grepl("time", lines))
  start_time <- lubridate::as_datetime(trimws(lines[[ time_idx[[1]] ]]), 
                                       format = "<time>%Y-%m-%dT%H:%M:%S+00:00</time>")
  end_time <- lubridate::as_datetime(trimws(lines[[ time_idx[[length(time_idx)]] ]]), 
                                     format = "<time>%Y-%m-%dT%H:%M:%S+00:00</time>")
  if(is.null(max_seconds)) max_seconds <- end_time - start_time + 1

  # Loop through each line, setting HR to hr_base if it exceeds hr_max, and 
  # stopping once max_seconds has passed
  this_time <- start_time
  i <- 1
  while( (this_time - start_time < lubridate::minutes(max_minutes)) & (i <= length(lines))) {
    if(grepl("<time>", lines[[i]])) {     #set the time of this observation
      this_time <- lubridate::as_datetime(trimws(lines[[i]]), 
                                          format = "<time>%Y-%m-%dT%H:%M:%S+00:00</time>")
    } else if(grepl("<gpxtpx:hr>", lines[[i]])) {
      hr <- stringr::str_extract(lines[[i]], "\\d+")
      if(as.numeric(hr) > hr_ceiling) {
        lines[[i]] <- gsub("\\d+", hr_base, lines[[i]])
      }
    }
    i <- i + 1
  }
  
  if(!is.null(outfile)) { 
    writeLines(lines, outfile) 
  }
  
  if(return_output) {
    res <- lines
  } else res <- NULL
  res
}