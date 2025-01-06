
# FUNCTIONS

.tabulate_gpx3d <- function(trkseg_list) {
  
  trkseg_attrs <- attributes(trkseg_list)
  
  data.frame(
    #time = strptime(trkseg_list[["time"]][[1]], "%Y-%m-%dT%H:%M:%SZ"),
    ele  = as.numeric(trkseg_list[["ele"]][[1]]), 
    lon  = as.numeric(trkseg_attrs[["lon"]]),
    lat  = as.numeric(trkseg_attrs[["lat"]])
  ) %>% 
    mutate(
      ele = ele
    )
  
}


.validate_extract_gpx3d <- function(gpx_file, sf_out) {
  
  if (!grepl(".gpx$", gpx_file)) {
    stop("'gpx_file' must be a path to a file with extension '.gpx'")
  }
  
  if (!file.exists(gpx_file)) {
    stop("'gpx_file' must be a filepath to an existing .gpx file")
  }
  
  if (!is.logical(sf_out)) {
    stop("'sf_out' must be TRUE or FALSE")
  }
  
}

extract_gpx3d <- function(gpx_file, sf_out = TRUE) {
  #browser()
  
  .validate_extract_gpx3d(gpx_file, sf_out)
  
  gpx_in <- xml2::read_xml(gpx_file)
  gpx_list <- xml2::as_list(gpx_in)
  
  route_list <- lapply(
    gpx_list$gpx$trk$trkseg,
    function(x) .tabulate_gpx3d(x)
  )
  
  route_df <- do.call(rbind, route_list)
  rownames(route_df) <- NULL
  
  route_df <- sf::st_as_sf(
    route_df,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )
  
  # https://stackoverflow.com/questions/49853696/distances-of-points-between-rows-with-sf
  route_df$lead <- c(
    route_df$geometry[1],
    route_df$geometry[as.numeric(rownames(route_df)) - 1]
  )
  
  route_df$distance <- c(
    sf::st_distance(route_df$geometry, route_df$lead, by_element = TRUE)
  )
  
  route <- route_df[, c( "ele", "lon", "lat", "geometry", "distance")]
  
  if (!sf_out) {
    route_df <- as.data.frame(route_df)
    route <- route_df[, c("ele", "lon", "lat", "distance")]
  }
  
  return(route)
  
}



strava_segments_call <- function(auth_url, segment_efforts_url, payload, params) {
  cat("Requesting Token...\n")
  res <- POST(auth_url, body = payload, encode = "form")
  
  if (status_code(res) == 200) {
    access_token <- content(res)$access_token
    cat(sprintf("Access Token = %s...\n", access_token))
  } else {
    stop(sprintf("Error requesting token: %d - %s", status_code(res), content(res, "text")))
  }
  
  header <- add_headers(Authorization = paste("Bearer", access_token))
  
  response <- GET(segment_efforts_url, header, query = params)
  
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    return(data)
  } else {
    stop(sprintf("Error fetching data: %d - %s", status_code(response), content(response, "text")))
  }
}

library(dplyr)
library(tidyr)

process_workouts <- function(data, workout_days) {
  #browser()
  workouts <- as.data.frame(data)
  
  workouts$date <- as.Date(workouts$start_date)
  workouts <- workouts %>% filter(date %in% workout_days)
  
  workouts <- workouts %>%
    mutate(
      distance_km = distance / 1000,          
      elapsed_hr = elapsed_time / 3600,      
      speed = distance_km / elapsed_hr
    ) %>%
    group_by(date) %>%
    mutate(order = dense_rank(start_date)) %>%
    ungroup()
  
  workouts <- workouts %>%
    mutate(
      speed_min_per_km = (elapsed_time / 60) / distance_km, # Speed in min/km
      min_per_km = floor(speed_min_per_km),                # Extract minutes
      sec_per_km = round((speed_min_per_km - min_per_km) * 60) # Extract seconds
    ) %>%
    group_by(date) %>%
    mutate(
      avg_speed = mean(speed_min_per_km),
      avg_min_per_km = floor(avg_speed),
      avg_sec_per_km = round((avg_speed - avg_min_per_km) * 60)
    ) %>%
    select(date, speed, min_per_km, sec_per_km, order, avg_speed, avg_min_per_km, avg_sec_per_km) %>%
    pivot_wider(
      id_cols = date,
      names_from = order,
      values_from = c(speed, min_per_km, sec_per_km, avg_speed, avg_min_per_km, avg_sec_per_km),
      names_sep = "_"
    ) %>%
    mutate(
      dif_speed = speed_1 - speed_2,
      dif_min_per_km = floor(abs(dif_speed)),
      dif_sec_per_km = round(abs((sec_per_km_1 - sec_per_km_2)))
    ) %>%
    mutate(
      speed_min_per_km_1 = paste0(min_per_km_1, ":", sprintf("%02d", sec_per_km_1)), # Format as MM:SS
      speed_min_per_km_2 = paste0(min_per_km_2, ":", sprintf("%02d", sec_per_km_2)),  # Format as MM:SS
      speed_avg_per_km = paste0(avg_min_per_km_1, ":", sprintf("%02d", avg_sec_per_km_1)),
      speed_dif_per_km = paste0(dif_min_per_km, ":", sprintf("%02d", dif_sec_per_km)),
      speed_dif_per_km = ifelse(speed_1 > speed_2, speed_dif_per_km, paste0("-", speed_dif_per_km))
    ) %>%
    select(
      Date = date, 
      `First Rep` = speed_min_per_km_1, 
      `Second Rep` = speed_min_per_km_2,
      `Average` = speed_avg_per_km,
      `Difference` = speed_dif_per_km
    ) %>%
    arrange(Date)
  
  return(workouts)
}
