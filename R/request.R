#' Request SolarGIS data.
#'
#' The parameters of this function may be used to perform requests for multiple
#' sights for multiple years.
#'
#' @param solargis_dir The root directory where solar-gis data is stored.
#'
#' @param lat The location's latitude.
#'
#' @param lon The location's longitude.
#'
#' @param start_date The start date for the data to be requested.
#' 
#' @param end_date The end date for the data to be reqeusted.
#'
#' @author author The author of the request. May be an individual's name or the
#' project name requesting data.
#'
#' @param api_key A SolarGIS API key.
#'
#' @param min_dist The minimum distance in meters from (lat, lon) that is
#' acceptable for use. Defaults to 1000 meters which is about 0.62 miles.
#'
#' @return A complete file path for the SolarGIS data requested.
#'
#' @export
request <- function(solargis_dir, lat, lon, start_date, end_date, author, 
                    api_key, min_dist = 1000) {
    if (!dir.exists(solargis_dir)) {
        stop(paste("The solargis directory", solargis_dir, "does not exist."))
    }
    
    if (start_date > end_date) {
        stop(paste("Start date occurs after end date."))
    }
    
    country_name <- coord_to_country(lat, lon)
    
    if (!country_name$ISO3 %in% c("USA", "CAN")) {
        stop(paste0("The requested location, ", country_name$full, 
                    ", is outside USA and Canada."))
    }

    if (usage_units_remaining(api_key) %in% c(0, NA)) {
        stop(paste("Account has no remaining data units available."))
    }

    requests_file <- paste0(solargis_dir, "/requests.csv")
    meta_file <- paste0(solargis_dir, "/meta.csv")

    # Insert the request metadata into "requests.csv" for request history.
    # Warnings are suppressed for when no file exists and a csv header must be
    # created.
    suppressWarnings(
        write.table(data.frame(
            lat = lat,
            lon = lon,
            start_date = start_date,
            end_date = end_date,
            author = author,
            min_dist = min_dist
        ), file = requests_file, sep = ",", dec = ".", append = TRUE,
        row.names = FALSE, col.names = !file.exists(requests_file))
    )
    
    location_hash <- digest::sha1(paste0(lat, lon))

    # Find the closest existing request and see if the location is within the
    # `min_dist` of the current requested location.
    if (file.exists(meta_file)) {
        meta <- data.table::fread(meta_file)

        # Convert the two arrays of latitude and longitude to coordinate pairs.
        coords <- mapply(c, meta$lat, meta$lon, SIMPLIFY = FALSE)

        dist_to_prev_requests <- unlist(lapply(coords, function(coord) {
            return(distance_between(lat, lon, coord[1], coord[2]))
        }))

        # If dist_to_prev_requests is empty, this will cause the comparison
        # below to fail and thus request data from SolarGIS.
        dist_to_prev_requests <- c(dist_to_prev_requests, min_dist)
        
        if (min(dist_to_prev_requests) < min_dist) {
            message(paste("Found existing location within", min_dist, "meters."))
            
            closest <- meta[which.min(dist_to_prev_requests), ]
            
            # Check the range of dates here. If they are contained, return
            # path to file, otherwise update date range for meta and change
            # lat/lon to be lat/lon in meta table. Update date range in meta
            # table. Then create and submit new request(s).
            message("Checking date ranges...")
            date_diffs <- subtract_date_ranges(start_date, end_date, 
                                               closest$start_date, 
                                               closest$end_date)
            
            site_data_file <- paste0(meta$location_hash, ".csv")
            site_data_path <- paste(solargis_dir, "data", site_data_file, 
                                    sep = "/")
            
            if (length(date_diffs) > 0) {
                n_days <-length(unlist(date_diffs))
                message(paste("Requesting", n_days, "more day(s) of data."))
                
                # Lat / lon are changed to those found in meta data table so
                # that data is true to location.
                lat <- closest$lat
                lon <- closest$lon
                
                meta$start_date[which.min(dist_to_prev_requests)] <- start_date
                meta$end_date[which.min(dist_to_prev_requests)] <- end_date
                
                write.table(meta, file = meta_file, sep = ",", dec = ".",
                            row.names = FALSE,
                            col.names = !file.exists(meta_file) |
                                        length(meta$location_hash) == 1)
                
                # Submit a request for each date range difference.
                for (date_range in date_diffs) {
                    req_start_date <- date_range[1]
                    req_end_date <- date_range[length(date_range)]
                    res <- request_remote(lat, lon, req_start_date, 
                                          req_end_date, api_key)
                    write.table(res, file = site_data_path, sep = ",",
                                dec = ".", append = TRUE, row.names = FALSE,
                                col.names = !file.exists(site_data_path))
                }
                
                return(site_data_path)
                
            } else {
                return(site_data_path)
            }
        }
    }

    # This point is only reached if no meta file exists or no existing data file
    # meets the request parameters (year & min distance).
    message(paste("No existing location within", min_dist, "meters."))
    message("Fetching new data set from SolarGIS...")

    # Add the request to "meta.csv" because we are requesting a new dataset.
    # Warnings are suppressed for a non-existing "meta.csv".
    suppressWarnings(
        write.table(data.frame(
            lat = lat,
            lon = lon,
            start_date = start_date,
            end_date = end_date,
            author = author,
            location_hash = location_hash
        ), file = meta_file, sep = ",", dec = ".", append = TRUE,
        row.names = FALSE, col.names = !file.exists(meta_file))
    )
    
    solargis_data_dir <- paste0(solargis_dir, "/data")
    dir.create(solargis_data_dir, showWarnings = FALSE)
    
    site_data_file <- paste0(location_hash, ".csv")
    site_data_path <- paste(solargis_data_dir, site_data_file, sep = "/")

    res <- request_remote(lat, lon, start_date, end_date, api_key)
    write.csv(res, file = site_data_path, row.names = FALSE)

    return(site_data_path)
}


#' Request data from SolarGIS servers.
#'
#' Submit a single request for a single location to SolarGIS's REST API.
#' IMPORTANT: NO CHECKS ARE MADE WHEN THIS FUNCTION IS CALLED AGAINST PREVIOUS
#' REQUESTS SO THE TIME PERIOD WILL COUNT AGAINST OUR DATA UNITS.
#'
#' @param lat The location's latitude.
#'
#' @param lon The location's longitude.
#'
#' @param start_date The start date for the data to be requested.
#' 
#' @param end_date The end date for the data to be reqeusted.
#'
#' @param api_key A SolarGIS API key.
#' 
#' @return A complete file path for the SolarGIS data requested.
request_remote <- function(lat, lon, start_date, end_date, api_key) {
    url <- rest_datadelivery_url("request", api_key)

    site_hash <- digest::sha1(paste0(lat, lon, start_date, end_date))
    site_id <- paste0("site-", site_hash)

    body <- "<ws:dataDeliveryRequest dateFrom='_!DFR!_' dateTo='_!DTO!_'
                xmlns='http://geomodel.eu/schema/data/request'
                xmlns:ws='http://geomodel.eu/schema/ws/data'>
                <site id='_!SID!_' lat='_!LAT!_' lng='_!LON!_'/>
                <processing key='GHI DIF GTI DNI TEMP RH AP WS WD SE SA' 
                 summarization='MIN_15'/>
            </ws:dataDeliveryRequest>"
    
    body <- gsub("_!DFR!_", start_date, body, fixed = TRUE)
    body <- gsub("_!DTO!_", end_date, body, fixed = TRUE)
    body <- gsub("_!LAT!_", lat, body, fixed = TRUE)
    body <- gsub("_!LON!_", lon, body, fixed = TRUE)
    body <- gsub("_!SID!_", site_id, body, fixed = TRUE)
    
    res <- httr::POST(url, body = body, httr::content_type_xml(),
                      httr::accept_json())
    json <- jsonlite::fromJSON(httr::content(res, "text"))
    cols <- unlist(json$sites$columns)
    rows <- as.data.frame(json$sites$rows)
    
    data <- data_frame(c("timestamp", cols), length(rows$dateTime))
    
    # Timestamps from SolarGIS are in milliseconds.
    data$timestamp <- as.POSIXct(rows$dateTime / 1000, "UTC", 
                                 origin = "1970-01-01")
    
    for (i in 1:length(cols)) {
        col_name <- cols[i]
        data[, col_name] <- sapply(rows$values, function(val_list) {
            return(unlist(val_list)[i])
        })
    }
    
    return(data)
}
