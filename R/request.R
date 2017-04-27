#' Request SolarGIS data.
#'
#' The parameters of this function may be used to perform requests for multiple
#' sights for multiple years.
#'
#' @param solargis_dir The root directory where solar-gis data is stored.
#' 
#' @param site_id A site identifier. Does not be unique. Unique identifier is
#' created with \code{site_id} and \code{location_hash} in the meta data table.
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
request <- function(solargis_dir, site_id, lat, lon, start_date, end_date,
                    author, api_key, min_dist = 1000) {
    if (!dir.exists(solargis_dir)) {
        stop(paste("The solargis directory", solargis_dir, "does not exist."))
    }
    
    if (start_date > end_date) {
        stop(paste("Start date occurs after end date."))
    }
    
    if (!is.numeric(lat) || !is.numeric(lon)) {
        stop("Latitude and longitude must be of numeric type.")
    }
    
    max_date_diff_in_days <- 3 * 365
    date_diff <- as.Date(end_date) - as.Date(start_date)
    units(date_diff) <- "days"
    
    if (as.numeric(date_diff) > max_date_diff_in_days) {
        stop(paste0("Number of days requested, ", date_diff, 
                    ", exceeds max per request (", max_date_diff_in_days, ")."))
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
            site_id = site_id,
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
    
    solargis_data_dir <- paste0(solargis_dir, "/data")
    dir.create(solargis_data_dir, showWarnings = FALSE)
    
    site_data_file <- paste0(location_hash, ".csv")
    site_data_path <- paste(solargis_data_dir, site_data_file, sep = "/")
    expected_days_written <- days_between_inclusive(start_date, end_date)

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
            
            if (length(date_diffs) > 0) {
                n_days <-length(unlist(date_diffs))
                message(paste("Requesting", n_days, "more day(s) of data."))
                
                # Lat / lon are changed to those found in meta data table so
                # that data is true to location.
                lat <- closest$lat
                lon <- closest$lon
                
                index_of_closest <- which.min(dist_to_prev_requests)
                previous_start_date <- meta$start_date[index_of_closest]
                
                # Only give real dates once data has been successfully fetched.
                meta$start_date[index_of_closest] <- NA
                meta$end_date[index_of_closest] <- NA
                
                write_meta(meta, meta_file)
                
                # Submit a request for each date range difference.
                for (date_range in date_diffs) {
                    date_block_indices <- generate_date_block_indices(
                        length(date_range)
                    )
                    
                    for (i in 2:length(date_block_indices)) {
                        start_date_index <- date_block_indices[i - 1] + 1
                        end_date_index <- date_block_indices[i]
                        
                        req_start_date <- date_range[start_date_index]
                        req_end_date <- date_range[end_date_index]
                        
                        # Will throw fatal error if request fails.
                        res <- request_remote(lat, lon, req_start_date, 
                                              req_end_date, api_key)
                        
                        # Because more than one request can be sent, set the end
                        # date to the most recent successful request end date.
                        meta$start_date[index_of_closest] <- start_date
                        meta$end_date[index_of_closest] <- ifelse(
                            as.character(req_end_date) < previous_start_date,
                            end_date,
                            as.character(req_end_date)
                        )
                        
                        write_meta(meta, meta_file, FALSE)
                        write_site_data(res, site_data_path)
                        
                        # TODO: PUT intro CRADLE. On confirmation of ingestion,
                        # remove `site_data_path` file.
                        
                    }
                }
                
                # When updating the location, the meta data is the only place
                # where total number of days is located.
                expected_days_written <- days_between_inclusive(
                    meta$start_date[index_of_closest],
                    meta$end_date[index_of_closest]
                ) 
                written_site_date <- read_csv(site_data_path, sorted = FALSE)
                days_written <- length(unique(as.Date(
                    written_site_date$timestamp
                ))) 
                
                if (days_written == expected_days_written) {
                    message("Correct number of days contained in file.")
                } else {
                    message("Incorrect number of days contained in file.")
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
    write_meta(data.frame(
        site_id = site_id,
        lat = lat,
        lon = lon,
        start_date = start_date,
        end_date = end_date,
        author = author,
        location_hash = location_hash
    ), meta_file, file.exists(meta_file))
    
    date_range <- as.Date(as.Date(start_date):as.Date(end_date),
                          origin = "1970-01-01")
    
    date_block_indices <- generate_date_block_indices(
        length(date_range)
    )
    
    for (i in 2:length(date_block_indices)) {
        start_date_index <- date_block_indices[i - 1] + 1
        end_date_index <- date_block_indices[i]
        
        start_date <- date_range[start_date_index]
        end_date <- date_range[end_date_index]

        res <- request_remote(lat, lon, start_date, end_date, api_key)
        
        write_site_data(res, site_data_path)
    }
    
    written_site_date <- read_csv(site_data_path, sorted = FALSE)
    days_written <- length(unique(as.Date(written_site_date$timestamp))) 
    
    if (days_written == expected_days_written) {
        message("Correct number of days contained in file.")
    } else {
        message("Incorrect number of days contained in file.")
    }

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
                <processing key='_!KEY!_' summarization='MIN_15'/>
            </ws:dataDeliveryRequest>"
    
    body <- gsub("_!DFR!_", start_date, body, fixed = TRUE)
    body <- gsub("_!DTO!_", end_date, body, fixed = TRUE)
    body <- gsub("_!LAT!_", lat, body, fixed = TRUE)
    body <- gsub("_!LON!_", lon, body, fixed = TRUE)
    body <- gsub("_!SID!_", site_id, body, fixed = TRUE)
    body <- gsub("_!KEY!_", paste(c(
        "GHI", "DNI", "DIF", "GTI", "KTM", "SE", "SA", "TEMP", "AP", "RH", "WS",
        "WD", "PWAT", "KT"
    ), collapse = " "), body, fixed = TRUE)
    
    res <- httr::POST(url, body = body, httr::content_type_xml(),
                      httr::accept_json())
    json <- jsonlite::fromJSON(httr::content(res, "text"))
    
    # If the message field is present, an error has occurred.
    if (!is.null(json$message)) {
        stop(paste0("SolarGIS Error: ", json$message))
    }
    
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
    
    data$lat <- lat
    data$lon <- lon
    
    return(data)
}
