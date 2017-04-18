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
#' @param min_dist The minimum distance in meters from (lat, lon) that is
#' acceptable for use. Defaults to 1000 meters which is about 0.62 miles.
#'
#' @return A complete file path for the SolarGIS data requested.
#'
#' @export
request <- function(solargis_dir, lat, lon, start_date, end_date, author, min_dist = 1000) {
    if (!dir.exists(solargis_dir)) {
        stop(paste("The solargis directory", solargis_dir, "does not exist."))
    }

    # TODO: Check the latitude value to see if it is bounded correctly.

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

    # Find the closest existing request and see if the location is within the
    # `min_dist` of the current requested location.
    if (file.exists(meta_file)) {
        meta <- data.table::fread(meta_file)
        meta <- meta[which(meta$year == year), ]

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
            
            # TODO: Check the range of dates here. If they are contained, return
            # path to file, otherwise update date range for meta and change
            # lat/lon to be lat/lon in meta table. Update date range in meta
            # table. Then create and submit new request(s).
            
            closest <- meta[which.min(dist_to_prev_requests), ]
            file <- paste0(meta$file_hash, ".csv")
            path <- paste(solargis_dir, "data", meta$year, file, sep = "/")

            return(path)
        }
    }

    # This point is only reached if no meta file exists or no existing data file
    # meets the request parameters (year & min distance).
    message(paste("No existing location within", min_dist, "meters."))
    message("Fetching new data set from SolarGIS...")

    # Add the request to "meta.csv" because we are requesting a new dataset.
    # Warnings are suppressed for a non-existing "meta.csv".
    suppressWarnings(
        write.csv(data.frame(
            lat = lat,
            lon = lon,
            start_date = start_date,
            end_date = end_date,
            author = author,
            file_hash = digest::sha1(paste0(lat, lon, start_date, end_date))
        ), file = meta_file, sep = ",", dec = ".", append = TRUE,
        row.names = FALSE, col.names = !file.exists(meta_file))
    )

    # TODO: Request data, write file & return path.

    return(NULL)
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
#' @param api_key The API key for solar gis.
#' 
#' @return A complete file path for the SolarGIS data requested.
request_remote <- function(lat, lon, start_date, end_date, api_key) {
    base_url <- "https://solargis.info/ws/rest/datadelivery"
    url <- paste0(base_url, "/request?key=", api_key)
    
    site_hash <- digest::sha1(paste0(lat, lon, start_date, end_date))
    site_id <- paste0("site-", site_hash)

    body <- "<ws:dataDeliveryRequest dateFrom='_!DFR!_' dateTo='_!DTO!_'
                xmlns='http://geomodel.eu/schema/data/request'
                xmlns:ws='http://geomodel.eu/schema/ws/data'>
                <site id='_!SID!_' lat='_!LAT!_' lng='_!LON!_'/>
                <processing key='GHI DIF DNI' summarization='MIN_15'/>
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
