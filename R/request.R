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
#' @param year The year of data to be requested.
#'
#' @author author The author of the request. May be an individual's name or the
#' project name requesting data.
#'
#' @param min_dist The minimum distance in meters from (lat, lon) that is
#' acceptable for use. Defaults to 10000 meters which is about 6.2 miles.
#'
#' @return A complete file path for the SolarGIS data requested.
#'
#' @export
request <- function(solargis_dir, lat, lon, year, author, min_dist = 10000) {
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
            year = year,
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
            year = year,
            file_hash = digest::sha1(paste(lat, lon, year, sep = "-"))
        ), file = meta_file, sep = ",", dec = ".", append = TRUE,
        row.names = FALSE, col.names = !file.exists(meta_file))
    )

    # TODO: Request data, write file & return path.

    return(NULL)
}
