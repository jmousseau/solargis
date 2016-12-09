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

    requests_file <- paste(solargis_dir, "/requests.csv")
    meta_file <- paste(solargis_dir, "/meta.csv")

    # Insert the request metadata into "requests.csv" for request history.
    write.csv(data.frame(
        lat = lat,
        lon = lon,
        year = year,
        author = author,
        min_dist = min_dist
    ), file = requests_file, append = TRUE)
}
