#' Calculate coordinate distance.
#'
#' @param lat1 Latitude of first coordinate.
#'
#' @param lon1 Longitude of the first coordinate.
#'
#' @param lat2 Latitude of second coordinate.
#'
#' @param lon2 Longitude of the second coordinate.
#'
#' @return The distance between the coordinates in meters
distance_between <- function(lat1, lon1, lat2, lon2) {
    to_radians <- function(degrees) {
        return(pi / 180 * degrees)
    }

    dlat <- to_radians(lat2 - lat1)
    dlon <- to_radians(lon2 - lon1)

    a <- to_radians(lat1) # lat1
    b <- to_radians(lat2) # lat2

    x <- sin(dlat / 2) * sin(dlat / 2) +
        sin(dlon / 2) * sin(dlon / 2) * cos(a) * cos(b)
    y <- 2 * atan2(sqrt(x), sqrt(1 - x))

    earth_radius_miles <- 3958.756

    dist_in_mi <- round(earth_radius_miles * y, digits = 4)
    dist_in_m <- dist_in_mi * 1609.34

    return(dist_in_m)
}