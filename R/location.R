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


#' Get the ISO3 country code for a coordinate.
#'
#' @param lat Latitude of coordinate.
#'
#' @param lon Longitude of coordinate.
#' 
#' @return A list containing \code{$ISO3} code and \code{$full} name.
coord_to_country <- function(lat, lon) {
    points <- data.frame(lon = c(lon), lat = c(lat))
    
    countries_sp <- rworldmap::getMap(resolution = "low")
    proj_4_string <- sp::CRS(sp::proj4string(countries_sp))
    
    points_sp <- sp::SpatialPoints(points, proj4string = proj_4_string)
    
    indices <- sp::over(points_sp, countries_sp)
    
    return(list(
        ISO3 = as.character(indices$ISO3),
        full = as.character(indices$ADMIN)
    ))
}


#' Get the fulll state for a coordinate.
#'
#' Return value will be NA if coordinate is located outside mainland United
#' States. Thus Hawaii and Alaska coordinates will return NA.
#'
#' @param lat Latitude of coordinate.
#'
#' @param lon Longitude of coordinate.
#' 
#' @return Lowercase state name or \code{NA}.
coordinate_to_state <- function(lat, lon) {
    coordinates <- data.frame(lon = c(lon), lat = c(lat))

    states <- maps::map("state", fill = TRUE, plot = FALSE)
    ids <- sapply(strsplit(states$names, ":"), function(x) x[1])
    proj4string <- sp::CRS("+proj=longlat +datum=WGS84")
    states_sp <- maptools::map2SpatialPolygons(states, IDs = ids,
                                               proj4string = proj4string)
    
    points_sp <- sp::SpatialPoints(coordinates, proj4string = proj4string)
    indices <- sp::over(points_sp, states_sp)
    
    stateNames <- sapply(states_sp@polygons, function(x) x@ID)
    
    return(stateNames[indices])
}