#' Create a data frame.
#'
#' @param col_names The column names for the data frame.
#' 
#' @param n_rows The number of rows to be in the data frame.
#' 
#' @return A data frame.
data_frame <- function(col_names, n_rows) {
    n_col <- length(col_names)
    data <- as.data.frame(matrix(nrow = n_rows, ncol = n_col))
    colnames(data) <- col_names
    return(data)
}

#' Compose a SolarGIS REST Data Delivery URL.
#'
#' @param route The route name. Will be prefixed with
#' \code{https://solargis.info/ws/rest/datadelivery/}.
#' 
#' @param api_key A SolarGIS API key.
#' 
#' @return A SolarGIS url as a string.
rest_datadelivery_url <- function(route, api_key) {
    base_url <- "https://solargis.info/ws/rest/datadelivery/"
    url <- paste0(base_url, route, "?key=", api_key)
    return(url)
}

#' Compose a SolarGIS Usage Data Delivery URL.
#'
#' @param route The route name. Will be prefixed with
#' \code{https://solargis.info/ws/usage/datadelivery/}.
#' 
#' @param api_key A SolarGIS API key.
#' 
#' @return A SolarGIS url as a string.
usage_datadelivery_url <- function(route, api_key) {
    base_url <- "https://solargis.info/ws/usage/datadelivery/"
    url <- paste0(base_url, route, "?key=", api_key)
    return(url)
}
