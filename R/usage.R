#' Get the number of successful SolarGIS API calls.
#'
#' @param api_key A SolarGIS API key.
#' 
#' @return The number of successful calls.
#' 
#' @export
usage_calls <- function(api_key) {
    res <- httr::GET(usage_datadelivery_url("count", api_key))
    return(as.numeric(httr::content(res, "text")))
}


#' Get the number of remaining SolarGIS API calls.
#'
#' @param api_key A SolarGIS API key.
#' 
#' @return The number of calls remaining or NA.
#' 
#' @export
usage_calls_remaining <- function(api_key) {
    res <- httr::GET(usage_datadelivery_url("limit/count", api_key))
    content <- httr::content(res, "text")
    return(ifelse(content == "N/A", NA, as.numeric(content)))
}


#' Get the number of used SolarGIS units.
#' 
#' A single unit is one day of data.
#'
#' @param api_key A SolarGIS API key.
#' 
#' @param start_date Query date range start. Format: yyyy-mm-dd.
#' 
#' @param end_date Query date range end. Format: yyyy-mm-dd.
#' 
#' @return The number of used data units.
#' 
#' @export
usage_units <- function(api_key, start_date, end_date) {
    url <- usage_datadelivery_url("units", api_key)
    
    if (!missing(start_date) && !missing(end_date)) {
        paste0(url, "&from=", start_date, "&to=", end_date)   
    }
    
    return(httr::content(httr::GET(url), "text"))
}


#' Get the number of remaining SolarGIS units.
#' 
#' A single unit is one day of data.
#'
#' @param api_key A SolarGIS API key.
#' 
#' @param start_date Query date range start. Format: yyyy-mm-dd.
#' 
#' @param end_date Query date range end. Format: yyyy-mm-dd.
#' 
#' @return The number of remaining units.
#' 
#' @export
usage_units_remaining <- function(api_key) {
    res <- httr::GET(usage_datadelivery_url("limit/units", api_key))
    content <- httr::content(res, "text")
    return(ifelse(content == "N/A", NA, as.numeric(content)))
}