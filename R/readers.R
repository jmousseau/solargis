#' Read a SolarGIS CSV file.
#'
#' @param file The path to a SolarGIS CSV file.
#' 
#' @param sorted Should the returned data frame be sorted. default value is
#' \code{TRUE}.
#'
#' @return A data frame with correctly formatted row names.
#'
#' @export
read_csv <- function(file, sorted = TRUE) {
    data <- data.table::fread(file)
    data$timestamp <- as_POSIXct(data$timestamp)
    
    for (col_name in setdiff(colnames(data), c("timestamp"))) {
        data[, col_name] <- sapply(data[, col_name], as.numeric)
    }
    
    if (sorted) {
        data <- data[order(data$timestamp), ]
    }
    
    return(data)
}


#' Convert to POSIXct type.
#' 
#' @param timestamps Value to convert to POSIXct type. Must be in the format
#' %Y-%m-%d %H:%M:%S.
#' 
#' @return \code{timestamps} as a POSIXct object.
as_POSIXct <- function(timestamps) {
    if(class(timestamps) != "character") {
        timestamps <- as.character(timestamps)
    }
    
    return(as.POSIXct(timestamps, "UTC", "%Y-%m-%d %H:%M:%S"))
}