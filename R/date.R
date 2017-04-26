#' Subtract date ranges.
#'
#' The set difference \code{[start_date, end_date] - [lower_date, upper_date]}
#' where the \code{[lower_date, upper_date]} range represents dates for which
#' SolarGIS data is known.
#'
#' @param start_date The start date for requested data.
#' 
#' @param start_date The end date for requested data.
#' 
#' @param lower_date The lower date for known SolarGIS data.
#' 
#' @param upper_date The uppper date for known SolarGIS data.
#'
#' @return A collection of date ranges that are not included in the range
#' \code{lower_date} and \code{upper_date}.
subtract_date_ranges <- function(start_date, end_date, lower_date, upper_date) {
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
    lower_date <- as.Date(lower_date)
    upper_date <- as.Date(upper_date)
    
    if (start_date > end_date) {
        stop("Start date is later than end date.")
    }
    
    if (lower_date > upper_date) {
        stop("Lower date is later than upper date.")
    }
    
    origin <- "1970-01-01"
    
    subtracted <- setdiff(start_date:end_date, lower_date:upper_date)

    # If no dates exist in the subtracted set exit early.
    if (length(subtracted) == 0) {
        return(list())
    }
    
    # Date range list will be populated with at most 2 date range collections.
    date_ranges <- list()
    date_ranges_i <- 1
    date_range <- c()
    
    # If the subtraction yields a single date exit early.
    if (length(subtracted) == 1) {
        return(list(as.Date(subtracted[[1]], origin = origin)))
    }
    
    subtracted_diff <- c(1, diff(subtracted))
    
    for (i in 1:length(subtracted_diff)) {
        date <- subtracted[i]
        
        if (subtracted_diff[i] != 1) {
            date_ranges[[date_ranges_i]] <- date_range
            date_ranges_i <- date_ranges_i + 1
            date_range <- c()
        }
        
        date_range <- c(date_range, date)
    }
    
    # If their is a second date range with contents add it to collection of date
    # ranges.
    if (length(date_range) > 0) {
        date_ranges[[date_ranges_i]] <- date_range
    }
    
    for (i in 1:length(date_ranges)) {
        date_ranges[[i]] <- as.Date(date_ranges[[i]], origin = origin)
    }
    
    return(date_ranges)
}


#' Generate date block indices.
#'
#' Due to SolgarGIS restricting the size of any one request to 31 days, we must
#' create a collection of indices such that a proper fetch range is created
#' from \code{result[i] + 1} to \code{result[i]}.
#'
#' @param total_number_of_days How many days are going to be requested.
#' 
#' @param days_per_request The number of days per request. Defaults to max of
#' 31.
#'
#' @return A collection of indices where \code{result[i] + 1} represents the
#' start date index and \code{result[i]} represents the end date index.
generate_date_block_indices <- function(total_number_of_days, 
                                        days_per_request = 31) {
    # Max number of days that can be fetched at once is 31. Thus
    # data must be fetched in 31 day blocks.
    expected_block_count <- ceiling(total_number_of_days / days_per_request)
    date_block_indices <- seq(0, total_number_of_days, by = days_per_request)
    
    if (length(date_block_indices) - 1 < expected_block_count) {
        date_block_indices <- c(date_block_indices, total_number_of_days)
    }
    
    return(date_block_indices)
}
