#' Write meta data.
#'
#' Writes meta data frame to meta file.
#'
#' @param meta Meta data frame.
#'
#' @param meta_file Meta CSV file.
#' 
#' @param should_append Should the entry be appended to existing file.
write_meta <- function(meta, meta_file, should_append = TRUE) {
    if (file.exists(meta_file)) {
        existing_entries <- length(read.csv(meta_file)$location_hash)
    } else {
        existing_entries <- 0
    }
    
    suppressWarnings(
        write.table(meta, file = meta_file, sep = ",", dec = ".",
                    append = should_append, 
                    row.names = FALSE, col.names = !file.exists(meta_file) |
                        (existing_entries == 1 & !should_append))
    )
}


#' Write site data.
#'
#' Appends data to site data file or creates a new file.
#'
#' @param data Site data frame.
#'
#' @param site_data_file Site data file.
write_site_data <- function(data, site_data_file) {
    suppressWarnings(
        write.table(data, file = site_data_file, sep = ",", dec = ".",
                    append = TRUE, row.names = FALSE,
                    col.names = !file.exists(site_data_file))
    )
}