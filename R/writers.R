#' Write meta data.
#'
#' Writes meta data frame to meta file.
#'
#' @param meta Meta data frame.
#'
#' @param meta_file Meta CSV file.
write_meta <- function(meta, meta_file) {
    suppressWarnings(
        write.table(meta, file = meta_file, sep = ",", dec = ".", 
                    row.names = FALSE, col.names = !file.exists(meta_file) |
                        length(meta$location_hash) == 1)
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