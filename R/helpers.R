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