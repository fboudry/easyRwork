#' Compute relative values
#'
#' @param input An R vector.
#'
#' @return Returns a vector with relative values based on maximal values from
#'   the given vector.
#' @export
#'
#' @examples

compute_relative <- function(input) {
  maximum_columns <-
    grep(pattern = "max", x = colnames(x = input)) # List max columns
  for (my_column in maximum_columns) {
    my_variable_name <- colnames(x = input[my_column]) %>%
      sub(pattern = "_max", replacement = "") # remove "_max" from column name
    max_colname <- paste0(my_variable_name, "_max")
    mean_colname <- paste0(my_variable_name, "_mean")
    min_colname <- paste0(my_variable_name, "_min")
    input[min_colname] <- # Compute minimum values as %
      100 * input[min_colname] / input[max_colname]
    input[mean_colname] <- # Compute mean values as %
      100 * input[mean_colname] / input[max_colname]
  }
  return(input)
}
