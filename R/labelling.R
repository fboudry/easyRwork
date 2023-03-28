#' Encode a column
#'
#' @param my_col
#'
#' @return
#' @export
#'
#' @examples
col_encode <- function(my_col) {
  convert_dic <- lst()
  if (is.numeric(x = my_col)) {
    my_col
  } else {
    label <- LabelEncoder.fit(y = my_col)
    convert_dic <<- append(x = convert_dic, values = label)
    transform(enc = label, my_col)
  }
}

#' Encode a data frame
#'
#' @param input
#' @param list_names
#'
#' @return
#' @export
#'
#' @examples
df_encode <- function(input = my_data, list_names) {
  # Encode (labeling) entire data frames
  encoded_data <- lapply(X = input,
                         FUN = col_encode) %>% as.data.frame()
  output <- lst(convert_dic, encoded_data)
  if (!missing(x = list_names)) {
    names(output) <- list_names
  }
  return(output)
}
