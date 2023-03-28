#' Create needed folders
#'
#' @param folder_list
#' @param folder_root
#'
#' @return
#' @export
#'
#' @examples
init_folder <- function(folder_list, folder_root = NULL) {
  if (is.null(folder_root)) {
    folder_root <- "./"
  }
  stopifnot(
    "Error in root folder format (you probably forgot the \"/\" at the end of your path)" =
      str_ends(string = folder_root, pattern = "/")
  )
  for (my_folder in folder_list) {
    if (!dir.exists(paste0(folder_root, my_folder))) {
      dir.create(paste0(folder_root, my_folder))
    }
  }
}
