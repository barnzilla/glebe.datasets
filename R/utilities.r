#' Export a dataset.
#'
#' This function allows users to export a dataset to Excel or SPSS.
#'
#' @export
#'
#' @importFrom haven write_sav
#' @importFrom writexl write_xlsx
#'
#' @param x A data frame.
#' @param path The path to the location on the user's local computer where the data frame should be stored. This parameter is set to the working directory #' by default.
#' @param type A character element representing the type of export to perform (options: "xlsx" or "spss"). This parameter is set to "xlsx" by default.
#'
#'
#' @return Returns none.
export_dataset <- function(x, path = paste0(getwd(), "/", deparse(substitute(x))), type = "xlsx") {
  if(type == "spss") {
    if(! grepl(".sav", path)) {
      path <- paste0(path, ".sav")
    } 
    write_sav(x, path)
  } else {
    if(! grepl(".xlsx", path)) {
      path <- paste0(path, ".xlsx")
    } 
    write_xlsx(x, path)
  }
}

#' Randomly create a correlation coefficient.
#'
#' This function randomly creates a correlation coefficient.
#'
#' @export
#'
#' @param size A character element (options: "w" for weak, "s" for small, "m" for moderate or "l" for large).
#' @param direction A character element (options: "p" for positive or "n" for negative) This paramater is set to "p" by default.
#' @param seed_number A numeric (integer) element that allows a randomly generated effect size to be reproduced. This parameter is set to 1 by default.
#'
#'
#' @return Returns a numeric element between -0.70 and 0.70.
get_correlation_coefficient <- function(size, direction = "p", seed_number = 1) {
  set.seed(seed_number)
  if(size == "w") {
    output <- sample(seq(from = 0, to = 0.1, by = 0.0001), 1, replace = TRUE)
  } else if(size == "s") {
    output <- sample(seq(from = 0.11, to = 0.29, by = 0.0001), 1, replace = TRUE)
  } else if(size == "m") {
    output <- sample(seq(from = 0.3, to = 0.49, by = 0.0001), 1, replace = TRUE)
  }  else if(size == "l") {
    output <- sample(seq(from = 0.5, to = 0.7, by = 0.0001), 1, replace = TRUE)
  } else {
    output <- NA
  }
  if(is.na(output)) {
    return(NA)
  } else if(direction == "n") {
    return(output * -1)
  } else {
    return(output)
  }
}
