#' Create a demo dataset
#'
#' This function creates a demo dataset.
#'
#' @export
#'
#' @importFrom scales rescale
#' @importFrom simstudy genCorData
#'
#' @param n A numberic (integer) element representing the number of participants (rows) in the dataset. This parameter is set to 1000 by default.
#'
#' @return Returns a data frame.
get_dataset <- function(n = 1000) {
  # Build covariance matrix
  cov_mat <- data.frame(
    age = c(
      1,
      get_correlation_coefficient("w", "p", 1),
      get_correlation_coefficient("w", "p", 2),
      get_correlation_coefficient("m", "n", 4),
      get_correlation_coefficient("m", "n", 7),
      get_correlation_coefficient("s", "p", 22),
      get_correlation_coefficient("w", "p", 27),
      get_correlation_coefficient("w", "n", 33),
      get_correlation_coefficient("w", "p", 40),
      get_correlation_coefficient("s", "p", 48),
      get_correlation_coefficient("w", "n", 57),
      get_correlation_coefficient("w", "p", 67)
    ),
    gender = c(
      get_correlation_coefficient("w", "p", 1),
      1,
      get_correlation_coefficient("w", "p", 3),
      get_correlation_coefficient("s", "p", 5),
      get_correlation_coefficient("m", "p", 8),
      get_correlation_coefficient("s", "p", 23),
      get_correlation_coefficient("s", "p", 28),
      get_correlation_coefficient("w", "p", 34),
      get_correlation_coefficient("s", "p", 41),
      get_correlation_coefficient("s", "p", 49),
      get_correlation_coefficient("m", "p", 58),
      get_correlation_coefficient("m", "p", 68)
    ),
    ethnicity = c(
      get_correlation_coefficient("w", "p", 2),
      get_correlation_coefficient("w", "p", 3),
      1,
      get_correlation_coefficient("w", "p", 6),
      get_correlation_coefficient("w", "p", 9),
      get_correlation_coefficient("w", "p", 24),
      get_correlation_coefficient("w", "p", 29),
      get_correlation_coefficient("w", "p", 35),
      get_correlation_coefficient("s", "p", 42),
      get_correlation_coefficient("w", "p", 50),
      get_correlation_coefficient("w", "p", 59),
      get_correlation_coefficient("w", "p", 69)
    ),
    vo2max = c(
      get_correlation_coefficient("m", "n", 4),
      get_correlation_coefficient("s", "p", 5),
      get_correlation_coefficient("w", "p", 6),
      1,
      get_correlation_coefficient("s", "p", 10),
      get_correlation_coefficient("s", "n", 25),
      get_correlation_coefficient("s", "p", 30),
      get_correlation_coefficient("w", "p", 36),
      get_correlation_coefficient("l", "p", 43),
      get_correlation_coefficient("s", "n", 51),
      get_correlation_coefficient("w", "p", 60),
      get_correlation_coefficient("l", "p", 70)
    ),
    mvpa = c(
      get_correlation_coefficient("m", "n", 7),
      get_correlation_coefficient("m", "p", 8),
      get_correlation_coefficient("w", "p", 9),
      get_correlation_coefficient("s", "p", 10),
      1,
      get_correlation_coefficient("s", "n", 26),
      get_correlation_coefficient("s", "p", 31),
      get_correlation_coefficient("s", "p", 37),
      get_correlation_coefficient("s", "p", 44),
      get_correlation_coefficient("s", "n", 52),
      get_correlation_coefficient("m", "p", 61),
      get_correlation_coefficient("l", "p", 71)
    ),
    sprint = c(
      get_correlation_coefficient("s", "p", 22),
      get_correlation_coefficient("s", "p", 23),
      get_correlation_coefficient("w", "p", 24),
      get_correlation_coefficient("s", "n", 25),
      get_correlation_coefficient("s", "n", 26),
      1,
      get_correlation_coefficient("s", "p", 32),
      get_correlation_coefficient("w", "p", 38),
      get_correlation_coefficient("m", "n", 45),
      get_correlation_coefficient("m", "p", 53),
      get_correlation_coefficient("s", "p", 62),
      get_correlation_coefficient("m", "p", 72)
    ),
    confidence = c(
      get_correlation_coefficient("w", "p", 27),
      get_correlation_coefficient("s", "p", 28),
      get_correlation_coefficient("w", "p", 29),
      get_correlation_coefficient("s", "p", 30),
      get_correlation_coefficient("s", "p", 31),
      get_correlation_coefficient("s", "p", 32),
      1,
      get_correlation_coefficient("m", "p", 39),
      get_correlation_coefficient("s", "p", 46),
      get_correlation_coefficient("m", "n", 54),
      get_correlation_coefficient("w", "n", 63),
      get_correlation_coefficient("w", "p", 73)
    ),
    wellbeing = c(
      get_correlation_coefficient("w", "n", 33),
      get_correlation_coefficient("w", "p", 34),
      get_correlation_coefficient("w", "p", 35),
      get_correlation_coefficient("w", "p", 36),
      get_correlation_coefficient("s", "p", 37),
      get_correlation_coefficient("w", "p", 38),
      get_correlation_coefficient("m", "p", 39),
      1,
      get_correlation_coefficient("w", "p", 47),
      get_correlation_coefficient("s", "n", 55),
      get_correlation_coefficient("w", "p", 64),
      get_correlation_coefficient("s", "p", 74)
    ),
    caffeine = c(
      get_correlation_coefficient("w", "p", 40),
      get_correlation_coefficient("s", "p", 41),
      get_correlation_coefficient("s", "p", 42),
      get_correlation_coefficient("l", "p", 43),
      get_correlation_coefficient("s", "p", 44),
      get_correlation_coefficient("m", "n", 45),
      get_correlation_coefficient("s", "p", 46),
      get_correlation_coefficient("w", "p", 47),
      1,
      get_correlation_coefficient("s", "n", 56),
      get_correlation_coefficient("w", "p", 65),
      get_correlation_coefficient("m", "p", 75)
    ),
    bmi = c(
      get_correlation_coefficient("s", "p", 48),
      get_correlation_coefficient("s", "p", 49),
      get_correlation_coefficient("w", "p", 50),
      get_correlation_coefficient("s", "n", 51),
      get_correlation_coefficient("s", "n", 52),
      get_correlation_coefficient("m", "p", 53),
      get_correlation_coefficient("m", "n", 54),
      get_correlation_coefficient("s", "n", 55),
      get_correlation_coefficient("s", "n", 56),
      1,
      get_correlation_coefficient("w", "p", 66),
      get_correlation_coefficient("m", "n", 76)
    ),
    push_ups = c(
      get_correlation_coefficient("w", "n", 57),
      get_correlation_coefficient("m", "p", 58),
      get_correlation_coefficient("w", "p", 59),
      get_correlation_coefficient("w", "p", 60),
      get_correlation_coefficient("m", "p", 61),
      get_correlation_coefficient("s", "p", 62),
      get_correlation_coefficient("w", "n", 63),
      get_correlation_coefficient("w", "p", 64),
      get_correlation_coefficient("w", "p", 65),
      get_correlation_coefficient("w", "p", 66),
      1,
      get_correlation_coefficient("s", "p", 77)
    ),
    heart_rate_time1 = c(
      get_correlation_coefficient("w", "p", 67),
      get_correlation_coefficient("m", "p", 68),
      get_correlation_coefficient("w", "p", 69),
      get_correlation_coefficient("l", "p", 70),
      get_correlation_coefficient("l", "p", 71),
      get_correlation_coefficient("m", "p", 72),
      get_correlation_coefficient("w", "p", 73),
      get_correlation_coefficient("s", "p", 74),
      get_correlation_coefficient("m", "p", 75),
      get_correlation_coefficient("m", "n", 76),
      get_correlation_coefficient("s", "p", 77),
      1
    )
  )

  # Add row names that are identical to column names (nice to have when viewing the covariance matrix)
  row.names(cov_mat) <- cnames <- colnames(cov_mat)

  # Convert cov_mat data frame to a matrix
  cov_mat <- unname(as.matrix(cov_mat))

  # Make cov_mat positive definite if it is not
  if(isFALSE(is.positive.definite(cov_mat))) cov_mat <- make.positive.definite(cov_mat)

  # Generate correlated dataset
  n = 1000
  d <- genCorData(
    n,
    mu = c(25, 0.5, 1, 42, 4, 13, 4.5, 3.8, 0.1, 22, 25, 65),
    sigma = c(2, 0.1, 3, 3, 1, 2, 1.1, 1.1, 1, 1.5, 5, 10),
    corMatrix = cov_mat,
    cnames = cnames
  )

  # Data wrangling
  d$age <- round(rescale(d$age, to = c(17, 30)))
  d$gender <- round(d$gender)
  d$ethnicity <- round(rescale(d$ethnicity, to = c(1, 5)))
  d$mvpa <- round(rescale(d$mvpa, to = c(0, 7)))
  d$sprint <- rescale(d$sprint, to = c(10, 20))
  d$confidence <- rescale(d$confidence, to = c(1, 7))
  d$wellbeing <- rescale(d$wellbeing, to = c(1, 5))
  d$caffeine <- rescale(d$caffeine, to = c(0, 1))
  d$caffeine <- ifelse(d$caffeine < 0.6, 0, 1)

  # More data wrangling
  d$gender <- factor(d$gender, levels = 0:1, labels = c("Female", "Male"))
  d$ethnicity <- factor(d$ethnicity, levels = 1:5, labels = c("Asian", "Hispanic", "Caucasian", "African-American", "Multiracial"))
  d$caffeine <- factor(d$caffeine, levels = 0:1, labels = c("Non-ingester", "Ingester"))
  d <- as.data.frame(d)
  d <- d[order(colnames(d))]
  d <- d[, ! colnames(d) %in% "id"]
  return(d)
}
