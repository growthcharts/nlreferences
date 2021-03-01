#' Transforms measurements to Z-scores using Dutch references
#'
#' This function transforms growth data in the wide matrix
#' to Z-scores using the selector method implemented in
#' [set_refcodes()]. It is the inverse of [transform2y()].
#'
#' By default, the function scans for variables named
#' `hgt`, `wgt`, `hdc`, `wfh`, `bmi` and `dsc`, and returns
#' Z-scores for any variables it finds.
#'
#' @param data Data frame with appropriate variables, at least
#' `sex` and `age`. The names of the measurements can
#' be one or more of: `hgt`, `wgt`, `hdc`, `wfh`, `bmi` and `dsc`.
#' @param ynames Character vector containing the measurements to convert.
#' Specify this to limit the number of conversions. If not specified, the
#' function calculates Z-scores for all measurements.
#' @param pkg Name of the package that stores the growth references. By
#' default, `transform2z` searches the `nlreferences` package.
#' @param verbose Set to `TRUE` to turn on warnings and messages, which is
#' useful for tracking down problem related to missing data or to the
#' availability of references.
#' @return
#' A data frame with either zero rows or the same number of rows
#' as `nrow(data)` with colums named `hgt_z`, `wgt_z`, and so on.
#' @author Stef van Buuren 2021
#' @seealso [set_refcodes()], [centile::y2z()]
#' @examples
#' df <- data.frame(hgt = 60, wgt = 5, hdc = 40, age = 0.3,
#' sex = "male", ga = c(20, 30, 40, 50))
#' transform2z(df, ynames = c("hdc", "wfh"))
#' @export
transform2z <- function(data,
                        ynames = c("hgt", "wgt", "hdc", "wfh", "bmi", "dsc"),
                        pkg = "nlreferences",
                        verbose = FALSE) {
  if (!is.data.frame(data))
    stop("Argument `data` should be a data frame.")
  if ("wfh" %in% ynames) data$wfh <- data$wgt
  vars <- colnames(data)
  if (!"age" %in% vars && any(c("hgt", "wgt", "hdc", "bmi", "dsc") %in% ynames))
    stop("Required variable `age` not found.")
  if (!"hgt" %in% vars && "wfh" %in% ynames)
    stop("Required variable `hgt` not found.")
  if (!"sex" %in% vars)
    stop("Required variable `sex` not found.")
  if (!"ga" %in% vars) {
    message("Variable `ga` not found. Assuming term births only.")
    data$ga <- 40
  }

  # active ynames
  yn <- ynames[ynames %in% vars]

  # calculate Z-scores for all ynames using long form
  long <- data %>%
    mutate(row = row_number(),
           xhgt = .data$hgt) %>%
    select(.data$row, .data$age, .data$xhgt, .data$sex, .data$ga, all_of(yn)) %>%
    pivot_longer(cols = all_of(yn), names_to = "yname", values_to = "y") %>%
    mutate(x = ifelse(.data$yname == "wfh", .data$xhgt, .data$age),
           xname = ifelse(.data$yname == "wfh", "hgt", "age")) %>%
    mutate(refcode = set_refcodes(data = .),
           z = y2z(y = .data$y,
                   x = .data$x,
                   refcode = .data$refcode,
                   pkg = pkg,
                   verbose = verbose))

  # fold back Z-scores into wide
  long %>%
    select(.data$row, .data$yname, .data$z) %>%
    pivot_wider(id_cols = .data$row, names_from = .data$yname, values_from = .data$z) %>%
    select(-.data$row) %>%
    rename_with(paste0, names(.), "_z")
}
