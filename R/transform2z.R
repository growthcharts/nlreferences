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
#' @param \dots Arguments passed down to [centile::y2z()], e.g. `rule = c(1, 2)`
#' to calculate Z-scores beyond the maximum age in the reference table.
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
                        verbose = FALSE,
                        ...) {
  if (!is.data.frame(data))
    stop("Argument `data` should be a data frame.")
  vars <- colnames(data)
  todo <- intersect(c(vars, "wfh"), ynames)
  if (!length(todo))
    stop("Expected one of `hgt`, `wgt`, `hdc`, `wfh`, `bmi`, `dsc`.")
  if (!"age" %in% vars && any(c("hgt", "wgt", "hdc", "bmi", "dsc") %in% todo))
    stop("Required variable `age` not found.")
  if (!"hgt" %in% vars && "wfh" %in% todo)
    stop("Required variable `hgt` not found.")
  if (!"wgt" %in% vars && "wfh" %in% todo)
    stop("Required variable `wgt` not found.")
  if ("wfh" %in% todo) data$wfh <- data$wgt
  if (!"sex" %in% vars)
    stop("Required variable `sex` not found.")
  if (!"ga" %in% vars) {
    message("Variable `ga` not found. Assuming term births only.")
    data$ga <- 40
  }

  # replacement for xheight
  xhgt <- rep(NA_real_, nrow(data))
  if ("hgt" %in% vars) xhgt <- data$hgt

  # calculate Z-scores for all ynames using long form
  long <- data %>%
    mutate(row = row_number(),
           xhgt = !! xhgt) %>%
    select(all_of(c("row", "age", "xhgt", "sex", "ga", todo))) %>%
    pivot_longer(cols = all_of(todo), names_to = "yname", values_to = "y") %>%
    mutate(x = ifelse(.data$yname == "wfh", .data$xhgt, .data$age),
           xname = ifelse(.data$yname == "wfh", "hgt", "age")) %>%
    mutate(refcode = set_refcodes(data = .),
           z = y2z(y = .data$y,
                   x = .data$x,
                   refcode = .data$refcode,
                   pkg = pkg,
                   verbose = verbose,
                   ...))

  # fold back Z-scores into wide
  long %>%
    select(all_of(c("row", "yname", "z"))) %>%
    pivot_wider(id_cols = "row", names_from = "yname", values_from = "z") %>%
    select(-"row") %>%
    rename_with(paste0, names(.), "_z")
}
