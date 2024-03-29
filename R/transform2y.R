#' Transforms Z-scores into measurements using Dutch references
#'
#' #' This function transforms Z-scores stored in the wide matrix
#' to measurements using the selector method implemented in
#' [set_refcodes()]. It is the inverse of [transform2z()].
#'
#' By default, the function scans for variables named
#' `hgt_z`, `wgt_z`, `hdc_z`, `wfh_z`, `bmi_z` and `dsc_z`, and returns
#' the measurement values for any variables it finds.
#'
#' The conversion of `wfh_z` to `wfh` is special. As `wfh` is conditional
#' on `hgt`, the calculation requires either `hgt` or `hgt_z` to be present
#' in the data. Availability of `hgt` takes precedence over `hgt_z`. If
#' only `hgt_z` is known, then the function calculates `hgt` from `hgt_z`.
#'
#' @inheritParams transform2z
#' @param data Data frame with appropriate variables, at least
#' `sex` and `age`. The names of the measurements can
#' be one or more of: `hgt_z`, `wgt_z`, `hdc_z`, `wfh_z`, `bmi_z` and `dsc_z`.
#' @param znames Character vector containing the names of the Z-scores
#' to convert. Specify this to limit the number of conversions. If not specified, the
#' function calculates measurements for all Z-scores.
#' @param \dots Arguments passed down to [centile::z2y()], e.g. `rule = c(1, 2)`
#' to calculate Z-scores beyond the maximum age in the reference table.
#' @return
#' A data frame with either zero rows or the same number of rows
#' as `nrow(data)` with colums named `hgt`, `wgt`, and so on.
#' @author Stef van Buuren 2021
#' @examples
#' df <- data.frame(hgt_z = 0, wgt_z = 1, hdc_z = -1, age = 0.3,
#' sex = "male", ga = c(20, 30, 40, 50))
#' transform2y(df, znames = c("hdc_z", "wfh_z"))
#' @export
transform2y <- function(data,
                        znames = c("hgt_z", "wgt_z", "hdc_z", "wfh_z", "bmi_z", "dsc_z"),
                        pkg = "nlreferences",
                        verbose = FALSE,
                        ...) {
  if (!is.data.frame(data))
    stop("Argument `data` should be a data frame.")
  vars <- colnames(data)
  todo <- intersect(vars, znames)
  if (!length(todo))
    stop("Expected one of `hgt_z`, `wgt_z`, `hdc_z`, `wfh_z`, `bmi_z`, `dsc_z`.")
  if (!"age" %in% vars && any(c("hgt_z", "wgt_z", "hdc_z", "bmi_z", "dsc_z") %in% todo))
    stop("Required variable `age` not found.")
  if ("wfh_z" %in% todo && !any(c("hgt", "hgt_z") %in% vars))
    stop("Required variable `hgt` or `hgt_z` not found.")
  if (!"sex" %in% vars)
    stop("Required variable `sex` not found.")
  if (!"ga" %in% vars) {
    message("Variable `ga` not found. Assuming term births only.")
    data$ga <- 40
  }

  # calculate measurement from Z-scores using long form
  long <- data %>%
    mutate(row = row_number(),
           ga = ifelse(!is.na(.data$ga) & .data$ga < 25 & .data$ga >= 21, 25, .data$ga),
           pt = !is.na(.data$ga) & .data$ga <= 36 & !is.na(.data$age) & .data$age < 4)

  # replacement for xheight
  long$xhgt <- rep(NA_real_, nrow(long))
  if ("hgt" %in% vars) long$xhgt <- long$hgt
  else if ("hgt_z" %in% vars) long$xhgt <- z2y(z = long$hgt_z,
                                               x = long$age,
                                               refcode = make_refcode(name = "nl",
                                                                      year = ifelse(long$pt, "2012", "1997"),
                                                                      yname = "hgt",
                                                                      sex = long$sex,
                                                                      sub = ifelse(long$pt, long$ga, "nl")),
                                               pkg = pkg,
                                               verbose = verbose,
                                               ...)
  long <- long %>%
    select(all_of(c("row", "age", "xhgt", "sex", "ga", todo))) %>%
    pivot_longer(cols = all_of(todo), names_to = "zname", values_to = "z") %>%
    mutate(yname = strtrim(.data$zname, nchar(.data$zname) - 2L),
           x = ifelse(.data$yname == "wfh", .data$xhgt, .data$age),
           xname = ifelse(.data$yname == "wfh", "hgt", "age")) %>%
    mutate(refcode = set_refcodes(data = .),
           y = z2y(z = .data$z,
                   x = .data$x,
                   refcode = .data$refcode,
                   pkg = pkg,
                   verbose = verbose))

  # fold back data into wide
  long %>%
    select(all_of(c("row", "yname", "y"))) %>%
    pivot_wider(id_cols = "row", names_from = "yname", values_from = "y") %>%
    select(-"row")
}
