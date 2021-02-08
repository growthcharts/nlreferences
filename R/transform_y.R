#' Transforms Z-scores into measurements using Dutch references
#'
#' This function transforms Z-scores into measurements using two
#' Dutch references: a term reference and a preterm-reference for
#' children born `ga <= 36` aged 0 - 4 years.
#' For head circumference, preterm references up to
#' 1.5 years are used, and term references thereafter.
#'
#' By default, the functions scans for Z-scores, and converts
#' any variables it finds.
#'
#' The conversion of `wfh_z` to `wfh` is special. As `wfh` is conditional
#' on `hgt`, the calculation requires either `hgt` or `hgt_z` to be present
#' in the data. Availability of `hgt` takes precedence over `hgt_z`. If
#' only `hgt_z` is known, then the function calculates `hgt` from `hgt_z`.
#'
#' The current function incorrectly assumes that the WFH-reference for children
#' older than 16 years equals that of children younger than 16 years. This
#' detail still needs to be fixed.
#'
#' @param data Data frame with appropriate variables, at least
#' `sex` and `age`. The names of the measurements can
#' be one or more of: `hgt_z`, `wgt_z`, `hdc_z`, `wfh_z`, `bmi_z` and `dsc_z`.
#' If the variable `ga` is entirely missing, the function assumes term
#' births and prints a message.
#' @param ynames Character vector containing the measurements to convert.
#' Specify this to limit the number of conversions. If not specified, the
#' functions tries all transformations
#' @param verbose Set to `TRUE` to turn on warnings and messages
#' @return
#' A data frame with either zero rows or the same number of rows
#' as `nrow(data)` with colums named `hgt`, `wgt`, and so on.
#' @examples
#' df <- data.frame(hgt_z = 0, wgt_z = 1, hdc_z = -1, age = 0.3,
#' sex = "male", ga = c(20, 30, 40, 50))
#' transform_y(df, ynames = c("hdc_z", "wfh_z"))
#' @export
transform_y <- function(data,
                        ynames = c("hgt_z", "wgt_z", "hdc_z", "wfh_z", "bmi_z", "dsc_z"),
                        verbose = FALSE) {
  if (!is.data.frame(data))
    stop("Argument `data` should be a data frame.")
  vars <- colnames(data)
  if (!"age" %in% vars && any(c("hgt_z", "wgt_z", "hdc_z", "bmi_z", "dsc_z") %in% ynames))
    stop("Required variable `age` not found.")
  if ("wfh_z" %in% ynames && !any(c("hgt", "hgt_z") %in% vars))
    stop("Required variable `hgt` or `hgt_z` not found.")
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
           ga = ifelse(!is.na(.data$ga) & .data$ga < 25 & .data$ga >= 21, 25, .data$ga),
           pt = !is.na(.data$ga) & .data$ga <= 36 & !is.na(.data$age) & .data$age < 4,
           xhgt = ifelse(rep("hgt" %in% vars, nrow(data)),
                         .data$hgt,
                         y(z = .data$hgt_z,
                           x = .data$age,
                           refcode = make_refcode(name = "nl",
                                                  year = ifelse(.data$pt, "2012", "1997"),
                                                  yname = "hgt",
                                                  sex = .data$sex,
                                                  sub = ifelse(.data$pt, .data$ga, "nl")),
                           pkg = "jamesreferences",
                           verbose = verbose))) %>%
    select(.data$row, .data$age, .data$xhgt, .data$sex, .data$ga, .data$pt, all_of(yn)) %>%
    pivot_longer(cols = all_of(yn)) %>%
    mutate(x = ifelse(.data$name == "wfh_z", .data$xhgt, .data$age),
           year = ifelse(.data$pt, "2012", ""),
           year = ifelse(!.data$pt & .data$name %in% c("hgt_z", "wgt_z", "hdc_z", "wfh_z", "bmi_z"),
                         "1997", .data$year),
           year = ifelse(!is.na(.data$age) & .data$age > 1.5 & .data$name == "hdc_z",
                         "1997", .data$year),
           year = ifelse(.data$pt & .data$name == "bmi_z", "1997", .data$year),
           year = ifelse(.data$name == "dsc_z", "2014", .data$year),
           sub = "",
           sub = ifelse(.data$pt & .data$name %in% c("hgt_z", "wgt_z", "hdc_z", "dsc_z"),
                        .data$ga, .data$sub),
           sub = ifelse(!is.na(.data$age) & .data$age > 1.5 & .data$name == "hdc",
                        "nl", .data$sub),
           sub = ifelse(!.data$pt & .data$name %in% c("hgt_z", "wgt_z", "hdc_z"),
                        "nl", .data$sub),
           sub = ifelse(.data$name == "bmi_z",
                        "nl", .data$sub),
           sub = ifelse(!.data$pt & .data$name == "dsc_z", "40", .data$sub),
           sub = ifelse(!.data$pt & .data$name == "wfh_z", "nla", .data$sub),
           refcode = make_refcode(
             name = "nl",
             year = .data$year,
             yname = strtrim(.data$name, nchar(.data$name) - 2L),
             sex = .data$sex,
             sub = .data$sub),
           y = y(z = .data$value,
                 x = .data$x,
                 refcode = .data$refcode,
                 pkg = "jamesreferences",
                 verbose = verbose))

  # fold back Z-scores into wide, strip "_z" in names
  long %>%
    select(.data$row, .data$name, .data$y) %>%
    pivot_wider(id_cols = .data$row, names_from = .data$name, values_from = .data$y) %>%
    select(-.data$row) %>%
    rename_with(strtrim, names(.), nchar(names(.)) - 2L)
}

