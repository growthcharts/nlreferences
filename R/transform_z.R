#' Transforms measurement to Z-scores using Dutch references
#'
#' This function transforms growth data to Z-scores using two
#' Dutch references: a term reference and a preterm-reference for
#' children born `ga <= 36` aged 0 - 4 years.
#' For head circumference, preterm references up to
#' 1.5 years are used, and term references thereafter.
#'
#' By default, the functions scans for growth variables, and converts
#' any variables it finds.
#'
#' @param data Data frame with appropriate variables, at least
#' `sex` and `age`. The names of the measurements can
#' be one or more of: `hgt`, `wgt`, `hdc`, `wfh`, `bmi` and `dsc`.
#' If the variable `ga` is entirely missing, the function assumes term
#' births and prints a message.
#' @param ynames Character vector containing the measurements to convert.
#' Specify this to limit the number of conversions. If not specified, the
#' functions tries all transformations
#' @param verbose Set to `TRUE` to turn on warnings and messages
#' @return
#' A data frame with either zero rows or the same number of rows
#' as `nrow(data)` with colums named `hgt_z`, `wgt_z`, and so on.
#' @examples
#' df <- data.frame(hgt = 60, wgt = 5, hdc = 40, age = 0.3,
#' sex = "male", ga = c(20, 30, 40, 50))
#' transform_z(df, ynames = c("hdc", "wfh"))
#' @export
transform_z <- function(data,
                        ynames = c("hgt", "wgt", "hdc", "wfh", "bmi", "dsc"),
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
           ga = ifelse(!is.na(.data$ga) & .data$ga < 25 & .data$ga >= 21, 25, .data$ga),
           pt = !is.na(.data$ga) & .data$ga <= 36 & !is.na(.data$age) & .data$age < 4,
           xhgt = .data$hgt) %>%
    select(.data$row, .data$age, .data$xhgt, .data$sex, .data$ga, .data$pt, all_of(yn)) %>%
    pivot_longer(cols = all_of(yn)) %>%
    mutate(x = ifelse(.data$name == "wfh", .data$xhgt, .data$age),
           year = ifelse(.data$pt, "2012", ""),
           year = ifelse(!.data$pt & .data$name %in% c("hgt", "wgt", "hdc", "wfh", "bmi"),
                         "1997", .data$year),
           year = ifelse(!is.na(.data$age) & .data$age > 1.5 & .data$name == "hdc",
                         "1997", .data$year),
           year = ifelse(.data$pt & .data$name == "bmi", "1997", .data$year),
           year = ifelse(.data$name == "dsc", "2014", .data$year),
           sub = "",
           sub = ifelse(.data$pt & .data$name %in% c("hgt", "wgt", "hdc", "dsc"),
                        .data$ga, .data$sub),
           sub = ifelse(!is.na(.data$age) & .data$age > 1.5 & .data$name == "hdc",
                        "nl", .data$sub),
           sub = ifelse(!.data$pt & .data$name %in% c("hgt", "wgt", "hdc"),
                        "nl", .data$sub),
           sub = ifelse(.data$name == "bmi",
                        "nl", .data$sub),
           sub = ifelse(!.data$pt & .data$name == "dsc", "40", .data$sub),
           sub = ifelse(!.data$pt & .data$name == "wfh", "nla", .data$sub),
           refcode = make_refcode(
             name = "nl",
             year = .data$year,
             yname = .data$name,
             sex = .data$sex,
             sub = .data$sub),
           z = z(y = .data$value,
                 x = .data$x,
                 refcode = .data$refcode,
                 pkg = "jamesyzy",
                 verbose = verbose))

  # fold back Z-scores into wide
  long %>%
    select(.data$row, .data$name, .data$z) %>%
    pivot_wider(id_cols = .data$row, names_from = .data$name, values_from = .data$z) %>%
    select(-.data$row) %>%
    rename_with(paste0, names(.), "_z")
}

