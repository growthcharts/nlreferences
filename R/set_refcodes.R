#' Calculates a name for a growth references for each measurement
#'
#' Determines the reference name for each measurement based on age, sex, outcome
#' and gestational age.
#'
#' @details
#' The present implementation uses three Dutch references: a term reference,
#' a pre-term reference for children born `ga <= 36` aged 0 - 4 years, and
#' a reference for the D-score. For head circumference, pre-term references up to
#' 1.5 years are used, and term references thereafter. Together, this
#' specification defines the **analysis metric** in JAMES.
#'
#' The pre-term references govern gestational ages between 25 and 36 weeks.
#' The function scores pre-term born infants with gestational ages between
#' 21 and 24 weeks relative to the 25-week references.
#'
#' If the variable `ga` is not present in `data`, the function assumes
#' term births for all children and prints a message.
#'
#' References for the Development Score (D-score) for pre-term were created
#' by shifting the term reference by 40 - ga weeks to the right, so the
#' references start at a later calender age. Because pre-terms are older
#' their Z-score initially score higher than those of terms. The effect
#' disappears after four months.
#'
#' The `yname` field currently supports the following measurements:
#' `hgt` (length/height in cm), `wgt` (body weight in kg),
#' `hdc` (head circumference in cm), `bmi` (body mass index in kg/m**2),
#' `wfh` (weight for height in kg) and `dsc` (D-score).
#'
#' The `xname` field currently supports the following measurements:
#' `age` (child age, decimal years) and `hgt` (height in cm, only for `wfh`).
#'
#' @param data Data frame in the long form, where each measurement has its
#' own row. Required variables are `xname`, `yname`, `x` and `sex`.
#' Optional variables are `age` and `ga`.
#' @return
#' A character vector of `nrow(data)` elements.
#' @author Stef van Buuren 2021
#' @seealso [centile::make_refcode()], [centile::load_reference()]
#' @references [D-score adjustment for pre-terms](https://d-score.org/dbook1/sec-pops.html#conclusions)
#' @examples
#' df <- data.frame(
#'   xname = c("age", "age", "age", "age", "hgt", "age"),
#'   yname = c("hgt", "hgt", "hdc", "hdc", "wfh", "dsc"),
#'   x = c(0.1, 0.1, 0.1, 1.6, 60, 1.0),
#'   sex = rep(c("male", "female"), 3),
#'   ga = c(39, 27, 27, 27, 39, 40))
#' refcodes <- set_refcodes(df)
#'
#' # show the preterm 27 weeks hgt reference
#' centile::load_reference(refcodes[2], pkg = "nlreferences")
#' @export
set_refcodes <- function(data) {
  if (!is.data.frame(data))
    stop("Argument `data` should be a data frame.")
  req <- c("xname", "yname", "x", "sex")
  if (!all(hasName(data, req))) {
    stop("Not found: ", paste(req[!hasName(data, req)], collapse = ", ", "."))
  }
  if (!hasName(data, "age")) {
    data$age <- ifelse(data$xname == "age", data$x, NA_real_)
  }
  if (!hasName(data, "ga")) {
    message("Variable `ga` not found. Assuming term births only.")
    data$ga <- 40
  }

  data %>%
    select(all_of(c(req, "age", "ga"))) %>%
    mutate(
      ga = ifelse(!is.na(.data$ga) & .data$ga < 25 & .data$ga >= 21, 25, .data$ga),
      pt = !is.na(.data$ga) & .data$ga <= 36 & !is.na(.data$age) &
        ((.data$age <= 4 & .data$yname != "hdc") | (.data$age <= 1.5 & .data$yname == "hdc")),
      year = ifelse(.data$pt, "2012", "1997"),
      year = ifelse(.data$yname == "dsc", "2014", .data$year),
      year = ifelse(.data$pt & .data$yname == "bmi", "1997", .data$year),
      sub = "",
      sub = ifelse(.data$pt & .data$yname %in% c("hgt", "wgt", "hdc", "dsc"),
                   .data$ga, .data$sub),
      sub = ifelse(!.data$pt & .data$yname %in% c("hgt", "wgt", "hdc"),
                   "nl", .data$sub),
      sub = ifelse(.data$yname == "bmi", "nl", .data$sub),
      sub = ifelse(!.data$pt & .data$yname == "dsc", "40", .data$sub),
      sub = ifelse(!.data$pt & .data$yname == "wfh", "nla", .data$sub),
      sub = ifelse(.data$sub == "nla" & !is.na(.data$age) & .data$age > 16, "nlb", .data$sub),
      refcode = make_refcode(
        name = "nl",
        year = .data$year,
        yname = .data$yname,
        sex = .data$sex,
        sub = .data$sub)) %>%
    pull(.data$refcode)
}
