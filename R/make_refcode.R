#' Make reference code from components
#'
#' This function creates reference codes by pasting `name`, `year`, `yname`,
#' `sex` and `sub` fields. The result is a string that identifies the
#' reference.
#' @param name Character vector identifying the reference name field
#' @param year Character vector identifying the reference year field
#' @param yname Character vector identifying the reference yname field
#' @param sex Character vector identifying the reference sex field
#' @param sub Character vector identifying the reference sub field
#' @return A character vector with `n` elements, where `n` equals the longest
#' vector in the input arguments.
#' @note The function silently converts any zero-length arguments to length `1`.
#' The procedure aborts with an error if the length of any input is unequal to
#' either `1` or `n`.
#' @examples
#' data <- data.frame(len = c(56, 42, 53),
#'                    age = c(0.1, 0.2, 0.15),
#'                    sex = c("male", "female", "female"),
#'                    ga = c(40, 27, 39))
#' make_refcode("nl", "1997", "hgt", data$sex, data$ga)
#' @export
make_refcode <- function(name = "",
                         year = "",
                         yname = "",
                         sex = "",
                         sub = "")
  {
  # replace any zero-length argument
  if (!length(name)) name <- ""
  if (!length(year)) year <- ""
  if (!length(yname)) yname <- ""
  if (!length(sex)) sex <- ""
  if (!length(sub)) sub <- ""

  # find longest element
  len <- c(length(name), length(year), length(yname), length(sex), length(sub))
  i <- which.max(len)
  if (!all(len %in% c(1L, len[i])))
    stop("Non-conformable lengths of arguments.")

  # code NA as ""
  name <- as.character(name)
  year <- as.character(year)
  yname <- as.character(yname)
  sex <- as.character(sex)
  sub <- as.character(sub)

  name[is.na(name)] <- ""
  year[is.na(year)] <- ""
  yname[is.na(yname)] <- ""
  sex[is.na(sex)] <- ""
  sub[is.na(sub)] <- ""

  # do it
  paste(name, year, yname, sex, sub, sep = "_")
}

