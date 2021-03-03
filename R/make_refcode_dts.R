#' Creates the refcode from dot arguments
#'
#' @param xname x
#' @param yname x
#' @param x x
#' @param sex x
#' @param age x
#' @param ga x
#' @param \dots not used
#' @return Character vector with reference codes
#' @export
make_refcode_dts <- function(xname = "age", yname, x, sex = "",
                             age = NULL, ga = NULL, ...) {
  if (yname == "wfh") xname <- "hgt"
  df <- data.frame(xname = xname,
                   yname = yname,
                   x = x,
                   sex = sex,
                   age = age,
                   ga = ga)
  as.character(nlreferences::set_refcodes(df))
}
