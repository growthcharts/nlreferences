#' Convert Z-scores into measurements
#'
#' Converts age- or hgt-conditional standard deviation scores into measurements
#' (KG, CM, ...) using one or more external reference distributions.
#'
#' @note The type of reference distribution is defined by the `distribution` field
#' of the `study` attribute in the reference. The function executes
#' transformations specified by the `tx` field (before calculation of `y`) and
#' the `yt` field (after calculating of `y`), so that the `y` have the original
#' units even if the reference is based on some transform of `y`.
#'
#' Functions \code{z()} and \code{y()} functions are the inverse of each other.
#'
#' @param z A numerical vector with Z-scores.
#' @param x A vector containing `length(z)` values of the numerical covariate (typically
#' decimal age or height) at which conversion is desired.
#' @param refcode A character vector with `length(z)` elements, each of which
#' points to a reference.
#' @param dec A scalar value indicating the number of decimals used to round the
#' value. The default is 3 decimals.
#' @param tail_adjust Logical. If \code{TRUE} the procedure applies the WHO
#' method for tail adjustment under the LMS distribution. Not implemented
#' in `z()`.
#' @param \dots Not used.
#' @return A vector with `length(z)` elements containing the measurements.
#' @author Stef van Buuren, 2021
#' @examples
#' z <- c(0, -1.662, 0.928, -1.662, -1.268, -1.697, -1.268, -1.697)
#' x <- rep(c(0.3, 60), 4)
#' refcode <- c(
#'   "nl_2012_hdc_male_20", "nl_2012_wfh_male_",
#'   "nl_2012_hdc_male_30", "nl_2012_wfh_male_",
#'   "nl_1997_hdc_male_nl", "nl_1997_wfh_male_nla",
#'   "nl_1997_hdc_male_nl", "nl_1997_wfh_male_nla"
#' )
#' y <- y(z, x, refcode)
#' @export
y <- function(z, x, refcode, dec = 3L, tail_adjust = FALSE, ...) {
  if (length(z) != length(x) || length(z) != length(refcode)) {
    message("y(): Non-conformable arguments")
    return(rep(NA_real_, length(z)))
  }
  if (!length(z)) {
    return(numeric(0))
  }

  data.frame(z = z, x = x, refcode = refcode) %>%
    group_by(.data$refcode) %>%
    mutate(y = y_grp(
      z = .data$z,
      x = .data$x,
      refcode = first(.data$refcode),
      tail_adjust = tail_adjust
    )) %>%
    ungroup() %>%
    pull(y) %>%
    round(digits = dec)
}

y_grp <- function(z, x, refcode, tail_adjust = FALSE) {
  r <- load_reference(refcode, "all")

  # do not process in absence of study attribute
  study <- attr(r, "study")
  if (is.null(study)) {
    return(rep(NA_real_, length(z)))
  }

  # handle transforms, if any
  yt <- function(y, study) {
    if ("yt" %in% names(study)) {
      return(eval(parse(text = study[["yt"]])))
    }
    y
  }
  if ("tx" %in% names(study)) {
    x <- eval(parse(text = study[["tx"]]))
  }

  dist <- toupper(study[["distribution"]])

  if (dist == "NO") {
    check_names(df = r, needed = c("x", "mean", "sd"))
    mean <- approx(x = r[["x"]], y = r[["mean"]], xout = x)$y
    sd <- approx(x = r[["x"]], y = r[["sd"]], xout = x)$y
    return(yt(mean + z * sd, study))
  }
  if (dist == "LMS") {
    check_names(df = r, needed = c("x", "L", "M", "S"))
    L <- approx(x = r[["x"]], y = r[["L"]], xout = x)$y
    M <- approx(x = r[["x"]], y = r[["M"]], xout = x)$y
    S <- approx(x = r[["x"]], y = r[["S"]], xout = x)$y
    y <- ifelse(L > 0.01 | L < (-0.01), M * (1 + L * S * z)^(1 / L), M * exp(S * z))
    if (tail_adjust) y <- adjust_tail_y(y, z, L, M, S)
    return(yt(y, study))
  }
  if (dist == "BCCG") {
    check_names(df = r, needed = c("x", "nu", "mu", "sigma"))
    nu <- approx(x = r[["x"]], y = r[["nu"]], xout = x)$y
    mu <- approx(x = r[["x"]], y = r[["mu"]], xout = x)$y
    sigma <- approx(x = r[["x"]], y = r[["sigma"]], xout = x)$y
    return(yt(qBCCG(pnorm(z), mu = mu, sigma = sigma, nu = nu), study))
  }
  if (dist == "BCPE") {
    check_names(df = r, needed = c("x", "nu", "mu", "sigma", "tau"))
    mu <- approx(x = r[["x"]], y = r[["mu"]], xout = x)$y
    sigma <- approx(x = r[["x"]], y = r[["sigma"]], xout = x)$y
    nu <- approx(x = r[["x"]], y = r[["nu"]], xout = x)$y
    tau <- approx(x = r[["x"]], y = r[["tau"]], xout = x)$y
    return(yt(qBCPE(pnorm(z), mu = mu, sigma = sigma, nu = nu, tau = tau), study))
  }
  if (dist == "BCT") {
    check_names(df = r, needed = c("x", "nu", "mu", "sigma", "tau"))
    mu <- approx(x = r[["x"]], y = r[["mu"]], xout = x)$y
    sigma <- approx(x = r[["x"]], y = r[["sigma"]], xout = x)$y
    nu <- approx(x = r[["x"]], y = r[["nu"]], xout = x)$y
    tau <- approx(x = r[["x"]], y = r[["tau"]], xout = x)$y
    return(yt(qBCT(pnorm(z), mu = mu, sigma = sigma, nu = nu, tau = tau), study))
  }

  stop(paste("Reference type", dist, "not implemented."))
}


adjust_tail_y <- function(y, z, L, M, S) {
  stop("Tail adjustmunt for y() not implemented.")
}
