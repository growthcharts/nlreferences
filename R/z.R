#' Convert measurements into Z-scores
#'
#' Converts measurements into age- or hgt-conditional standard deviation scores
#' (SDS, Z-score) using one or more external reference distributions.
#'
#' @note The type of reference distribution is defined by the `distribution` field
#' of the `study` attribute in the reference. The function executes
#' transformations specified by the `tx` and `ty` fields before calculating the
#' Z-score.
#'
#' Functions \code{z()} and \code{y()} functions are the inverse of each other.
#'
#' @param y A numerical vector with measurements`.
#' @param x A vector containing `length(y)` values of the numerical covariate (typically
#' decimal age or height) at which conversion is desired.
#' @param refcode A character vector with `length(y)` elements, each of which
#' points to a reference.
#' @param dec A scalar value indicating the number of decimals used to round the
#' value. The default is 3 decimals.
#' @param tail_adjust Logical. If \code{TRUE} the procedure applies the WHO
#' method for tail adjustment under the LMS distribution.
#' The default is \code{FALSE}.
#' @param \dots Not used.
#' @return A vector with `length(y)` elements containing the Z-scores.
#' @author Stef van Buuren, 2021
#' @examples
#' y <- c(rep(c(40, 5), 4))
#' x <- rep(c(0.3, 60), 4)
#' refcode <- c(
#'   "nl_2012_hdc_male_20", "nl_2012_wfh_male_",
#'   "nl_2012_hdc_male_30", "nl_2012_wfh_male_",
#'   "nl_1997_hdc_male_nl", "nl_1997_wfh_male_nla",
#'   "nl_1997_hdc_male_nl", "nl_1997_wfh_male_nla"
#' )
#' z <- z(y, x, refcode)
#' @export
z <- function(y, x, refcode, dec = 3L, tail_adjust = FALSE, ...) {
  if (length(y) != length(x) || length(y) != length(refcode)) {
    message("z(): Non-conformable arguments")
    return(rep(NA_real_, length(y)))
  }
  if (!length(y)) {
    return(numeric(0))
  }

  data.frame(y = y, x = x, refcode = refcode) %>%
    group_by(.data$refcode) %>%
    mutate(z = z_grp(
      y = .data$y,
      x = .data$x,
      refcode = first(.data$refcode),
      tail_adjust = tail_adjust
    )) %>%
    ungroup() %>%
    pull(z) %>%
    round(digits = dec)
}

z_grp <- function(y, x, refcode, tail_adjust = FALSE) {
  r <- load_reference(refcode, "all")

  # do not process in absence of study attribute
  study <- attr(r, "study")
  if (is.null(study)) {
    return(rep(NA_real_, length(y)))
  }

  # handle transforms, if any
  if ("ty" %in% names(study)) {
    y <- eval(parse(text = study[["ty"]]))
  }
  if ("tx" %in% names(study)) {
    x <- eval(parse(text = study[["tx"]]))
  }

  dist <- toupper(study[["distribution"]])
  if (dist == "NO") {
    check_names(df = r, needed = c("x", "mean", "sd"))
    mean <- approx(x = r[["x"]], y = r[["mean"]], xout = x)$y
    sd <- approx(x = r[["x"]], y = r[["sd"]], xout = x)$y
    return((y - mean) / sd)
  }
  if (dist == "LMS") {
    check_names(df = r, needed = c("x", "L", "M", "S"))
    L <- approx(x = r[["x"]], y = r[["L"]], xout = x)$y
    M <- approx(x = r[["x"]], y = r[["M"]], xout = x)$y
    S <- approx(x = r[["x"]], y = r[["S"]], xout = x)$y
    z <- ifelse(L > 0.01 | L < (-0.01), (((y / M)^L) - 1) / (L * S), log(y / M) / S)
    if (tail_adjust) z <- adjust_tail_z(y, z, L, M, S)
    return(z)
  }
  if (dist == "BCCG") {
    check_names(df = r, needed = c("x", "nu", "mu", "sigma"))
    nu <- approx(x = r[["x"]], y = r[["nu"]], xout = x)$y
    mu <- approx(x = r[["x"]], y = r[["mu"]], xout = x)$y
    sigma <- approx(x = r[["x"]], y = r[["sigma"]], xout = x)$y
    return(qnorm(pBCCG(y, mu = mu, sigma = sigma, nu = nu)))
  }
  if (dist == "BCPE") {
    check_names(df = r, needed = c("x", "nu", "mu", "sigma", "tau"))
    mu <- approx(x = r[["x"]], y = r[["mu"]], xout = x)$y
    sigma <- approx(x = r[["x"]], y = r[["sigma"]], xout = x)$y
    nu <- approx(x = r[["x"]], y = r[["nu"]], xout = x)$y
    tau <- approx(x = r[["x"]], y = r[["tau"]], xout = x)$y
    return(qnorm(pBCPE(y, mu = mu, sigma = sigma, nu = nu, tau = tau)))
  }
  if (dist == "BCT") {
    check_names(df = r, needed = c("x", "nu", "mu", "sigma", "tau"))
    mu <- approx(x = r[["x"]], y = r[["mu"]], xout = x)$y
    sigma <- approx(x = r[["x"]], y = r[["sigma"]], xout = x)$y
    nu <- approx(x = r[["x"]], y = r[["nu"]], xout = x)$y
    tau <- approx(x = r[["x"]], y = r[["tau"]], xout = x)$y
    return(qnorm(pBCT(y, mu = mu, sigma = sigma, nu = nu, tau = tau)))
  }

  stop(paste("Reference type", dist, "not implemented."))
}

adjust_tail_z <- function(y, z, L, M, S) {
  idx <- !is.na(z) & z > 3
  if (any(idx)) {
    sd3 <- ifelse(L > 0.01 | L < (-0.01), M * (1 + L * S * 3)^(1 / L), M * exp(S * 3))
    sd2 <- ifelse(L > 0.01 | L < (-0.01), M * (1 + L * S * 2)^(1 / L), M * exp(S * 2))
    z[idx] <- (3 + (y - sd3) / (sd3 - sd2))[idx]
  }
  idx <- !is.na(z) & z < (-3)
  if (any(idx)) {
    sd3 <- ifelse(L > 0.01 | L < (-0.01), M * (1 + L * S * (-3))^(1 / L), M * exp(S * (-3)))
    sd2 <- ifelse(L > 0.01 | L < (-0.01), M * (1 + L * S * (-2))^(1 / L), M * exp(S * (-2)))
    z[idx] <- (-3 + (y - sd3) / (sd2 - sd3))[idx]
  }
  z
}
