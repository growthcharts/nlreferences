validate_distribution <- function(names, dist) {
  if (length(dist) != 1L) return(FALSE)
  switch(dist,
         NO = all(c("mean", "sd") %in% names),
         LMS = all(c("L", "M", "S") %in% names),
         BCCG = all(c("mu", "sigma", "nu") %in% names),
         BCPE = all(c("mu", "sigma", "nu", "tau") %in% names),
         MP = any(substr(names, 1L, 1L) == "p") && "mean" %in% names,
         MEA = !any(substr(names, 1L, 1L) == "p") && "mean" %in% names,
         PCT = any(substr(names, 1L, 1L) == "p"),
         ORD = any(substr(names, 1L, 3L) == "cat"),
         FALSE)
}
