# dscore, gsed lexicon
data <- data.frame(
  age = rep(round(21 / 365.25, 4), 10),
  ddifmd001 = c(NA, NA, 0, 0, 0, 1, 0, 1, 1, 1),
  ddicmm029 = c(NA, NA, NA, 0, 1, 0, 1, 0, 1, 1),
  ddigmd053 = c(NA, 0, 0, 1, 0, 0, 1, 1, 0, 1)
)

# z <- dscore::dscore(data)
# a n   p     d  sem    daz
# 1      NA 0  NA    NA   NA     NA
# 2      NA 0  NA    NA   NA     NA
# 3  0.0575 1 0.0  6.61 2.76 -2.019
# 4  0.0575 2 0.0  5.60 2.46 -2.235
# 5  0.0575 2 0.5  9.09 1.70 -1.447
# 6  0.0575 2 0.5  9.09 1.70 -1.447
# 7  0.0575 2 0.5  9.09 1.70 -1.447
# 8  0.0575 2 0.5  9.09 1.70 -1.447
# 9  0.0575 2 1.0 15.30 3.85  0.277
# 10 0.0575 2 1.0 15.30 3.85  0.277

expected_d <- c(NA, NA, 6.61, 5.60, 9.09, 9.09, 9.09, 9.09, 15.30, 15.30)
expected_daz <- c(NA, NA, -2.019, -2.235, -1.447, -1.447, -1.447, -1.447, 0.277, 0.277)

df <- data.frame(age = data$age,
                 dsc = expected_d,
                 sex = "male",
                 ga = 40)
res <- nlreferences::transform2z(df, ynames = "dsc", verbose = TRUE)
daz <- dplyr::pull(res)

test_that("produces correct DAZ-scores", {
  expect_identical(expected_daz, daz)
})

# default key: "gsed" (currently points to "gsed2212", population "phase1")
expected_d <- c(NA, NA, 6.61, 5.60, 9.09, 9.09, 9.09, 9.09, 15.30, 15.30)
expected_daz <- c(NA, NA, -2.019, -2.235, -1.447, -1.447, -1.447, -1.447, 0.277, 0.277)
# test_that("produces expected D-scores - key gsed", {
#   expect_identical(z$d, expected_d)
#   expect_identical(z$daz, expected_daz)
# })

