
df <- data.frame(hgt = 60, wgt = 5, hdc = 40, age = 0.3,
                 sex = "male", ga = c(20, 30, 40, 50))

test_that("produces tibble of Z-scores", {
  expect_equal(transform_z(df, ynames = c("hdc", "wfh")), structure(list(
    hdc_z = c(NA, 0.928, -1.268, -1.268), wfh_z = c(-1.662, -1.662,
                                                    -1.697, -1.697)),
    row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame")))
})
