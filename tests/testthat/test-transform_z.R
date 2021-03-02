
df <- data.frame(hgt = 60, wgt = 5, hdc = 40, age = 0.3,
                 sex = "male", ga = c(20, 30, 40, 50))

test_that("produces tibble of Z-scores", {
  expect_equal(transform2z(df, ynames = c("hdc", "wfh")), structure(list(
    hdc_z = c(NA, 0.928, -1.268, -1.268), wfh_z = c(-1.662, -1.662,
                                                    -1.697, -1.697)),
    row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame")))
})

df2 <- df[, -c(1, 3)]
test_that("errors if no `hgt` is found because we wish wfh implicitly", {
  expect_error(transform2z(df2), "Required variable `hgt` not found.")})

test_that("same case, but now we don't wish wfh", {
  expect_equal(transform2z(df2, ynames = "wgt"), structure(list(
    wgt_z = c(NA, 0.627, -2.131, -2.131)), row.names = c(NA,
                                                         -4L), class = c("tbl_df", "tbl", "data.frame")))
  })

