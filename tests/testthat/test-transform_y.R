
df1 <- data.frame(hgt_z = c(NA, 1.819, -1.183, -1.183),
                  wgt_z = c(NA, 0.627, -2.131, -2.131),
                  hdc_z = c(NA, 0.928, -1.268, -1.268),
                  wfh_z = c(-1.662, -1.662, -1.697, -1.697),
                  age = 0.3,
                  sex = "male",
                  ga = c(20, 30, 40, 50))
test_that("Produces inverse transformation z-->y, no hgt column", {
  expect_equal(transform_y(df1), structure(list(hgt = c(NA, 60.001,
  60.001, 60.001), wgt = c(NA, 5, 5, 5), hdc = c(NA, 40, 40, 40),
  wfh = c(NA, 5, 5, 5)), row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame")))
})

df2 <- cbind(df1, hgt = 60)
test_that("Uses hgt column to detemine WFH inverse transformation z-->y", {
  expect_equal(transform_y(df2), structure(list(hgt = c(NA, 60.001,
  60.001, 60.001), wgt = c(NA, 5, 5, 5), hdc = c(NA, 40, 40, 40),
  wfh = c(5, 5, 5, 5)), row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame")))
})

