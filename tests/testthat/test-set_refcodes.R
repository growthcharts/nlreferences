df <- data.frame(xname = "age", yname = "hgt",
                 x = c(0, 0.2, 0.5), sex = "female",
                 age = NA, ga = 32)

test_that("produces correct result with age = NA", {
  expect_equal(set_refcodes(df), c("nl_2012_hgt_female_32", "nl_2012_hgt_female_32",
                                   "nl_2012_hgt_female_32"))
})
