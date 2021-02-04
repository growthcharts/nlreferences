data <- data.frame(len = c(56, 42, 53),
                   age = c(0.1, 0.2, 0.15),
                   sex = c("male", "female", "female"),
                   ga = c(40, 27, 39))

test_that("make_refcode() handles empty arguments", {
  expect_equal(make_refcode(), c("____"))
})

test_that("make_refcode() places sub argument at end", {
  expect_equal(make_refcode(sub = "hi"), c("____hi"))
})

test_that("make_refcode() works with vectors", {
  expect_equal(make_refcode("nl", "1997", "hgt", data$sex, data$ga),
               c("nl_1997_hgt_male_40", "nl_1997_hgt_female_27", "nl_1997_hgt_female_39"))
})

test_that("make_refcode() detects incompatible vector lengths", {
  expect_error(make_refcode(year = 1971:1974, sex = c("male", "female"), sub = "hi"))
})

test_that("make_refcode() accepts zero-length arguments", {
  expect_equal(make_refcode(year = character(0), sex = c("male", "female"), sub = NULL),
               c("___male_", "___female_"))
})

