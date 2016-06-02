context("Random utility functions work")

test_that("Itemized fields are properly pasted together", {
  expect_equal(itemize_fields(c("foo", "bar", "baz")),
               "items(foo,bar,baz)")
})

test_that("NA default-handling infix works", {
  expect_identical(1 %NA% 2, 1)
  expect_identical(NA %NA% 2, 2)
  expect_warning(NULL %NA% 2)
})
