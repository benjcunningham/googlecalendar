context("Calendars and calendar lists work")

.cred$token <- readRDS("googlecalendar_token.rds")

test_that("Calendar lists are well-formed", {
  expect_is(gc_ls(), "googlecalendar_ls")
})

cal <- gc_new("TestThat", verbose = FALSE)
test_that("New calendars are well-formed", {
  expect_is(cal, "googlecalendar")
})

refetch <- gc_id(cal$id, verbose = FALSE)
test_that("The new calendar can be fetched", {
  expect_identical(refetch, cal)
})

status <- gc_delete(cal)
test_that("Calendar deletion works", {
  expect_true(status)
})
