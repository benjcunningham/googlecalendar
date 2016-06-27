context("Calendars")

# Setup ----------------------------------------------------------------

.cred$token <- readRDS("googlecalendar_token.rds")

file <- "testobjs.rds"

# Ensure fresh environment ---------------------------------------------

ls <- suppressMessages(dplyr::filter(gc_ls(), summary == "TestThat"))

tmp <- suppressMessages(purrr::`%||%`(
  unlist(lapply(ls$id, function(i) gc_delete(gc_id(i)))), TRUE
))

test_that("Environment is free of test calendars", {
  expect_equal(sum(!tmp), 0)
})

# Regular testing ------------------------------------------------------

test_that("Calendar lists are well-formed", {
  expect_is(gc_ls(), "googlecalendar_ls")
})

cal <- suppressMessages(gc_new("TestThat"))

test_that("New calendars are well-formed", {
  expect_is(cal, "googlecalendar")
  expect_equal(cal$summary, "TestThat")
})

test_that("The new calendar can be fetched by ID", {

  refetch <- gc_id(cal$id, verbose = FALSE)

  expect_is(refetch, "googlecalendar")
  expect_identical(refetch, cal)

})

test_that("The new calendar can be fetched by summary", {

  refetch <- gc_summary(cal$summary, verbose = FALSE)

  expect_is(refetch, "googlecalendar")
  expect_identical(refetch, cal)

})

cal <- suppressMessages(gc_edit(
  cal,
  description = "Enjoyable testing",
  timeZone = "America/Chicago",
  colorId = "20"
))

test_that("Calendar edits are reflected downstream", {
  expect_is(cal, "googlecalendar")
  expect_equal(cal$description, "Enjoyable testing")
  expect_equal(cal$timeZone, "America/Chicago")
  expect_equal(cal$colorId, "20")
})

# Saving ---------------------------------------------------------------

save(cal, file = file)
