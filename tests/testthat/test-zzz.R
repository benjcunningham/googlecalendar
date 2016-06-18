context("Cleanup")

# Setup ----------------------------------------------------------------

.cred$token <- readRDS("googlecalendar_token.rds")

file <- "testobjs.rds"
load(file)

# Regular testing ------------------------------------------------------

status <- gc_event_delete(event, verbose = FALSE)

test_that("Event deletion works", {
  expect_true(status)
})

status <- gc_delete(cal, verbose = FALSE)

test_that("Calendar deletion works", {
  expect_true(status)
})

file.remove(file)

test_that("Local copies have been trashed", {
  expect_false(file.exists(file))
})
