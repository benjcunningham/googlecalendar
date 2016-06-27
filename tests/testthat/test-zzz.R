context("Cleanup")

# Setup ----------------------------------------------------------------

.cred$token <- readRDS("googlecalendar_token.rds")

file <- "testobjs.rds"
load(file)

# Regular testing ------------------------------------------------------

test_that("Event deletion works", {
  expect_true(gc_event_delete(event))
})

test_that("Calendar deletion works", {
  expect_true(gc_delete(cal))
})

test_that("Local copies have been trashed", {
  file.remove(file)
  expect_false(file.exists(file))
})
