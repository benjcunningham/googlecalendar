context("Events")

# Setup ----------------------------------------------------------------

.cred$token <- readRDS("googlecalendar_token.rds")

file <- "testobjs.rds"
load(file)

# Regular testing ------------------------------------------------------

event <- suppressMessages(gc_event_new(
  cal,
  start = list(dateTime = "2016-09-21T17:00:00Z"),
  end = list(dateTime = "2016-09-21T19:00:00Z"),
  summary = "Birthday Dinner"
))

test_that("New events are well-formed", {
  expect_is(event, "event")
})

test_that("Event lists are well-formed", {
  expect_is(gc_event_ls(cal), "event_ls")
})

test_that("Events can be retrieved by ID", {

  refetch <- gc_event_id(cal, event$id)

  expect_is(refetch, "event")
  expect_identical(refetch, event)

})

test_that("Events can be retrieved by query", {

  refetch <- gc_event_query(cal, q = "Birthday Dinner")

  expect_is(refetch, "event")
  expect_identical(refetch, event)

})

event <- suppressMessages(gc_event_edit(
  event,
  start = list(dateTime = "2016-09-21T16:00:00-05:00"),
  end = list(dateTime = "2016-09-21T18:00:00-05:00"),
  summary = "Early Birthday Dinner",
  verbose = FALSE
))

test_that("Event edits are reflected downstream", {
  expect_is(event, "event")
  expect_equal(event$start$dateTime, "2016-09-21T16:00:00-05:00")
  expect_equal(event$end$dateTime, "2016-09-21T18:00:00-05:00")
  expect_equal(event$summary, "Early Birthday Dinner")
})

test_that("CSV files import without warnings", {

  csv <- read.csv("event_import.csv", stringsAsFactors = FALSE)
  imports <- gc_event_import(cal, csv)

  expect_is(imports, "character")
  expect_length(imports, nrow(csv))
  expect_equal(sum(is.na(imports)), 0)

})

# Saving ---------------------------------------------------------------

save(cal, event, file = file)
