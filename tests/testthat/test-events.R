context("Events")

# Setup ----------------------------------------------------------------

.cred$token <- readRDS("googlecalendar_token.rds")

file <- "testobjs.rds"
load(file)

# Regular testing ------------------------------------------------------

event <- gc_event_new(
  cal,
  start = list(dateTime = "2016-09-21T17:00:00Z"),
  end = list(dateTime = "2016-09-21T19:00:00Z"),
  summary = "Birthday Dinner",
  verbose = FALSE
)

test_that("New events are well-formed", {
  expect_is(event, "event")
})

test_that("Event lists are well-formed", {
  expect_is(gc_event_ls(cal, verbose = FALSE), "event_ls")
})

refetch <- gc_event_id(cal, event$id, verbose = FALSE)

test_that("Events can be retrieved by ID", {
  expect_is(refetch, "event")
  expect_identical(refetch, event)
})

refetch <- gc_event_query(cal, q = "Birthday Dinner", verbose = FALSE)

test_that("Events can be retrieved by query", {
  expect_is(refetch, "event")
  expect_identical(refetch, event)
})

event <- gc_event_edit(
  event,
  start = list(dateTime = "2016-09-21T16:00:00-05:00"),
  end = list(dateTime = "2016-09-21T18:00:00-05:00"),
  summary = "Early Birthday Dinner",
  verbose = FALSE
)

test_that("Event edits are reflected downstream", {
  expect_is(event, "event")
  expect_equal(event$start$dateTime, "2016-09-21T16:00:00-05:00")
  expect_equal(event$end$dateTime, "2016-09-21T18:00:00-05:00")
  expect_equal(event$summary, "Early Birthday Dinner")
})

# Saving ---------------------------------------------------------------

save(cal, event, file = file)
