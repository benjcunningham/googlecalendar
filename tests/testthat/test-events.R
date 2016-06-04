context("Events and event lists work")

.cred$token <- readRDS("googlecalendar_token.rds")

cal <- gc_new("TestThat", verbose = FALSE)
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

status <- gc_event_delete(event, verbose = FALSE)
test_that("Event deletion works", {
  expect_true(status)
})

gc_delete(cal, verbose = FALSE)
