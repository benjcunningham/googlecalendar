context("Calendars")

# Setup ----------------------------------------------------------------

.cred$token <- readRDS("googlecalendar_token.rds")

file <- "testobjs.rds"

# Ensure fresh environment ---------------------------------------------

ls <- dplyr::filter(gc_ls(), summary == "TestThat")

tmp <- purrr::`%||%`(
  unlist(lapply(ls$id, function(i) {
    gc_id(i, verbose = FALSE) %>%
      gc_delete(verbose = FALSE)
  })),
  TRUE
)

test_that("Environment is free of test calendars", {
  expect_equal(sum(!tmp), 0)
})

# Regular testing ------------------------------------------------------

test_that("Calendar lists are well-formed", {
  expect_is(gc_ls(), "googlecalendar_ls")
})

cal <- gc_new("TestThat", verbose = FALSE)

test_that("New calendars are well-formed", {
  expect_is(cal, "googlecalendar")
  expect_equal(cal$summary, "TestThat")
})

refetch <- gc_id(cal$id, verbose = FALSE)

test_that("The new calendar can be fetched by ID", {
  expect_is(refetch, "googlecalendar")
  expect_identical(refetch, cal)
})

refetch <- gc_summary(cal$summary, verbose = FALSE)

test_that("The new calendar can be fetched by summary", {
  expect_is(refetch, "googlecalendar")
  expect_identical(refetch, cal)
})

cal <- gc_edit(
  cal,
  description = "Enjoyable testing",
  timeZone = "America/Chicago",
  colorId = "20",
  verbose = FALSE
)

test_that("Calendar edits are reflected downstream", {
  expect_is(cal, "googlecalendar")
  expect_equal(cal$description, "Enjoyable testing")
  expect_equal(cal$timeZone, "America/Chicago")
  expect_equal(cal$colorId, "20")
})

# Saving ---------------------------------------------------------------

save(cal, file = file)
