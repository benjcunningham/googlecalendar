#' Retrieve a calendar
#'
#' Retrieves metadata for a Google Calendar in the form of a
#' \code{googlecalendar} object. This method filters information made
#' available through the Google Calendar API CalendarList
#' resource.
#'
#' While the Google Calendar API prefers to return sparse structures,
#' \code{googlecalendar} objects attempt to faithfully represent all
#' documented fields in the CalendarList resource. In practice, this
#' means that values unreported by the service are reported as \code{NA}
#' or, if applicable, their documented default values.
#'
#' For more information on the structure of a \code{googlecalendar}
#' object, see the Google Calendar API
#' \href{https://developers.google.com/google-apps/calendar/v3/reference/calendarList}{CalendarList Resource Overview}.
#'
#' @name gc_calendar
#'
#' @param x Calendar-identifying information. For \code{gc_id}, a
#'   calendar ID as a character string. For \code{gc_summary}, a regular
#'   expression to be matched to the calendar title field,
#'   \code{summary}.
#' @param fixed Logical indicating whether \code{x} should be matched
#'   literally.
#' @param \dots Optional arguments to be passed to \code{\link[base]{grep}}.
#' @template verbose
#'
#' @return Calendar metadata as a \code{googlecalendar} object (a custom
#'   class wrapping a named list).
#'
#' @examples
#' \dontrun{
#' gc_id("kpoji8n345cid1eu6o0mlcbs30@group.calendar.google.com")
#'
#' gc_summary("ments$")
#' gc_summary("commitments", ignore.case = TRUE)
#' gc_summary("Commitments", fixed = TRUE)
#' }
#'
#' @export
gc_id <- function(x, verbose = TRUE) {

  gc_lookup(x, "id", fixed = TRUE, verbose = verbose)

}

#' @rdname gc_calendar
#'
#' @export
gc_summary <- function(x, fixed = FALSE, ..., verbose = TRUE) {

  gc_lookup(x, "summary", fixed = fixed, ..., verbose = verbose)

}

#' @keywords internal
gc_lookup <- function(x, lvar, fixed = FALSE, ..., verbose = TRUE) {

  stopifnot(length(x) == 1, is.character(x))

  resp <- GET_resource("users/me/calendarList", fields = "items")

  cals <- json_content(resp, flatten = TRUE)$items

  i <- grep(x, cals[[lvar]], fixed = fixed, ...)

  if (length(i) != 1) {
    if (verbose) {
      sprintf(paste0("Found %s calendars where `%s` matches \"%s\".\n",
                     "Must match exactly one."),
              length(i), lvar, x) %>%
        message()
    }
    return(invisible(NULL))
  }

  if (verbose) {
    sprintf("Successfully identified: \"%s\"", cals$summary[i]) %>%
      message()
  }

  as.googlecalendar(cals[i, ])

}
