#' Retrieve a Google Calendar
#'
#' Retrieves metadata for a Google Calendar in the form of a
#' \code{googlecalendar} object. This function filters information made
#' available through the Google Calendar API \code{CalendarList}
#' resource. Use requires authorization.
#'
#' For more information on the structure of the \code{CalendarList}
#' resource (and therefore the expected contents of a
#' \code{googlecalendar} object), see the Google Calendar API
#' \href{https://developers.google.com/google-apps/calendar/v3/reference/calendarList}{CalendarList
#' Overview}.
#'
#' @name googlecalendars
#'
#' @param x Calendar-identifying information. In the case of
#'   \code{gc_id}, a character string representing a calendar ID to
#'   which the user has read access. In the case of \code{gc_summary}, a
#'   character string containing a regular expression (or exact
#'   character string for \code{fixed = TRUE}) to be matched to the
#'   calendar title field, \code{summary}.
#' @param fixed Logical indicating if \code{x} should be matched
#'   literally.
#' @param ... Optional arguments to be passed to \code{\link{grep}}.
#' @template verbose
#'
#' @return A \code{googlecalendar} object (a custom class wrapping a
#'   named list representing metadata for a single calendar.
#'
#' @examples
#' \dontrun{
#' gc_id("<26-CHAR-ID>@group.calendar.google.com")
#'
#' gc_summary("ments$")
#' gc_summary("commitments", ignore.case = TRUE)
#' gc_summary("Commitments", fixed = TRUE)
#' }
#'
#' @export
gc_id <- function(x, verbose = TRUE) {

  stopifnot(length(x) == 1, is.character(x))
  gc_lookup(x, "id", fixed = TRUE, verbose = verbose)

}

#' @rdname googlecalendars
#' @export
gc_summary <- function(x, fixed = FALSE, ..., verbose = TRUE) {

  stopifnot(length(x) == 1, is.character(x))
  gc_lookup(x, "summary", fixed = fixed, ..., verbose = verbose)

}

#' @keywords internal
gc_lookup <- function(x, lvar, fixed = FALSE, ..., verbose = TRUE) {

  url <-
    file.path(.cred$base_url_v3, "users", "me", "calendarList") %>%
    httr::modify_url(query = list(
      fields = "items",
      key = getOption("googlecalendar.client_key")
    ))

  resp <-
    httr::GET(url, gc_token()) %>%
    httr::stop_for_status()

  cals <- json_content(resp, flatten = TRUE)$items

  i <- grep(x, cals[[lvar]], fixed = fixed, ...)

  if (length(i) != 1) {
    sprintf(paste("Found %s calendars where `%s` matches",
                  "\"%s\".\nMust match exactly one calendar."),
            length(i), lvar, x) %>%
      stop(call. = FALSE)
  }

  if (verbose) {
    sprintf("Successfully identified: \"%s\"", cals$summary[i]) %>%
      message()
  }

  as.googlecalendar(cals[i, ])

}
