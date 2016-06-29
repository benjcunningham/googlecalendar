#' Retrieve a calendar event
#'
#' Retrieves metadata for a Google Calendar event in the form of an
#' \code{event} object. This method requests information made available
#' through the Google Calendar API Events resource.
#'
#' For \code{gc_event_query}, an arbitrary number of named arguments
#' may be supplied via the \dots parameter. These values are passed as
#' part of the HTTP request. A list of available optional query
#' parameters can be found in
#' \href{https://developers.google.com/google-apps/calendar/v3/reference/events/list#request}{Events: List}.
#'
#' Notable parameters include:
#'
#' \itemize{
#'   \item{\code{q} -- Free text search terms to find in any field.}
#'   \item{\code{singleEvents} -- Logical indicating whether to expand
#'     recurring events into instances and only return single one-off
#'     events.}
#'   \item{\code{timeMax} -- Exclusive upper bound of the start time
#'     filter (formatted as an RFC3339 timestamp).}
#'   \item{\code{timeMin} -- Inclusive lower bound of the end time
#'     filter (formatted as an RFC3339 timestamp).}
#' }
#'
#' For more information on the structure of an \code{event} object, see
#' the Google Calendar API
#' \href{https://developers.google.com/google-apps/calendar/v3/reference/events}{Events Resource Overview}.
#'
#' @name gc_event
#'
#' @param x \code{googlecalendar} object representing the calendar for
#'   which to list events.
#' @param id Event ID as a character string.
#' @param \dots Additional parameters to be passed as part of the HTTP
#'   request. More information on these named arguments are available
#'   below.
#' @template verbose
#'
#' @return Event metadata as an \code{event} object (a custom class
#'   wrapping a named list).
#'
#' @examples
#' \dontrun{
#' gc_summary("Commitments") %>%
#'   gc_event_id("lohlv4duqhqu8bh6kfok9ookmk")
#'
#' gc_summary("Commitments") %>%
#'   gc_event_query(cal, q = "Lunch with Will")
#' }
#'
#' @export
gc_event_query <- function(x, ..., verbose = TRUE) {

  stopifnot(methods::is(x, "googlecalendar"))

  ls <- gc_event_ls(x, ..., verbose = FALSE)

  if (is.null(ls) || nrow(ls) != 1) {
    if (verbose) {
      matches <- ifelse(!is.null(ls), nrow(ls), 0)
      sprintf(paste("Found %s events matching the query.",
                    "Must match exactly one."),
              matches) %>%
        message()
    }
    return(invisible(NULL))
  }

  gc_event_id(x, ls$id, verbose = verbose)

}

#' @rdname gc_event
#'
#' @export
gc_event_id <- function(x, id, verbose = TRUE) {

  stopifnot(methods::is(x, "googlecalendar"))

  gc_event_lookup(x$id, id, verbose = verbose)

}

#' @keywords internal
gc_event_lookup <- function(cid, id, verbose) {

  path <- file.path("calendars", cid, "events", id)
  resp <- GET_resource(path)

  event <- json_content(resp, flatten = TRUE)
  event$cid <- cid

  as.event(event)

}
